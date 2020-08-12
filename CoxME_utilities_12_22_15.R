getDataFileNames <- function(getAllFiles = TRUE, fileList){
  if(getAllFiles){ 
    allFileNames <- list.files()
    fileList <- allFileNames[grepl('.csv', allFileNames)]
  }
  badFiles <- !grepl('.csv', fileList)
  if(any(badFiles)){
    stop(c("Included non .csv files to be analyzed, which is a problem. Here's the bad file names: ", 
           fileList[badFiles]) )
  }
  if(length(fileList) == 0) stop("No files found for processing")
  return(fileList)
}


getAllData <- function(fileNames, baseline){
  necessaryVarNames <- c('Image', 'Neuron', 'Time', 'Censored', 'Construct','Experiment')
  data <- read.csv(fileNames[1], stringsAsFactors = FALSE)
  checkVarNames(data, necessaryVarNames, fileNames[1])
  data <- data[,necessaryVarNames]
  data[['fileName']] <- fileNames[1]
  
  if(length(fileNames) > 1){
    for(i in 2:length(fileNames)){
      thisFileName <- fileNames[i]
      thisData <- read.csv(thisFileName, stringsAsFactors = FALSE)
      checkVarNames(thisData, necessaryVarNames, thisFileName)
      thisData <- thisData[,necessaryVarNames]
      thisData[['fileName']] <- thisFileName
      data <- rbind(data, thisData)
    }
  }
  if(!(baseline %in% data$Construct)) 
    stop('baseline value not found in data. Check that control parameter BASELINE_CONSTRUCT has been properly defined')
  data$Construct <- as.factor(data$Construct)
  data$Construct <- relevel(data$Construct, baseline)
  return(data)
}

checkVarNames <- function(data, necessaryVarNames, file){
  varNames <- names(data)
  if(!all(necessaryVarNames %in% varNames)){
    missingVarInds  <- which(!necessaryVarNames %in% varNames)
    errorMess <- c("missing variable expected by script. Could not find variable(s)\n",
                   paste(necessaryVarNames[missingVarInds]) , 
                   '\nin file ', file)
    stop(errorMess)
  }
}


makeAllFits <- function(data){
  fullFit <- coxme(Surv(Time, Censored) ~ Construct + (1 | Experiment) +  (1 | Image), data = data )
  km_fits <- list()
  contructs <- levels(data$Construct)
  for(this_con in contructs){
    thisFit <- survfit(Surv(Time, Censored) ~ Experiment, data = data, subset = data$Construct == this_con)
    km_fits[[this_con]] <- thisFit
  }
  return(list(fullFit = fullFit, km_fits = km_fits) ) 
}

makeResults <- function(fits, includeCIs = FALSE, includeLegend = TRUE, plotType = 'KM'){

  if(!plotType %in% c('KM', 'CH')) stop('plotType not recognized. Options = "KM" or "CH"')
  currentDirs <- list.dirs()
  if(! any(grepl('ScriptResults', currentDirs))) dir.create('ScriptResults')

  setwd('ScriptResults')
  
  fullFitText <- capture.output(fits[[1]])
  cat(fullFitText, file = 'fullMEfit.txt', sep = '\n', append = FALSE)  

  expNames <- names(fits[[2]])
  for(i in seq_along(expNames) ){
    thisExpName <- expNames[i]
    pdf(file = paste0(thisExpName, '.pdf') )
    k <- length(fits[[2]][[i]][['strata']])
    
    if(plotType == "KM"){
    plot(fits[[2]][[i]], col = 1:k, conf.int = includeCIs)
    lgdLocation <- 'bottomleft'
    }
    else{
      plot(fits[[2]][[i]], col = 1:k, conf.int = includeCIs, fun = 'cumhaz')
      lgdLocation <- 'topleft'
    }
    if(includeLegend){
      grps <- names(fits[[2]][[i]][['strata']])
      if(length(grps) > 1){
        grps <- gsub('Experiment=', '', grps)
        legend(lgdLocation, legend = grps, lty = 1, col = 1:k)
      }
    }
    dev.off()
  }
  setwd('..')
}