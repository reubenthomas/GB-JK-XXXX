#### CONTROL PARAMETERS

PATH <- "~/Dropbox/Bioinformatics/Users/Cliff (1)/CliffsProjects/bc-jk-564-cliffab-Julia_NonpropHaz"
# This should be the path to where your data and the CoxeME_utilities.R file is 

BASELINE_CONSTRUCT <- 'HD53n2+ISX9'
# This is the name of the construct that all other constructs will be compared against

GET_ALL_FILES <- TRUE
# Should we process all files in this folder?

INDIVIDUAL_FILES <- c('exampleFile1.csv', 'exampleFile2.csv')
# If GET_ALL_FILES <- FALSE, will just process these files instead
# if GET_ALL_FILES <- TRUE, this is ignored


INCLUDE_CIs <- FALSE
# Should we include confidence intervals for our Kaplan Meier/Cumulateive Hazard curves?
# If we compare across lots of experiments/constructs, the CI's can make plots *really* cluttered

PLOT_TYPE <- "CH"
# What type of plots should be used to compare across experiments 
# in the same construct?
# Options are "CH" = cumulative hazards or "KM" = Kaplan Meier

#### RUNNING ANALYSIS
library(coxme)
setwd(PATH)
source("CoxME_utilities.R")
fileNames <- getDataFileNames(getAllFiles = GET_ALL_FILES, fileList = INDIVIDUAL_FILES)
all_data <- getAllData(fileNames, baseline = BASELINE_CONSTRUCT)
all_fits <- makeAllFits(all_data)
makeResults(all_fits, includeCIs = INCLUDE_CIs, plotType = PLOT_TYPE)
