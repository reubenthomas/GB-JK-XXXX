rm(list = ls())

require(dplyr)
require(magrittr)
require(tibble)
require(broom)

sourcedir <- "~/Dropbox (Gladstone)/BC-ZY-1231/"
data_file <- "All_16_Samples_Normalized_Count.csv"


##arbitary select log2FC threshold for genes to be up or down regulated
log2_thresh <- log2(1)

data <- paste0(sourcedir, data_file) %>%
  read.csv(., header = TRUE, skip = 1) %>%
  filter(X != "" & X.4 == "Endogenous") %>%
  select(-2)

# ##arbitary select threshold for genes to be present in at least alpha percent of the samples
# alpha <- 0.25
# data %<>% filter(X.6 >= 25)

annot_data <- data %>%
  select(1:6)

norm_data <- data %>%
  select(-(1:6)) %>%
  log2()

row.names(norm_data) <- as.character(data$X)

genotype <- norm_data %>%
  colnames() %>%
  sapply(., function(x) strsplit(x, "[.]")[[1]][4])

virus <- norm_data %>%
  colnames() %>%
  sapply(., function(x) strsplit(x, "[.]")[[1]][1])

pheno_data <- data.frame(genotype, virus) %>%
  mutate(genotype = relevel(genotype, ref = "WT")) %>%
  mutate(virus = relevel(virus, ref = "Bald"))


##Comparison 1: WT - Spike vs Bald
data_comp_1 <- norm_data %>%
  select(which((genotype=="WT") & (virus == "Spike" | virus == "Bald"))) 

data_comp_1 <- data_comp_1[rowSums(data_comp_1 == log2(20)) <= 4, ]
pheno_comp_1 <- pheno_data %>%
  filter((genotype=="WT") & (virus == "Spike" | virus == "Bald"))

##up regulation
res = apply(data_comp_1,1,function(i)tidy(t.test(i ~ pheno_comp_1$virus, alternative="less", mu = -log2_thresh)))
res = do.call(rbind,res)
res$adjP = p.adjust(res$p.value,"BH")

res %<>% mutate(gene=row.names(data_comp_1))

res %<>%
  select(gene, estimate, p.value, adjP, method) %>%
  rename(log2FC=estimate) %>%
  mutate(log2FC_thresh=rep(log2_thresh, nrow(.)))

res %>%
  write.csv(., paste0(sourcedir, "WT_Spike_vs_Bald_up_regulation.csv"), row.names = FALSE)

##down regulation
res = apply(data_comp_1,1,function(i)tidy(t.test(i ~ pheno_comp_1$virus, alternative="greater", mu = log2_thresh)))
res = do.call(rbind,res)
res$adjP = p.adjust(res$p.value,"BH")

res %<>% mutate(gene=row.names(data_comp_1))

res %<>%
  select(gene, estimate, p.value, adjP, method) %>%
  rename(log2FC=estimate) %>%
  mutate(log2FC_thresh=rep(log2_thresh, nrow(.)))

res %>%
  write.csv(., paste0(sourcedir, "WT_Spike_vs_Bald_down_regulation.csv"), row.names = FALSE)


##Comparison 2: WT - Spike vs Bald
data_comp_2 <- norm_data %>%
  select(which((virus=="Spike") & (genotype == "WT" | genotype == "FIbmac"))) 

data_comp_2 <- data_comp_2[rowSums(data_comp_2 == log2(20)) <= 4, ]
pheno_comp_2 <- pheno_data %>%
  filter((virus=="Spike") & (genotype == "WT" | genotype == "FIbmac"))

##up regulation
res = apply(data_comp_2,1,function(i)tidy(t.test(i ~ pheno_comp_2$genotype, alternative="less", mu = -log2_thresh)))
res = do.call(rbind,res)
res$adjP = p.adjust(res$p.value,"BH")

res %<>% mutate(gene=row.names(data_comp_2))

res %<>%
  select(gene, estimate, p.value, adjP, method) %>%
  rename(log2FC=estimate) %>%
  mutate(log2FC_thresh=rep(log2_thresh, nrow(.))) 

res %>%
  write.csv(., paste0(sourcedir, "Spike_FIbmac_vs_WT_up_regulation.csv"), row.names = FALSE)

##down regulation
res = apply(data_comp_2,1,function(i)tidy(t.test(i ~ pheno_comp_2$genotype, alternative="greater", mu = log2_thresh)))
res = do.call(rbind,res)
res$adjP = p.adjust(res$p.value,"BH")

res %<>% mutate(gene=row.names(data_comp_2))

res %<>%
  select(gene, estimate, p.value, adjP, method) %>%
  rename(log2FC=estimate) %>%
  mutate(log2FC_thresh=rep(log2_thresh, nrow(.))) 

res %>%
  write.csv(., paste0(sourcedir, "Spike_FIbmac_vs_WT_down_regulation.csv"), row.names = FALSE)

  
