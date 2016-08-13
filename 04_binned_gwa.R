###########################################################################
# (binned) GWA of brain size
###########################################################################

###########################################################################
# libraries
###########################################################################

library("dplyr")
library("ggplot2")
library("tidyr")
library("cowplot")
library("visreg")
library("lme4")
library("SNPRelate")

###########################################################################
# load raw genotype data
###########################################################################

br_geno <- read.csv(file = "data/brain_genos.csv", h = T)

###########################################################################
# prepare snprelate file (for pruning)
###########################################################################

# convery to 012

num_br_geno <- list()

for (i in 1:nrow(br_geno)){
  
  if (i %% 1000 == 0){
    print(i/nrow(br_geno))
  }
  
#for (i in 1:10){
  
  genos <- br_geno[i,-c(1:3)] %>% unlist %>% as.character
  
  alleles <- genos %>%
    strsplit(split = "") %>%
    unlist %>%
    unique %>%
    grep(pattern = "[^N]", value = TRUE)
  
  key <- c(paste0(alleles[1], alleles[1]), paste0(alleles[1], alleles[2]), paste0(alleles[2], alleles[2]), "NN")
  remap <- c(0, 1, 2, 9)
  
  map <-  setNames(remap, key)
  
  num_geno <- t(map[unlist(genos)])
  
  if(length(alleles) == 0){
    alleles <- c("N", "N")
  }
  
  num_br_geno[[i]] <- data.frame(br_geno[i, c(1:3)], allele_1 = alleles[1], allele_2 = alleles[2],  num_geno)
  
}

geno_mat <- rbind_all(num_br_geno)

# keep chr and pos info
chr <- br_geno$CHROM
pos <- br_geno$POS

# extract genotypes into matrix
sample_id <- colnames(br_geno[,-c(1:3)])
snp_id <- br_geno$Combined %>% as.character

geno_mat <- data.matrix(t(br_geno[,-c(1:3)]))

snpgdsCreateGeno("data/brain_geno_snprelate.gds", genmat = geno_mat, 
                 sample.id = sample_id, snpfirstdim = FALSE, 
                 snp.id = snp_id, snp.chromosome = chr, snp.position = pos)

# reclaim memory
rm(list=c("pos_df", "geno_matrix"))


################################################################################
# create/open gen object 
################################################################################

#snpgdsCreateGeno("data/snp_relate/whtstbk_all_pruned.gds", genmat = geno_matrix, 
#                 sample.id = sample_id, snpfirstdim = FALSE, 
#                 snp.id = snp_id, snp.chromosome = pos_df$chr, snp.position = pos_df$pos)
# reclaim memory
rm(list=c("pos_df", "geno_matrix"))

br_geno <- snpgdsOpen("data/brain_geno_snprelate.gds", allow.duplicate = TRUE)



http://mouse.cs.ucla.edu/emma/index.html