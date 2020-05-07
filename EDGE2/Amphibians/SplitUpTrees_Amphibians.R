# This script reads in the 100 trees file, and then splits them into seperate trees
# Will be helpful for HPC 

# housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/EDGE2/Amphibians/")

# load required packages

library(phylobase)
library(phytools)

# read in the 100 trees

load("500_full_phylos_amphibians_501-1000_resave.RData")

# now save them down seperately 
for (i in 1:length(phy.block.501.1000)) {
  tree <- phy.block.501.1000[[i]]
  tn <- i + 500
  name <- paste("AmTrees/AmTree_", tn, ".Rdata", sep = "")
  save(tree, file = name)
}
  
  
save(Species, file = "AmSpecies.Rdata")



