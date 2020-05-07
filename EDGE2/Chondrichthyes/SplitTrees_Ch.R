# This script reads in the 100 trees file, and then splits them into seperate trees
# Will be helpful for HPC 

# housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/EDGE2/Chondrichthyes/")

# load required packages

library(phylobase)
library(phytools)

# read in the 100 trees

load("1000_Chond_phylos.Rdata")

# now save them down seperately 
for (i in 1:length(phy.block.1000)) {
  tree <- phy.block.1000[[i]]
  name <- paste("UpdatedTrees/ChtreeUpdated_", i, ".Rdata", sep = "")
  save(tree, file = name)
}



