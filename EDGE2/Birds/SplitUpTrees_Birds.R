# This script reads in the 100 trees file, and then splits them into seperate trees
# Will be helpful for HPC 

# housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/EDGE2/Birds/")

# load required packages

library(phylobase)
library(phytools)

# read in the 100 trees

load("1000_full_bird_trees_23-04-20.RData")

# now save them down seperately 
for (i in 1:length(phy.block.1000)) {
  tree <- phy.block.1000[[i]]
  name <- paste("BirdTree/BirdTree_", i, ".Rdata", sep = "")
  save(tree, file = name)
}
  
  
save(Species, file = "BirdSpecies.Rdata")

