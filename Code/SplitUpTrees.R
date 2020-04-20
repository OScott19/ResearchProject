# This script reads in the 100 trees file, and then splits them into seperate trees
# Will be helpful for HPC 

# housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code")

# load required packages

library(phylobase)
library(phytools)

# read in the 100 trees

all.trees <- read.tree("../Data/actinopt_full.trees")

### we are also going to take this opportunity to remove all of the extinct species


######## load in the data that we need

load("../Data/Extinct_species.Rdata")



########## fire up each tree in turn and remove the extinct species
# save down in new list

updated.trees <- list()


for (i in 1:length(all.trees)) {
  tree.use <- all.trees[[i]]
  nodes <- which(tree.use$tip.label %in% extinct$species)
  new.tree <- drop.tip(tree.use, tree.use$tip.label[nodes]) 
  updated.trees[[i]] <- new.tree 
  print(paste("Tree", i, "transformed", sep = " "))
  }

save(updated.trees, file = "../Data/100Act_trees_transformed.Rdata")

### now save down the transformed trees

for (i in 1:length(updated.trees)) {
  tree <- updated.trees[[i]]
  name <- paste("../Data/UpdatedTrees/Act_tree_updated_", i, ".Rdata", sep = "")
  save(tree, file = name)
}
  
  


