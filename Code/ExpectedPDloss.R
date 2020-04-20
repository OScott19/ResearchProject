# this script is going to calculate the expected PD loss for the entire actinopterygii tree
# we are going to use 100 trees and see what happens
# THIS SCRIPT: BEFORE I HAD THE SCRIPT TO ADD IN MISSING FEATURES

# housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

###

# To calculated expected PD loss:
# 1) sum length of every branch in tree, which is the total PD of clade
# 2) multiply each branch by the product of the probabilities of extinction of each descentant species
# 3) Sum the branch lengths of the transformed tree to get the expected PD loss is all of the pexts are true
# 4) Subtract from the original tree to get the expected pd saved into the future


#######################
start.time <- proc.time()[3]

print("Starting the script")

# We are going to do 1-4 above with one three, then modify the script for HPC to run for all 100


# Let's read in that tree!

load("../Data/one_tree.Rdata")

# 1: sum the length of every branch in the tree

totalPD <- sum(one.tree$edge.length) # 321,612 MY of evolutionary history


## read in data

tree <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)
red <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)

## create species/ PExt

pext <- data.frame(matrix(ncol = 3, nrow = length(tree$superclass)))
colnames(pext) <- c("Species", "category", "pext")

pext$category <- NA
pext$scientifcName <- tree$genus.species

for (x in 1:length(pext$scientifcName)) {
  ref <- match(pext$scientifcName[x], red$scientificName)
  if (length(ref) != 0) {
    pext$category[x] <- red$redlistCategory[ref]
  } 
  }

pext$pext <- NA

# add pext scores

scores <- data.frame(matrix(ncol = 2, nrow = length(unique(red$redlistCategory))))
colnames(scores) <- c("category", "score")
scores$category <- unique(red$redlistCategory)
scores$score <- c(0.4, NA, 0.1, 0.025, 0.2, NA, 0.4, 0.05, NA, NA, NA)

for (i in 1:length(pext$pext)) {
  ref <- match(pext$category[i], scores$category)
  pext$pext[i] <- scores$score[ref]
}


pext$Species <- gsub(" ", "_", pext$scientifcName)


print("Starting the EDGE calculcation")
source("EDGE_functions.R")

run <- ePD.loss.calc(one.tree, pext)

save(run, file = "../Results/WholeTree_EDGE2.Rdata")
save(tree_dat, file = "../Results/WholeTree_EDGE2_treedat.Rdata")

end.time <- proc.time()[3] - start.time

print("Run Completed!")

print(end.time)