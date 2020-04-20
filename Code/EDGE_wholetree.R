############################ housekeeping

rm(list = ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

# load up the relevant packages 
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)


# import data

load("../Data/Red_Categorymod.Rdata")
load("../Data/one_tree.Rdata")
load("../Data/OrderData_Groups.Rdata")


tree <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)

tree$scientificName <- gsub(" ", "_", tree$genus.species)

#### source functions
source("EDGE_PD_LOSS_NONNY_v2_modified.R")

load("../Data/pext_for_HPC.Rdata")

pext$pext[which(is.na(pext$category))] <- sample(pext.NA, length(pext$category[which(is.na(pext$category))]), replace = T)

### run EDGE

print("Running EDGE calcs")

whole.tree <- EDGE.only.calc(one.tree, pext)

print("Saving results")
save(whole.tree, file = "../Data/EDGE_wholetree_runresults_v2.Rdata")

load("../Data/EDGE_wholetree_runresults_v2.Rdata")

print("Now looking to find PD loss")

one.phylo4 <- as(one.tree, "phylo4")

# extract the edge lengths

tot <- c()

for (i in 1:length(one.phylo4@edge.length)) {
  l <- one.phylo4@edge.length[[i]]
  tot <- c(tot, l)
}

tot <- tot[-1]

PD.untransformed <- sum(as.numeric(((tot)))) # full PD

tot.t <- c()

whole.tree.edge <- whole.tree[[1]]
whole.tree.phylo <- whole.tree[[2]]

for (i in 1:length(whole.tree.phylo@edge.length)) {
  l <- whole.tree.phylo@edge.length[[i]]
  tot.t <- c(tot.t, l)
}############################ housekeeping

rm(list = ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

# load up the relevant packages 
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)


# import data

load("../Data/Red_Categorymod.Rdata")
load("../Data/one_tree.Rdata")
load("../Data/OrderData_Groups.Rdata")


tree <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)

tree$scientificName <- gsub(" ", "_", tree$genus.species)

#### source functions
source("EDGE_PD_LOSS_NONNY_v2_modified.R")

load("../Data/pext_for_HPC.Rdata")

pext$pext[which(is.na(pext$category))] <- sample(pext.NA, length(pext$category[which(is.na(pext$category))]), replace = T)

#### Step one: split the orders into groups
# then create a tree for just that order group

print("Running EDGE calcs")

whole.tree <- EDGE.only.calc(one.tree, pext)

print("Saving results")
save(whole.tree, file = "../Data/EDGE_wholetree_runresults_v2.Rdata")

load("../Data/EDGE_wholetree_runresults_v2.Rdata")

print("Now looking to find PD loss")

one.phylo4 <- as(one.tree, "phylo4")

# extract the edge lengths

tot <- c()

for (i in 1:length(one.phylo4@edge.length)) {
  l <- one.phylo4@edge.length[[i]]
  tot <- c(tot, l)
}

tot <- tot[-1]

PD.untransformed <- sum(as.numeric(((tot)))) # full PD

tot.t <- c()

whole.tree.edge <- whole.tree[[1]]


tot.t <- tot.t[-1]
total.PD.loss <- sum(as.numeric(tot.t))  

loss.percentage <- total.PD.loss / PD.untransformed * 100

results <- list(tot, PD.untransformed, tot.t, total.PD.loss, loss.percentage)

print("Saving results")

save(results, "../Data/EDGE_wholetree_run.Rdata")

print("Finished running script")
