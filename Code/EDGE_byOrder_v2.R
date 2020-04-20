# import adjusted red list
# import pext 
# import order data

#############################

# create a tree for each order group (refernce "whole ED" script)
# create appropriate subsets of the pext group to match the order group
# run the EDGE2 code
# save down each order's results!

############################ housekeeping

rm(list = ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

# load up the relevant packages 

library(caper) #installed
library(phytools) #installed
library(phylobase) #installed
library(data.table) #installed
library(geiger) #installed
library(pez)  #installed

# import data
load("../Data/UpdatedTrees/Act_tree_updated_1.Rdata")
load("../Data/OrderData_Groups.Rdata")
load("../Data/100_pext_vals.Rdata")

tax <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)
tax$scientificName <- gsub(" ", "_", tax$genus.species)

#### EDGE FUNCTION
source("EDGE_functions.R")



#### Step one: split the orders into groups
# then create a tree for just that order group


tree.list <- list()


for (i in 1:length(unique(order.data$group))) {
  orders.use <- order.data$Order[order.data$group == i]
  temp.data <- subset(tax, tax$order %in% orders.use)
  nodes <- which(tree.use$tip.label %in% temp.data$scientificName)
  cut.tree <-  drop.tip(tree.use, tree.use$tip.label[-nodes]) 
  tree.list[[i]] <- cut.tree
  #print(orders.use)
}

#save(tree.list, file = "../Data/Trees_byOrderGroup.Rdata")  
#load("../Data/Trees_byOrderGroup.Rdata")

# now we run edge two 

edge.list <- list()

pext <- pext.100[,1:2]
colnames(pext) <- c("species", "pext")

for (i in 1:length(unique(order.data$group))) {
  tree.use <- tree.list[[i]]
  edge.res <- EDGE.only.calc(tree = tree.use, pext = pext)
  edge.list[[i]] <- edge.res
  print(paste("Transformed order.data group", i, sep = " "))
  save(edge.list, file = "../Data/Order_EDGE2_Run.Rdata")
  }

load("../Data/Order_EDGE2_Run.Rdata")



# patching the results back together

# we want to calculate PD loss so we add up the branch lengths in each tree
# and compare to the untransformed tree

all.lengths <- c()

for (i in 1:length(edge.list)) {
  transformed.tree <- edge.list[[i]][[2]]
  t.tree <- as(transformed.tree, "phylo")
  length <- sum(t.tree$edge.length)
  all.lengths <- c(all.lengths, length)
}

orders.pd.lost <- sum(all.lengths)
#### calculate branch lengths of the untransformed trees to compare

untransformed.lengths <- c()

for (i in 1:length(tree.list)) {
  u.tree <- tree.list[[1]]
  length <- sum(u.tree$edge.length)
  untransformed.lengths <- c(untransformed.lengths, length)
}

orders.original.pd <- sum(untransformed.lengths)

orders.remaining.pd <- orders.original.pd - orders.pd.lost

orders.lost.pd.percentage <- orders.pd.lost / orders.original.pd * 100
orders.remaining.pd.percentage <- orders.remaining.pd / orders.original.pd * 100          
  





##### what if I try with the whole tree

whole.tree.2 <-EDGE.only.calc(tree = tree, pext = pext)
EDGE.2.list <- list()
res <- whole.tree.2
res2  <- data.frame(res[[1]], above.median = 0, Iteration = i)
res2$above.median[res$EDGE > median(res$EDGE)] <- 1
EDGE.2.list[[length(EDGE.2.list)+1]] <- res2[order(res2$EDGE,decreasing = T),]
exp.PD.trees <- list()
exp.PD.trees[[length(exp.PD.trees)+1]] <- data.frame(PD = sum(tree$edge.length),ePDloss = sum(res[[2]]$edge.length),
                                                     ePD = (sum(tree$edge.length) - sum(res[[2]]$edge.length)))





save(whole.tree.2, file = "../Data/WholeTree_EDGE2Results.Rdata")
load("../Data/WholeTree_EDGE2Results.Rdata")


EDGE.scores <- whole.tree.2[[1]]

### now sum up lost PD

whole.tree.pd <- sum(tree$edge.length)
edge2.tree <- whole.tree.2[[2]]

whole.pd.lost <- sum(edge2.tree$edge.length)

whole.pd.remaining <- whole.tree.pd - whole.pd.lost

whole.lost.pd.percentage <- whole.pd.lost / whole.tree.pd * 100
whole.remaining.pd.percentage <- whole.pd.remaining / whole.tree.pd * 100
