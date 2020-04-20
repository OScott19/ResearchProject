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


# import data

load("../Data/Red_Categorymod.Rdata")
load("../Data/one_tree.Rdata")
load("../Data/OrderData_Groups.Rdata")
load("../Data/Sp_pext_ready.Rdata")
tree <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)

tree$scientificName <- gsub(" ", "_", tree$genus.species)

#### source functions
source("EDGE_PD_LOSS_NONNY.R")

tree.use <- one.tree
tree.phylo4 <- as(tree.use, "phylo4")

#### Step one: split the orders into groups
# then create a tree for just that order group

tree.list <- list()


for (i in 1:length(unique(order.data$group))) {
  orders.use <- order.data$Order[order.data$group == i]
  temp.data <- subset(tree, tree$order %in% orders.use)
  nodes <- which(tree.use$tip.label %in% temp.data$scientificName)
  cut.tree <-  drop.tip(tree.use, tree.use$tip.label[-nodes]) 
  tree.list[[i]] <- cut.tree
  print(orders.use)
}

save(tree.list, file = "../Data/Trees_byOrderGroup.Rdata")  
load("../Data/Trees_byOrderGroup.Rdata")

# now we run edge two 

edge.list <- list()

for (i in 1:length(unique(order.data$group))) {
  tree.use <- tree.list[[i]]
  pext <- pext
  edge.res <- EDGE.only.calc(tree = tree.use, pext = pext)
  edge.list[[i]] <- edge.res
  #print(order.data$group[i])
  save(edge.list, file = "../Data/Order_EDGE_Run.Rdata")
}

load("../Data/Order_EDGE_Run.Rdata")

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

pd.lost <- sum(all.lengths)
#### calculate branch lengths of the untransformed trees to compare

untransformed.lengths <- c()

for (i in 1:length(tree.list)) {
  u.tree <- tree.list[[1]]
  length <- sum(u.tree$edge.length)
  untransformed.lengths <- c(untransformed.lengths, length)
}

total.tree.pd <- sum(untransformed.lengths)
total.pd.lost <- sum(all.lengths)
remaining.pd <- totalPD - total.pd.lost









##### what if I try with the whole tree

#### source functions
source("EDGE_PD_LOSS_NONNY.R")

whole.tree <-EDGE.only.calc(one.tree, pext)
