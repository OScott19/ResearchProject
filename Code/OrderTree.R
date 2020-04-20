# This script is going to create a phylogenetic tree but of family groups
# It will help us examine which family groups are related to one another

# housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

# load relevant packages

library(phylobase)
library(phytools)
library(rredlist)

# import relevant datar
# let's start with just one tree

load(file = "../Data/one_tree.Rdata") # a tree
red <- read.csv(file = "../Data/RedListDownload.csv", stringsAsFactors = F)
tree <- read.csv(file = "../Data/PFC_taxonomy.csv", stringsAsFactors = F)

# rename genus.species group so they have a "_" seperating them not a space
tree$genus.species <- gsub(" ", "_", tree$genus.species)


# list of the families in this group

orders <- unique(tree$order)

tree.use <- one.tree

node.remove <- NULL

for (x in 1:length(orders)) {
  temp.data <- tree[tree$order == orders[x],]
  nodes <- which(tree.use$tip.label %in% temp.data$genus.species)
  node.remove <- c(node.remove, nodes[-1]) # store all the nodes but the first one
  tree.use$tip.label[nodes[1]] <- orders[x]
  print(paste(x," of ",length(orders),"!",sep=""))
}

tree.use <-  drop.tip(tree.use, tree.use$tip.label[node.remove]) 
save(tree.use, file = "../Data/OrderTree.Rdata") # 

# check that there are the same number of unique tip labels as there are unique families in the family list 
length(unique(tree.use$tip.label)) == length(tree.use$tip.label)

plot <- plotTree(tree.use)

# renaming tips

# create table with information: Order name & number of species

order.data <- data.frame(matrix(ncol = 2, nrow = length(orders)))
colnames(order.data) <- c("Order", "Species")

order.data$Order <- tree.use$tip.label

#order.data$Species <- length(tree$superclass[tree$order==order.data$Order])

for (x in 1:length(orders)) {
  order.data$Species[x] <- length(tree$superclass[tree$order==order.data$Order[x]])
}

for(x in 1:length(tree.use$tip.label)){
  # step one, match the family in the tip label with the family group in the spp matched table
  ref <- as.numeric(match(tree.use$tip.label[x], order.data$Order))
  # creat a new label containing the interesting information
  label.temp <- paste(order.data$Order[ref], order.data$Species[ref], sep = "_")
  # print current label
  print(tree.use$tip.label[x])
  # replace label with more informative one
  tree.use$tip.label[x] <- label.temp
  # print new label (can check the family groups are the same)
  print(tree.use$tip.label[x])
}

save(tree.use, file = "../Data/OrderTree_Named.Rdata")

named.plot <- treePlot(tree.use)

tree.use.phylo4 <- as(tree.use, "phylo4")

named_plot <- treePlot(tree.use.phylo4)

#####################

# Now we are going to assign the phylo trees into groups 

order.data$group <- NA
order.data$group.spp <- NA

order.data$group[1:8] <- 1
order.data$group[9:10] <- 2
order.data$group[11:15] <- 3
order.data$group[16] <- 16
order.data$group[17] <- 4
order.data$group[18:23] <- 5
order.data$group[24:31] <- 6
order.data$group[32:38] <- 7
order.data$group[39:42] <- 8
order.data$group[43:45] <- 9
order.data$group[46:52] <- 10
order.data$group[53:54] <- 11
order.data$group[55:60] <- 12
order.data$group[61:63] <- 13
order.data$group[64:65] <- 14
order.data$group[66:68] <- 15

# sum up how many species are in each group - theyre definitely not even but let's look anyway

for (i in 1:length(unique(order.data$group))) {
  temp <- subset(order.data, order.data$group == i)
  spp <- as.numeric(sum(temp$Species))
  order.data$group.spp[order.data$group == i] <- spp
}


save(order.data, file = "../Data/OrderData_Groups.Rdata")
