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

# import relevant data
# let's start with just one tree

load(file = "../Data/one_tree.Rdata") # a tree
load(file = "../Data/Spp_matched_results.Rdata") # 
red <- read.csv(file = "../Data/RedListDownload.csv", stringsAsFactors = F)
tree <- read.csv(file = "../Data/PFC_taxonomy.csv", stringsAsFactors = F)

# rename genus.species group so they have a "_" seperating them not a space
tree$genus.species <- gsub(" ", "_", tree$genus.species)


# list of the families in this group

families <- unique(tree$family)

tree.use <- one.tree

node.remove <- NULL

for (x in 1:length(families)) {
    temp.data <- tree[tree$family == families[x],]
    nodes <- which(tree.use$tip.label %in% temp.data$genus.species)
    node.remove <- c(node.remove, nodes[-1]) # store all the nodes but the first one
    tree.use$tip.label[nodes[1]] <- families[x]
    print(paste(x," of ",length(families),"!",sep=""))
}

tree.use <-  drop.tip(tree.use, tree.use$tip.label[node.remove]) 
save(tree.use, file = "../Data/FamilyTree.Rdata") # 

# check that there are the same number of unique tip labels as there are unique families in the family list 
length(unique(tree.use$tip.label)) == length(tree.use$tip.label)

plot <- plotTree(tree.use)

# renaming tipsrrrr

# first - round off the two numerical categories to 3 sig figs- don't want great long strigs

spp.matched.table$ProportionAssessed <- round(spp.matched.table$ProportionAssessed, digits = 1)
spp.matched.table$`ProportionEndangered-Total` <- round(spp.matched.table$`ProportionEndangered-Total`, digits = 1)


for(x in 1:length(tree.use$tip.label)){
  # step one, match the family in the tip label with the family group in the spp matched table
  ref <- as.numeric(match(tree.use$tip.label[x], spp.matched.table$Family))
  # creat a new label containing the interesting information
  label.temp <- paste(spp.matched.table$Family[ref], spp.matched.table$TotalSpp[ref], 
                      spp.matched.table$ProportionAssessed[ref], spp.matched.table$`ProportionEndangered-Total`[ref], sep = "_")
  # print current label
  print(tree.use$tip.label[x])
  # replace label with more informative one
  tree.use$tip.label[x] <- label.temp
  # print new label (can check the family groups are the same)
  print(tree.use$tip.label[x])
  }

save(tree.use, file = "../Data/FamilyTree_Named.Rdata")

named.plot <- treePlot(tree.use)

tree.use.phylo4 <- as(tree.use, "phylo4")

named_plot <- treePlot(tree.use.phylo4)

print("Script finished")
