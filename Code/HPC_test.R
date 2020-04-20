# Walk through each step of the process

# Housekeeping
rm(list=ls())
graphics.off()
#setwd("../../../rds/general/user/ojs19/home")

# set the iteration number (1:100)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
set.seed <- iter


# first things first: packages

library(ape)
library(caper)
library(data.table)
require(geiger)
require(pez)
require(phytools)
require(phylobase)

# load in the data that we need
tree.to.load <- paste("Trees/Act_tree_updated_", iter, ".Rdata", sep = "")

load(tree.to.load) # they're called 'tree'

load("100_pext_vals.Rdata")
pext.100$X1 <- as.character(pext.100$X1)

# select the pext vals to use

pext <- data.frame(Species = as.character(pext.100$X1), pext = NA, stringsAsFactors = F)
pext$pext <- pext.100[,(iter + 1)]

# load up the edge function

EDGE.only.calc <- function(tree, pext){
  require(phylobase)
  require(data.table)
  # converts tree to phylo4 object
  if(!class(tree) == "phylo4"){
    tree <- as(tree, "phylo4")
  }
  names(pext) <- c("species","pext")
  plot(tree)
  # create df for data
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), pext = NA, EDGE = NA, ED = NA)
  for(i in 1:length(tree_dat$Species)){
    tree_dat$pext[i] <- pext$pext[pext$species == tree_dat$Species[i]]
  }
  # calculate IWE and TBL for each species
  nodes <- descendants(tree, rootNode(tree), "all")
  for(i in 1:length(nodes)){
    tips <- descendants(tree, nodes[i], "tips")
    tips <- names(tips)
    tipscores <- which(pext$species %in% tips)
    tree@edge.length[which(tree@edge[,2] == nodes[i])] <- edgeLength(tree, nodes[i])*prod(pext$pext[tipscores])
    #print(paste("Node",i,"of",length(nodes),"transformed!", sep = " "))
  }
  #plot(tree)
  for(i in 1:length(tree_dat$Species)){
    tree_dat$EDGE[i] <- sum(tree@edge.length[which(tree@edge[,2] %in% ancestors(tree, 
                                                                                which(tipLabels(tree) == tree_dat$Species[i]), "ALL"))], na.rm=T)
    #print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!",sep=" ")) 
  }
  # calculate ED
  tree_dat$ED <- tree_dat$EDGE / tree_dat$pext
  tree <- as(tree, "phylo")
  edge.res <- list(tree_dat, tree)
  return(edge.res)
}

### 

results.list <- paste(iter, "test complete", sep = "")


print("Calcs finished")

print("Saving results")

name_to_save <- paste("EDGE_HPC_test", iter, ".Rdata", sep = "")

save(results.list, file = name_to_save )
