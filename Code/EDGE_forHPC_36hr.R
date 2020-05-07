# Walk through each step of the process

# Housekeeping
rm(list=ls())
graphics.off()


# set the iteration number (1:100)

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
print(iter)
set.seed <- iter

vals <- c(6,7,8,9,10,13,19,21,59)

iter <- vals[iter]
print(iter)

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

print(tree.to.load)

load(tree.to.load) # they're called 'tree'

load("100_pext_vals.Rdata")
pext.100$X1 <- as.character(pext.100$X1)

# select the pext vals to use

pext <- data.frame(Species = as.character(pext.100$X1), pext = NA, stringsAsFactors = F)
pext$pext <- pext.100[,(iter + 1)]

# load up the edge function

# EDGE 2.0 function
# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree
# pext - the pext of each species
# iteration - the numeric value to be assigned to the run - e.g. the i in a for loop of 'i in 1:n'

EDGE.2.calc <- function(tree, pext){
  require(phylobase)
  require(data.table)
  require(caper)
  # create df for data
  if(!class(tree) == "phylo"){
    tree <- as(tree, "phylo")
  }
  tree_dat <- data.frame(Species = as.character(unique(tree$tip.label)),
                         TBL = tree$edge.length[sapply(c(1:length(tree$tip.label)),
                                                       function(x,y) which(y==x),y=tree$edge[,2])], 
                         pext = NA, ED = NA, EDGE = NA)
  
  # converts tree to phylo4 object
  tree <- as(tree, "phylo4")
  names(pext) <- c("species","pext")
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
    print(paste("Node",i,"of",length(nodes),"transformed!", sep = " "))
  }
  #plot(tree)
  for(i in 1:length(tree_dat$Species)){
    tree_dat$EDGE[i] <- sum(tree@edge.length[which(tree@edge[,2] %in% ancestors(tree, 
                                                                                which(tipLabels(tree) == tree_dat$Species[i]), "ALL"))], na.rm=T)
    tree_dat$ED[i] <- tree_dat$EDGE[i] / tree_dat$pext[i] 
    print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!",sep=" ")) 
  }
  tree <- as(tree, "phylo")
  edge.res <- list(tree_dat,tree)
  return(edge.res)
}


### use the edge function

print("Using EDGE function")

res <- EDGE.2.calc(tree, pext)

print("EDGE finished")

print("Saving results")

name_to_save <- paste("EDGE_HPC_36hr_", iter, ".Rdata", sep = "")

save(res, file = name_to_save )


print("All done")