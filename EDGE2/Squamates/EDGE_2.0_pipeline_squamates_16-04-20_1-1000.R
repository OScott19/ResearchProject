# housekeeping
rm(list=ls())
graphics.off()

# packages
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)


# load up  setup

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
iter <- 1

set.seed <- iter

print(iter)


# load in trees and species data
load("1000_full_squamate_trees_21-04-20.RData") # loads "phy.block.1000" (list of trees) and "Species" (df of deets)

#save(data,pext,file = "pext_scores_for_EDGE2.RData")
load("pext_scores_for_EDGE2.RData") # called "data"

# Assign pext values to each GE
# These will be randomised for each GE among the species and we will draw NE/DD pexts from entire pext distribution
pext.LC <- data$rank.pext[data$pext == pext[2]]
pext.NT <- data$rank.pext[data$pext == pext[3]]
pext.VU <- data$rank.pext[data$pext == pext[4]]
pext.EN <- data$rank.pext[data$pext == pext[5]]
pext.CR <- data$rank.pext[data$pext == pext[6]]
pext.NA <- data$rank.pext


# Now we calculate EDGE 2.0 for each phylogeny and combine the results

# EDGE 2.0 functionhange 
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

# List to store EDGE scores across distribution of trees
EDGE.2.list <- list()
exp.PD.list <- list()
exp.PD.trees <- list()

# now calculate EDGE2

  # randomly select pext scores for all species in tree based on their GE

  Species$pext <- 0
  Species$pext[which(Species$GE == 0)] <- sample(pext.LC,length(Species$GE[which(Species$GE == 0)]),replace = T)
  Species$pext[which(Species$GE == 1)] <- sample(pext.NT,length(Species$GE[which(Species$GE == 1)]),replace = T)
  Species$pext[which(Species$GE == 2)] <- sample(pext.VU,length(Species$GE[which(Species$GE == 2)]),replace = T)
  Species$pext[which(Species$GE == 3)] <- sample(pext.EN,length(Species$GE[which(Species$GE == 3)]),replace = T)
  Species$pext[which(Species$GE == 4)] <- sample(pext.CR,length(Species$GE[which(Species$GE == 4)]),replace = T)
  Species$pext[which(is.na(Species$GE))] <- sample(pext.NA,length(Species$GE[which(is.na(Species$GE))]),replace = T)
  col.num.1 <- which(colnames(Species) == "Species")
  col.num.2 <- which(colnames(Species) == "pext")
  sp.pext <- Species[,c(col.num.1,col.num.2)]
  names(sp.pext) <- c("Species","pext")
  # calculate EDGE 2
  res  <- EDGE.2.calc(phy.block.1000[[iter]],sp.pext)
  res2 <- data.frame(res[[1]],above.median = 0)
  res2$above.median[res2$EDGE > median(res2$EDGE)] <- 1
  EDGE.2.list[[length(EDGE.2.list)+1]] <- res2[order(res2$EDGE,decreasing = T),]
  exp.PD.list[[length(exp.PD.list)+1]] <- data.frame(PD = sum(phy.block.1000[[iter]]$edge.length),ePDloss = sum(res[[2]]$edge.length),
                                                     ePD = (sum(phy.block.1000[[iter]]$edge.length) - sum(res[[2]]$edge.length)))
  exp.PD.trees[[length(exp.PD.trees)+1]] <- res[[2]]
  print(iter)


name_to_save <- paste("Squamate_EDGE2_run", iter, ".Rdata", sep = "")

save(exp.PD.trees, exp.PD.list, EDGE.2.list, file = name_to_save)
