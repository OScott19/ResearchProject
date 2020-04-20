
# EDGE 2.0 pipeline - squamates

# packages
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)

# set working directory
setwd("~/Documents/ResearchProject/Code/")

#phy.block.1000 <- #your phylo

#phy.block.1000 <- actinopt_full.trees # load it before!

# get pext for simulated GE distributions that has median pext = to doubling
# we model the distribution of scores to get a smooth curve.
treesim <- sim.bdtree(n=10000)
iucn <- sample(1:5, size=length(treesim$tip.label), replace=TRUE)
pext <- c(0.0025, .05, .1, .2, .4)
data <- data.frame(species=treesim$tip.label, pext=pext[iucn])
data <- data[order(data$pext),]
data$rank <- seq_len(nrow(data))

rank <- c(0, with(data, tapply(rank, pext, median)))
pext <- c(0, pext)
rank.sq <- rank^2; rank.cub <- rank^3; rank.qu <- rank^4; rank.quu <- rank^5
model <- lm(pext ~ rank + rank.sq + rank.cub + rank.qu)
data$rank.sq <- data$rank^2; data$rank.cub <- data$rank^3; data$rank.qu <- data$rank^4; data$rank.quu <- data$rank^5

data$rank.pext <- predict(model, data)
data$rank.pext[data$rank.pext <= 0] <- 0.0001


plot(data$rank,data$rank.pext)
plot(predict(model,data))

save(data,pext,file = "../Data/pext_scores_for_EDGE2.RData")
#load("pext_scores_for_EDGE2.RData")



# Assign pext values to each GE
# These will be randomised for each GE among the species and we will draw NE/DD pexts from entire pext distribution
pext.LC <- data$rank.pext[data$pext == pext[2]]
pext.NT <- data$rank.pext[data$pext == pext[3]]
pext.VU <- data$rank.pext[data$pext == pext[4]]
pext.EN <- data$rank.pext[data$pext == pext[5]]
pext.CR <- data$rank.pext[data$pext == pext[6]]
pext.NA <- data$rank.pext[data$pext > pext[2]]



# Now we calculate EDGE 2.0 for each phylogeny and combine the results

# EDGE 2.0 function
# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree
# pext - the pext of each species

EDGE.only.calc <- function(tree, pext){
  require(phylobase)
  require(data.table)
  # converts tree to phylo4 object
  if(!class(tree) == "phylo4"){
    tree <- as(tree, "phylo4")
  }
  # create df for data
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), EDGE = NA)
  # calculate IWE and TBL for each species
  nodes <- descendants(tree, rootNode(tree), "all")
  for(i in 1:length(nodes)){
    tips <- descendants(tree, nodes[i], "tips")
    tree@edge.length[which(tree@edge[,2] == nodes[i])] <- edgeLength(tree, nodes[i])*prod(pext$pext[pext$Species %in% tips])
    print(paste("Node",i,"of",length(nodes),"transformed!", sep = " "))
  }
  for(i in 1:length(tree_dat$Species)){
    tree_dat$EDGE[i] <- sum(ancestors(tree, which(tipLabels(tree) == tree_dat$Species[i]), "ALL"), na.rm=T)
    print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!",sep=" ")) 
  }
  head(tree_dat)
  edge.res <- list(tree_dat,tree)
  return(edge.res)
}
