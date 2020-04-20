# EDGE calculations for a single phylogeny - the Acipenseridae (sturgeon - 25 species)

# Housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

# packages
library(caper) #installed
library(phytools) #installed
library(phylobase) #installed
library(data.table) #installed
library(geiger) #installed
library(pez)  #installed

##################################### CALCULATING EDGE2 METRIC

# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree))
# pext - the probability of extinction of each species - see Mooers et al. 2008 for standard pext values

rm(list=ls())
graphics.off()

# TREE is going to be the acipen tree 

load(file = "../Data/AcipenTree.Rdata")

tree <- Acipen.tree

###  species will be the list of the species in the tree 

load(file = "../Data/AcipenseridaeForEDGE.Rdata")
species.GE <- Acipen.use
colnames(species.GE) <- c("Species", "Status")
species.GE$GE <- NA
species.GE$Species <-  gsub(" ", "_", species.GE$Species)
species.GE$Pext <- NA

pextISAAC <- c(0.025, 0.05, 0.1, 0.2, 0.4)


### starting with Isaac

# fuction: start with a dataframe with species & their red list assessment in 


source(file = "EDGE_functions.R")

### calculating pext for ISAAC
isaac.data <- data.frame(matrix(ncol = 1, nrow = 25))
isaac.data$Species <- species.GE$Species
isaac.data$Status <- species.GE$Status
isaac.data <- isaac.data[,-1]

isaac.data <- calculate.pext(pextISAAC, isaac.data)


############## ADDING IN NEW CODE


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

#save(data,pext,file = "../Data/pext_scores_for_EDGE2.RData")
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

pext <- sp.pext
tree2 <- 
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
  #plot(as(tree, "phylo"))
  #treePlot(as(tree, "phylo4"))
  for(i in 1:length(tree_dat$Species)){
    tree_dat$EDGE[i] <- sum(ancestors(tree, which(tipLabels(tree) == tree_dat$Species[i]), "ALL"), na.rm=T)
    print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!",sep=" ")) 
  }
  head(tree_dat)
  tree <- as(tree, "phylo")
  edge.res <- list(tree_dat,tree)
  return(edge.res)
}


### Saving pext
save(pext, file = "../Data/pext_acipensuridae_edge2.Rdata")

# List to store EDGE scores across distribution of trees
EDGE.2.list <- list()

# loop through each of the 1000 trees and calculate EDGE

Species <- species.GE[,c(1,2)]
Species$GE <- NA

risk <- c(0,1,2,3,4)

Species <- calculate.pext(ext.risk = risk, data = Species)
Species$GE <- Species$pext
Species$pext <- NA

for(i in 1:1) {
  # randomly select pext scores for all species in tree based on their GE
  i = 1
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
  res  <- data.frame(EDGE.only.calc(Acipen.tree,sp.pext)[[1]], above.median = 0, Iteration = i)
  res$above.median[res$EDGE > median(res$EDGE)] <- 1
  EDGE.2.list[[length(EDGE.2.list)+1]] <- res[order(res$EDGE,decreasing = T),]
  print(i)
}
