



##################################### CALCULATING EDGE2 METRIC

# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree))

# read in pext & tree

pext <- sp.pext
str(pext)
tree <- Acipen.tree

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
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), EDGE = NA)
  # calculate IWE and TBL for each species
  nodes <- descendants(tree, rootNode(tree), "all")
  for(i in 1:length(nodes)){
    tips <- descendants(tree, nodes[i], "tips")
    tips <- names(tips)
    tipscores <- which(pext$species %in% tips)
    tree@edge.length[which(tree@edge[,2] == nodes[i])] <- edgeLength(tree, nodes[i])*prod(pext$pext[tipscores])
    print(paste("Node",i,"of",length(nodes),"transformed!", sep = " "))
  }
  plot(tree)
  for(i in 1:length(tree_dat$Species)){
    tree_dat$EDGE[i] <- sum(tree@edge.length[which(tree@edge[,2] %in% ancestors(tree, 
                                            which(tipLabels(tree) == tree_dat$Species[i]), "ALL"))], na.rm=T)
    print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!",sep=" ")) 
  }
  head(tree_dat)
  tree <- as(tree, "phylo")
  sum(tree$edge.length)
  sum(tree$edge.length)
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

exp.PD.trees <- list()

# remember to rename tree objects on lines 95-96 and 91

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
  
  
  print(i)
}
