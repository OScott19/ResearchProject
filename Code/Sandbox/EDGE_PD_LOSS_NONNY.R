# function to calculate EDGE only for species and Expected PD Loss

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
  # Internal weighted edges - sum of branch lengths that aren't the TBL that make up the ED/EDGE score of teh species
  nodes <- descendants(tree, rootNode(tree), "all")
  for(i in 1:length(nodes)){
    tips <- descendants(tree, nodes[i], "tips")
    tree@edge.length[which(tree@edge[,2] == nodes[i])] <- edgeLength(tree, nodes[i])*prod(pext$pext[pext$Species %in% tips])
    print(paste("Node",i,"of",length(nodes),"transformed!", sep = " "))
  }
  # remove this loop if you just want to calculate expected pd loss
  #for(i in 1:length(tree_dat$Species)){
  #  tree_dat$EDGE[i] <- sum(ancestors(tree, which(tipLabels(tree) == tree_dat$Species[i]), "ALL"), na.rm=T)
  #  #print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!",sep=" ")) 
  #}
  #head(tree_dat)
  # summing the branch lengths of the 'tree' object gives you expected PD loss for the clade - you can
  # calculate a proportion of the total PD from the unaltered tree
  edge.res <- list(tree_dat,tree)
  return(edge.res)
}
