#### this one calculates the pextintion from a vector of the extinction risks and 
# a dataframe containing the species in col 1 and the RedList status in col 2


calculate.pext <- function(ext.risk, data) {
  data[,3] <- NA
  colnames(data) <- c("Species", "Status", "pext")
  for (i in 1:length(data$pext)) {
    if (data[i,2] == "Least Concern") {
      data$pext[i] <- ext.risk[1]
    }
    if (data[i,2] == "Near Threatened") {
      data$pext[i] <- ext.risk[2]
    }
    if (data[i,2] == "Vulnerable") {
      data$pext[i] <- ext.risk[3]
    }
    if (data[i,2] == "Endangered") {
      data$pext[i] <- ext.risk[4]
    }
    if (data[i,2] == "Critically Endangered") {
      data$pext[i] <- ext.risk[5]
    }
  } 
  return(data)
}



#### THIS IS FOR adding in the 'GE' column as part of the EDGE 2 step 

calculate.GE <- function(data) {
  ext.risk <- c(0:9)
  #data[,3] <- NA
  #colnames(data) <- c("species", "status", "GE")
  for (i in 1:length( data$GE )) {
    if( !is.na(data[i,2])) {
      
      if (data[i,2] == "Least Concern") {
        data$GE[i] <- ext.risk[1]
      }
      if (data[i,2] == "Near Threatened") {
        data$GE[i] <- ext.risk[2]
      }
      if (data[i,2] == "Vulnerable") {
        data$GE[i] <- ext.risk[3]
      }
      if (data[i,2] == "Endangered") {
        data$GE[i] <- ext.risk[4]
      }
      if (data[i,2] == "Critically Endangered") {
        data$GE[i] <- ext.risk[5]
      }
      if (data[i,2] == "Data Deficient") {
        data$GE[i] <- NA
      }
      if (data[i,2] == "Extinct in the Wild") {
        data$GE[i] <- ext.risk[6]
      }
      if (data[i,2] == "Lower Risk/least concern") {
        data$GE[i] <- ext.risk[7]
      }
      if (data[i,2] == "Lower Risk/conservation dependent") {
        data$GE[i] <- ext.risk[8]
      }
      if (data[i,2] == "Lower Risk/near threatened") {
        data$GE[i] <- ext.risk[9]
      }
      if (data[i,2] == "Extinct") {
        data$GE[i] <- 999
        print("Warning, extinct species in dataset")
      }
  } }
  return(data)
}



###################################

# inputs: tree (phylo or phylo4 object) and pext - a dataframe containin the list of species and the probabilities of extinction of these species
# don't use this one 
ePD.loss.calc <- function(tree, pext){
  #tree <- tree.phylo4
  require(phylobase)
  require(data.table)
  # converts tree to phylo4 object
  if(!class(tree) == "phylo4"){
    tree <- as(tree, "phylo4")
  }
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), IWE = NA, TBL = NA, ED = NA, EDGE = NA)
  # calculate IWE and TBL for each species
  for(ii in 1:length(tipLabels(tree))){
    ii <- 1
    nodes <- ancestors(tree, ii, type = "ALL")
    root <- rootNode(tree)
    nodes <- nodes[-which(nodes == root)]
    a <- 0
    for(i in nodes){
      #i = nodes[1]
      if(i == ii){
        tree_dat$TBL[ii] <- edgeLength(tree)[getEdge(tree, ii)]
      }else{
        tips <- descendants(tree, nodes[nodes == i], "tips")
        tips <- tips[-which(tips == ii)]
        # calculate total pext to transform branch
        a <- c(a,as.numeric(edgeLength(tree)[getEdge(tree, i)] * prod(pext$pext[pext$Species %in% tipLabels(tree)[tips]])))
      }
    }
    tree_dat$IWE[ii] <- sum(a)
    tree_dat$ED[ii] <- tree_dat$IWE[ii] + tree_dat$TBL[ii]
    tree_dat$EDGE[ii] <- tree_dat$ED[ii]*pext$pext[pext$Species == tree_dat$Species[ii]]
    head(tree_dat)
    print(ii)
  }
  return(tree_dat)
}

#########
## from ACIPEN_EDGE_code_v3.R


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



######

ED.only.calc <- function(tree, pext){
  require(phylobase)
  require(data.table)
  # converts tree to phylo4 object
  if(!class(tree) == "phylo4"){
    tree <- as(tree, "phylo4")
  }
  names(pext) <- c("species","pext")
  plot(tree)
  # create df for data
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), pext = NA)
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
  tree <- as(tree, "phylo")
  edge.res <- list(tree_dat, tree)
  return(edge.res)
}

##### and the updated EDGE 2 function (from Rikki 21 April Email)

# EDGE 2.0 function
# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree
# pext - the pext of each species
# iteration - the numeric value to be assigned to the run - e.g. the i in a for loop of 'i in 1:n'

EDGE.2.calc <- function(tree, pext, iteration){
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
    print(paste("Node",i,"of",length(nodes),"transformed!","For iteration",iteration, sep = " "))
  }
  #plot(tree)
  for(i in 1:length(tree_dat$Species)){
    tree_dat$EDGE[i] <- sum(tree@edge.length[which(tree@edge[,2] %in% ancestors(tree, 
                                                                                which(tipLabels(tree) == tree_dat$Species[i]), "ALL"))], na.rm=T)
    tree_dat$ED[i] <- tree_dat$EDGE[i] / tree_dat$pext[i] 
    print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!","For iteration",iteration,sep=" ")) 
  }
  tree <- as(tree, "phylo")
  edge.res <- list(tree_dat,tree)
  return(edge.res)
}
