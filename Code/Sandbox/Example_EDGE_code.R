# EDGE calculations for a single phylogeny

rm(list=ls())
graphics.off()


# packages
library(caper) #installed
library(phytools) #installed
library(phylobase) #installed
library(data.table) #installed
library(geiger) #installed
library(pez)  #installed




# set working directory - enter your desired folder location e.g. "c:/users/rikki/Desktop")
setwd("~/Documents/CMEECourseWork/ResearchProject/Code")

# read in phylogeny - this function works if the tree file is .nwk, .tre/.tres/.trees, .txt
# if extension is .nex then use "read.nexus()" function
tree <- read.tree("../../Data/croc_phylo.tre")

# read in csv with two columns: Species and GE 
# Species should have the names of the species in the phylogeny (if present) 
# with "_" rather than " " between genus and species e.g. "Homo_sapiens" rather than "Homo sapiens"
# GE should be the RL status converted to GE as outlined by Isaac et al. 2007 - 0 = LC, 1 = NT, 2 = VU, 3 = EN, 4 = CR
# DD and unassessed species have no GE score and therefore no EDGE score but should be included with GE listed as NA
species.GE <- read.csv("../../Data/croc_GE_example.csv",stringsAsFactors = F)

# calculate ED for all species in phylogeny
ed.scores <- ed.calc(tree)$spp

# get dataframe ready to calculate EDGE scores
edge.scores <- data.frame(ed.scores, GE = NA, EDGE = NA)

# run through each species and calculate EDGE scores using a for loop
for(i in 1:length(edge.scores$species)){
  # if the species in the tree is in the GE dataset
  if(edge.scores$species[i] %in% species.GE$Species){
    # if the GE is not NA (i.e. DD or unassessed species)
    if(!is.na(species.GE$GE[species.GE$Species == edge.scores$species[i]])){
      # assign GE to the dataset from the GE dataset
      edge.scores$GE[i] <- species.GE$GE[species.GE$Species == edge.scores$species[i]]
      # calculate EDGE score using ED and GE for the species
      edge.scores$EDGE[i] <- log(1+edge.scores$ED[i]) + (edge.scores$GE[i]*log(2))
    }
  }
}

# get EDGE species - those with greater than median ED and in a threatened RL category (VU, EN, CR)
edge.spp <- edge.scores[edge.scores$ED > median(edge.scores$ED),]
edge.spp <- edge.scores[edge.scores$GE %in% c(2,3,4),]
edge.spp <- edge.spp[order(edge.spp$EDGE, decreasing = T, na.last = T),]


# save the csvs
write.csv(edge.scores, "ED_EDGE_scores_TAXONOMICGROUP.csv",row.names = F)
write.csv(edge.spp, "EDGE_spp_TAXONOMICGROUP.csv",row.names = F)

# save the R data
save(tree, species.GE, ed.scores,edge.scores,edge.spp, file = "EDGE_calculations_TAXONOMICGROUP.RData")

# et voila!

# if you're feeling adventurous, here is the function for generating Expected PD Loss (Faith 2008, Steel et al. 2007),
# which underpins new EDGE metric

# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree
# pext - the probability of extinction of each species - see Mooers et al. 2008 for standard pext values

ePD.loss.calc <- function(tree, pext){
  require(phylobase)
  require(data.table)
  # converts tree to phylo4 object
  if(!class(tree) == "phylo4"){
    tree <- as(tree, "phylo4")
  }
  # create df for data
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), IWE = NA, TBL = NA, ED = NA, EDGE = NA)
  # calculate IWE and TBL for each species
  for(ii in 1:length(tipLabels(tree))){
    nodes <- ancestors(tree, ii, type = "ALL")
    root <- rootNode(tree)
    nodes <- nodes[-which(nodes == root)]
    a <- 0
    for(i in nodes){
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
    #print(ii)
  }
  return(tree_dat)
}










