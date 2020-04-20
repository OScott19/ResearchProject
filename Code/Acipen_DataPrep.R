# In this script we prepare the data that will be fed into the scripts that calculate
# the EDGE and HEDGE scores for the Acipensuridae

#housekeeping
rm(list=ls())
graphics.off()

# set wd
setwd("~/Documents/ResearchProject/Code/")
source("EDGE_functions.R")

#load in useful data

redlist <- read.csv(file = "../Data/RedListDownload.csv", stringsAsFactors = F)

a <- toupper("Acipenseridae") # this creates a vector with 'Acipenseridae' in all caps 

Acipen <- subset(redlist, redlist$familyName == a)

cols.use <- c(4,5)
Acipen.use <- Acipen[,cols.use]


## now we allocate scores: 
# GE should be the RL status converted to GE as outlined by Isaac et al. 2007 - 0 = LC, 1 = NT, 2 = VU, 3 = EN, 4 = CR

scores <- c(0,1,2,3,4)

Acipen.use <- calculate.pext(data = Acipen.use, ext.risk = scores) # use function pre-developed 

######

save(Acipen.use, file = "../Data/AcipenseridaeForEDGE.Rdata")

##### now we create the correct tree for this group 


# packages
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)



tree_all <- read.tree("../Data/actinopt_full.trees")
one_tree <- tree_all[[1]]
or_this <- tree_all[[1]]

# use MRCA to find the most recent common ancestors of the Acipenseridae family
# to do this, we need to find the node numbers associated with each of the species 

# turn the data into the correct format: needs to be in a 'phylo4d' format: 
new_data <- phylo4d(x=or_this)

# and we need our species to match those in the tree (with underscores seperating them)

Acipen.use$Species <- gsub(" ", "_", Acipen.use$Species)

###

### TEST WITH JUST ONE SPECIES to see if the code works as I expect

huso <- as.vector("Huso_dauricus") # target name 

test <- getNode(new_data, huso, type = "all") # this works: now we need to find all of the nodes 

### now we can move to the example EDGE code script 

#nodes_all <- getNode(new_data, Acipen.use$Species, type = "all")
#nodes_all 
#ancestors_all <- MRCA(new_data, nodes_all)
#new.tree <- keep.tip(or_this, ancestors_all)

### didn't actually need to do any of that (oh dear)

nodes_all <- getNode(new_data, Acipen.use$Species, type = "all")
Acipen.tree <- c()
Acipen.tree <- drop.tip(or_this, or_this$tip.label[-nodes_all]) 


# Can now do an edge list on this tree? 

save(Acipen.tree, file = "../Data/AcipenTree.Rata")


### Now, let's get the Acipen section from each of the 100 trees

tree.list <- list()

for (i in 1:100) {
  tree.temp <- tree_all[[i]]
  tree.temp.2 <-  phylo4d(x = tree.temp)
  locations <- getNode(tree.temp.2, Acipen.use$Species, type = "all")
  cut.temp <-  drop.tip(tree.temp, tree.temp$tip.label[-locations]) 
  tree.list[[i]] <- cut.temp
  
}


save(tree.list, file = "../Data/100AcipenTrees.Rdata")
