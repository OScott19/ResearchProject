#housekeeping
rm(list=ls())
graphics.off()


# set wd
setwd("~/Documents/ResearchProject/Code/")


#load in useful data
load("../Data/RedListFishBaseFamilyAnalysis.Rdata")
load("../Data/APIScriptOutput.Rdata")
load("../Data/APIScriptOutput2.Rdata")


store <- data.frame()

a <- toupper("Acipenseridae")

for (i in 1:length(taxanomic.data)) {
  if (a %in% taxanomic.data[[i]] ) {
    temp.name <- taxanomic.data[[i]]$name
    temp.rest <- taxanomic.data[[i]]$result
    temp <- cbind(as.character(temp.name), temp.rest)
    store <- rbind(store, temp)
  }
}


## lets try another approach

data <- read.csv("../Data/FBandRed.csv", stringsAsFactors = F)
data.a <- subset(data, data$Family == "Acipenseridae")

cols.use <- c(2,3)
data.a <- data.a[,cols.use]
data.a$Scores <- NA

## now we allocate scores: 
# GE should be the RL status converted to GE as outlined by Isaac et al. 2007 - 0 = LC, 1 = NT, 2 = VU, 3 = EN, 4 = CR

for (x in 1:length(data.a$ScientificName)) {
if (data.a$RedStatus[x] == "Critically Endangered") {
  data.a$Scores[x] <-  4
}}

for (x in 1:length(data.a$ScientificName)) {
  if (data.a$RedStatus[x] == "Endangered") {
    data.a$Scores[x] <-  3
  }}

for (x in 1:length(data.a$ScientificName)) {
  if (data.a$RedStatus[x] == "Vulnerable") {
    data.a$Scores[x] <-  2
  }}


for (x in 1:length(data.a$ScientificName)) {
  if (data.a$RedStatus[x] == "Near Threatened") {
    data.a$Scores[x] <-  1
  }}



for (x in 1:length(data.a$ScientificName)) {
  if (data.a$RedStatus[x] == "Least Concern") {
    data.a$Scores[x] <-  0
  }}

######



save(data.a, file = "../Data/AcipenseridaeForEDGE.Rdata")

##### now we create the correct tree for this group 


# packages
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)



tree_all <- read.tree("../Data/actinopt_full.trees")
one_tree <- tree_all[1]
or_this <- tree_all[[1]]

# use MRCA to find the most recent common ancestors of the Acipenseridae family
# to do this, we need to find the node numbers associated with each of the species 

# turn the data into the correct format: needs to be in a 'phylo4d' format: 
new_data <- phylo4d(x=or_this)

# and we need our species to match those in the tree (with underscores seperating them)

data.a$ScientificName <- gsub(" ", "_", data.a$ScientificName)

###


### TEST WITH JUST ONE 
hi <- as.vector("Huso_dauricus") # target name 

test <- getNode(new_data, hi, type = "all") # this works: now we need to find all of the nodes 

# now we can move to the example EDGE code script 

test_all <- getNode(new_data, data.a$ScientificName, type = "all")
test_all

ancestors_all <- MRCA(new_data, test_all)


new.tree <- keep.tip(or_this, ancestors_all)
### didn't actually need to do any of that (oh dear)

Acipen.tree <- c()
Acipen.tree <- drop.tip(or_this, or_this$tip.label[-test_all]) 

# Can now do an edge list on this tree? 

save(Acipen.tree, file = "../Data/AcipenTree.Rata")
