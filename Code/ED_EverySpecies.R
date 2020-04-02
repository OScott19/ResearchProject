#I'd like to work out the ED of every species on the tree, to find the most ancient one
#Can't just do "tree.ed <- ed.calc(tree)$spp " as the tree is way too large
# steps: split the tree up into the 48 orders, and then calcuate the ED per order

print("Running the R file")

## housekeeping
rm(list = ls())
graphics.off()
#setwd("../ojs19/Documents/ResearchProject/Code/")

print("Loading packages")

# load packages
library(rfishbase)
library(phylobase)
library(phytools)
library(caper)


print("Reading in required data")
fishbase.orders <- unique(fishbase$Order)

FTdata <- read.csv(file = "../Data/PFC_taxonomy.csv", sep = ",", header = T, stringsAsFactors = F)
FTdata2 <- subset(FTdata, FTdata$class=="Actinopteri")
fishtree.orders <- unique(FTdata2$order)

# just use the fishtree orders, to make life simpler
# before loop: 
# change "genus species to genus_species
# trim uncecessary columns
# add column to store GE score

# in loop:
# create a sub-set of the fishtree for each of the genuses in order
# retrieve the nodes associated with each of the species in that subset
# store nodes in the gerneal storage vehicle
# create sub-tree by trimming all other nodes out 
# perform  the ED calculation 
# store ED next to species name & node number 
# eventually, repeat accross all 100 trees

#### prep data

print("Creating storage device")

FTdata2$genus.species <- gsub(" ", "_", FTdata2$genus.species)
col_use <- c(16,21)
data_use <- FTdata2[,col_use]
data_use$node <- NA
data_use$ED <- NA

### load & prep tree
tree.all <- read.tree("../Data/actinopt_full.trees")
#tree.use <- tree.all[[1]]
#tree.phylo4 <- phylo4d(x = tree.all[[1]])


data.100.trees <- list()

print("Starting loop")

for (t in 1:26) {
  tree.use <- tree.all[[t]]
  tree.phylo4 <- phylo4d(x = tree.use)
  data.all <- c()
    for (x in 1:length(fishtree.orders)) {
      temp.data <- subset(data_use, data_use$order==fishtree.orders[x])
      nodes <- getNode(tree.phylo4, temp.data$genus.species)
      temp.data$node <- nodes
      cut.temp <-  drop.tip(tree.use, tree.use$tip.label[-temp.data$node]) 
      ed.vector <- as.vector(ed.calc(cut.temp)$spp)
      temp.data$ED <- ed.vector$ED
      data.all <- rbind(data.all, temp.data)
      #print(x)
    }
  data.100.trees[[t]] <- data.all
  print(paste("Tree number:", t))
  save(data.100.trees, file = "data100trees_take4.Rdata")
}



