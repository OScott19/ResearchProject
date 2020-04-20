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


# read in phylogeny - this function works if the tree file is .nwk, .tre/.tres/.trees, .txt
# if extension is .nex then use "read.nexus()" function

load(file = "../Data/AcipenTree.Rdata") # this loads a single tree

# VISUALISE THE TREE & SAVE IT DOOWN
tree.phylo4 <- as(Acipen.tree, "phylo4") # this converts the phylo object (tree) into a phylo4 tree, which is what the package needs

acipen.plot <- treePlot(tree.phylo4) # creaes a visual of the tree
dev.print(png, "../Data/AcipenTreePlot.png") # SAVES as png

save(tree.phylo4, file="../Data/AcipenTreePlot.pdf") # saves as a pdf

tree <- Acipen.tree # renames the tree as tree

# read in csv with two columns: Species and GE 
# Species should have the names of the species in the phylogeny (if present) 
# with "_" rather than " " between genus and species e.g. "Homo_sapiens" rather than "Homo sapiens"
# GE should be the RL status converted to GE as outlined by Isaac et al. 2007 - 0 = LC, 1 = NT, 2 = VU, 3 = EN, 4 = CR
# DD and unassessed species have no GE score and therefore no EDGE score but should be included with GE listed as NA

load(file = "../Data/AcipenseridaeForEDGE.Rdata") # this is a list of the species and their rest list rankings
species.GE <- data.a
colnames(species.GE) <- c("Species", "Status", "GE")
species.GE$Species <-  gsub(" ", "_", species.GE$Species)

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
write.csv(edge.scores, "../Results/ED_EDGE_scores_Acipenseridae.csv",row.names = F)
write.csv(edge.spp, "../Results/EDGE_spp_Acipenseridae.csv",row.names = F)

# save the R data
save(tree, species.GE, ed.scores,edge.scores,edge.spp, file = "../Data/EDGE_calculations_Acipenseridae.RData")

# et voila!


################################ NOW TRY WITH 100 TREES AND SEE IF THERE IS MUCH DIFFERENCE
#
# advance prep:
load(file = "../Data/AcipenseridaeForEDGE.Rdata")
species.GE <- data.a
colnames(species.GE) <- c("Species", "Status", "GE")
species.GE$Species <-  gsub(" ", "_", species.GE$Species)

results.list <- list()
edge.scores <- data.frame(ed.scores, GE = NA, EDGE = NA)

# load in the trees
load("../Data/100AcipenTrees.Rdata")


for (x in 1:100 ) { # 100: number of trees
  tree <- tree.list[[x]]
  ed.scores <- ed.calc(tree)$spp
  edge.scores <- data.frame(ed.scores, GE = NA, EDGE = NA)
  
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
  results.list[[x]] <- edge.scores
}

all.edge.scores <- data.frame(matrix(nrow = 25, ncol = 100))

for (i in 1:100) {
  all.edge.scores[,i] <- results.list[[i]]$EDGE
}

all.edge.data <- data.frame(matrix(ncol = 5, nrow = 25))

all.edge.data$means <- rowMeans(all.edge.scores)
all.edge.data$mins <- apply(all.edge.scores, 1, FUN=min)
all.edge.data$max <- apply(all.edge.scores, 1, FUN=max)

all.edge.data <- all.edge.data[,6:8]

for (x in 1:25) {
  if (all.edge.data$mins[x] != all.edge.data$max[x]) {    
    print(x)
  }
}

# save the outputs
write.csv(all.edge.scores, "../Results/100trees_ED_EDGE_scores_Acipenseridae.csv",row.names = F)
write.csv(all.edge.data, "../Results/100trees_EDGE_spp_Acipenseridae.csv",row.names = F)


#### THERE ARE THREE DIFFERENCES! 


# is this the full number of sturgeon?

library(rfishbase)


sturgeon.fam <- rfishbase::species_list(Family = "Acipenseridae") # there are 25 names on this list

# put in same format as the fish tree ones 

Fishbase <-  as.character(gsub(" ", "_",sturgeon.fam))

sturgeon.names <- c()   
sturgeon.names$FishBase <- Fishbase
sturgeon.names$FishTree <- species.GE$Species

for (i in 1:length(sturgeon.names$FishBase)) {
  if (sturgeon.names$FishBase[i] %in% sturgeon.names$FishTree) {
    print(sturgeon.names$FishBase[i])
    print("is present")
  }
  else {
    print("Oh no")
    print(sturgeon.names$FishBase[i])
    print("is different")
  }
}

# so, there are 25 species of sturgeon


##################################### CALCULATING EDGE2 METRIC

# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree
# pext - the probability of extinction of each species - see Mooers et al. 2008 for standard pext values
rm(list=ls())
graphics.off()

# TREE is going to be the acipen tree 

tree <- load(file = "../Data/AcipenTree.Rdata")

tree <- Acipen.tree

###  species will be the list of the species in the tree 

load(file = "../Data/AcipenseridaeForEDGE.Rdata")
species.GE <- data.a
colnames(species.GE) <- c("Species", "Status", "GE")
species.GE$Species <-  gsub(" ", "_", species.GE$Species)
species.GE$Pext <- NA

pextISAAC <- c(0.025, 0.05, 0.1, 0.2, 0.4)
pext100 <- c(0.0001, 0.01, 0.1, 0.667, 0.999)
pext50 <- c(0.00005, 0.004, 0.05, 0.42, 0.97)
pext500 <- c(0.0005, 0.02, 0.39, 0.996, 1)
pextPess <- c(0.2, 0.4, 0.8, 0.9, 0.99)

### starting with Isaac

# fuction: start with a dataframe with species & their red list assessment in 


source(file = "EDGE_functions.R")

### calculating pext for ISAAC
isaac.data <- data.frame(matrix(ncol = 1, nrow = 25))
isaac.data$Species <- species.GE$Species
isaac.data$Status <- species.GE$Status
isaac.data <- isaac.data[,-1]

isaac.data <- calculate.pext(pextISAAC, isaac.data)



##### pextnct 


hedge_isaac <- ePD.loss.calc(Acipen.tree, isaac.data)

### save down 

write.csv(hedge_isaac, "../Results/HEDGE_Acipenseridae.csv",row.names = F)


