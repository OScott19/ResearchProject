# In this script we are going to create a df with the pext values for all species, 100 times
# We will use this df when we calculate EDGE 2 using 100 trees

# housekeeping
graphics.off()
rm(list = ls())
setwd("~/Documents/ResearchProject/Code/")

# load packages
library(caper) #installed
library(phytools) #installed
library(phylobase) #installed
library(data.table) #installed
library(geiger) #installed
library(pez)  #installed


######## load in the data that we need
red <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)
tax <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)
tax$scientificName <- gsub(" ", "_", tax$genus.species)

load("../Data/one_tree.Rdata")

### create df with: species, category & ge score

Species <- data.frame(matrix(ncol = 3, nrow = length(tax$superclass)))
colnames(Species) <- c("species", "category", "GE")


Species$species <- tax$genus.species

for (x in 1:length(Species$species)) {
  ref <- match(Species$species[x], red$scientificName)
  if (length(ref) != 0) {
    Species$category[x] <- red$redlistCategory[ref]
  } 
}

# now we replace species with the format that matches the tree
Species$species <- gsub(" ", "_", Species$species)

# now we add in the ge scores - use "add ge" function

### load in functions we need

source("EDGE_functions.R")

Species <- calculate.GE(Species)
Species$GE <- as.numeric(Species$GE)


# identify the extinct species and cut them out of the tree

extinct <- subset(Species, Species$category == "Extinct")

save(extinct, file = "../Data/Extinct_species.Rdata") # we will use this again later!

nodes <- which(one.tree$tip.label %in% extinct$species)
tree.use <- drop.tip(one.tree, one.tree$tip.label[nodes]) 

# now, remove extinct species from the "Species" dataframe
length(extinct$species)

Species <- subset(Species, (Species$species %in% extinct$species ==F))


############ now we need to create the pext values to fill the table with

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


# Assign pext values to each GE
# These will be randomised for each GE among the species and we will draw NE/DD pexts from entire pext distribution

pext.LC <- data$rank.pext[data$pext == pext[2]]
pext.NT <- data$rank.pext[data$pext == pext[3]]
pext.VU <- data$rank.pext[data$pext == pext[4]]
pext.EN <- data$rank.pext[data$pext == pext[5]]
pext.CR <- data$rank.pext[data$pext == pext[6]]
pext.NA <- data$rank.pext[data$pext > pext[2]]


### We allocate these pexts into the Species df

Species$pext <- NA

## allocating

Species$pext[which(Species$GE == 0)] <- sample(pext.LC,length(Species$GE[which(Species$GE == 0)]),replace = T)
Species$pext[which(Species$GE == 1)] <- sample(pext.NT,length(Species$GE[which(Species$GE == 1)]),replace = T)
Species$pext[which(Species$GE == 2)] <- sample(pext.VU,length(Species$GE[which(Species$GE == 2)]),replace = T)
Species$pext[which(Species$GE == 3)] <- sample(pext.EN,length(Species$GE[which(Species$GE == 3)]),replace = T)
Species$pext[which(Species$GE == 4)] <- sample(pext.CR,length(Species$GE[which(Species$GE == 4)]),replace = T)

# GE = 5 == "Extinct in the wild"
pext.ENCR <- c(pext.EN, pext.CR) # mixture of endangered & critically endangered
Species$pext[which(Species$GE == 5)] <- sample(pext.ENCR,length(Species$GE[which(Species$GE == 5)]),replace = T)

# GE ## 6 "Lower Risk/ least concern" 
pext.NTLC <- c(pext.NT, pext.LC)
Species$pext[which(Species$GE == 6)] <- sample(pext.NTLC,length(Species$GE[which(Species$GE == 6)]),replace = T)

# GE == 7 "Lower Risk/conservation dependent"
pext.NTVUEN <- c(pext.NT, pext.VU, pext.EN)
Species$pext[which(Species$GE == 7)] <- sample(pext.NTVUEN,length(Species$GE[which(Species$GE == 7)]),replace = T)


# GE == 8 "Lower Risk/near threatenedt"
Species$pext[which(Species$GE == 8)] <- sample(pext.NTLC,length(Species$GE[which(Species$GE == 8)]),replace = T)

# Don't forget the NAs
Species$pext[which(is.na(Species$GE))] <- sample(pext.NA,length(Species$GE[which(is.na(Species$GE))]),replace = T)


## now, every species should have a pext value 

######### Now, we are going to run the loop above 100 times to create 100 pext values for each species

# holding df
pext.100 <- data.frame(matrix(ncol = 100, nrow = length(Species$species)))
pext.100[,1] <- Species$species

temp <- Species[,1:3]

for (i in 1:100) {
  Species <- temp
  Species$pext <- NA
  # allocate the scores
  Species$pext[which(Species$GE == 0)] <- sample(pext.LC,length(Species$GE[which(Species$GE == 0)]),replace = T)
  Species$pext[which(Species$GE == 1)] <- sample(pext.NT,length(Species$GE[which(Species$GE == 1)]),replace = T)
  Species$pext[which(Species$GE == 2)] <- sample(pext.VU,length(Species$GE[which(Species$GE == 2)]),replace = T)
  Species$pext[which(Species$GE == 3)] <- sample(pext.EN,length(Species$GE[which(Species$GE == 3)]),replace = T)
  Species$pext[which(Species$GE == 4)] <- sample(pext.CR,length(Species$GE[which(Species$GE == 4)]),replace = T)
  Species$pext[which(Species$GE == 5)] <- sample(pext.ENCR,length(Species$GE[which(Species$GE == 5)]),replace = T)
  Species$pext[which(Species$GE == 6)] <- sample(pext.NTLC,length(Species$GE[which(Species$GE == 6)]),replace = T)
  Species$pext[which(Species$GE == 7)] <- sample(pext.NTVUEN,length(Species$GE[which(Species$GE == 7)]),replace = T)
  Species$pext[which(Species$GE == 8)] <- sample(pext.NTLC,length(Species$GE[which(Species$GE == 8)]),replace = T)
  Species$pext[which(is.na(Species$GE))] <- sample(pext.NA,length(Species$GE[which(is.na(Species$GE))]),replace = T)
  
  # now we store the score
  pext.100[,(i+1)] <- Species$pext
  
}


################ save down this pext file 

save(pext.100, file =  "../Data/100_pext_vals.Rdata")

