graphics.off()
rm(list = ls())

setwd("~/Documents/ResearchProject/Code/")

## load required packages

library(fishtree)
library(rfishbase)
library(picante)
library(geiger)
library(ape) # analyses of phylogenetics and evolution

library(dplyr)
library(tidyr)
library(ggplot2)
library(rredlist)


source("ExplorationAllData.R")
source("FunctionsforComparisons.R")

### what are our list of species?

#WoRMS: worms$scientificName

#FishBase: fb$ScientificName

#FishTree: tree

#IUCN: red$scientificName

#################
################# WORMS AND RED LIST 

# quick check to see how many species match for the two data lists
RnW <- intersect(red$scientificName, worms$scientificName)  # worms has fewest matches, despite having largest database! 
length(RnW) # 9580

# and how many are in RL but not Worms & vice versa? 
RyWn <- data.frame()
RyWy <- data.frame(matrix(nrow = length(RnW), ncol = 5))
RyWyCols <- c("ScientificName", "RedListCategory", "RedAssessmetID", "Order", "Family")
colnames(RyWy) <- RyWyCols
# RESET IN BETWEEN EVERY RUN
countera <- 0
counterb <- 0
for (x in 1:length(red$scientificName)) {
  # if the red list species is not found on the  species list (i.e. match is False)
  if ((red$scientificName[x] %in% worms$scientificName)==T) {
    # then add one to the counter, and add the species to the RedList only vector
    countera <- countera + 1
    RyWy[countera,1] <-red$scientificName[x]
    RyWy[countera,2] <-red$redlistCategory[x]
    RyWy[countera,3] <-red$assessmentId[x] 
    }
  if ((red$scientificName[x] %in% worms$scientificName)==F) {
    # then add one to the counter, and add the species to the RedList only vector
    counterb <- counterb + 1
    RyWn[counterb,1] <-red$scientificName[x]
    RyWn[counterb,2] <-red$redlistCategory[x]
    RyWn[counterb,3] <-red$assessmentId[x] 
  }
}

# then want to add details about the family & genus of these species that overlap
# go through each of the species in the red list, check which WoRMS spp it matches, then add all of the family data

for (i in 1:length(RyWy$ScientificName)) {
  for (x in 1:length(worms$scientificName)) {
    if (worms$scientificName[x] == RyWy$ScientificName[i]) {
    RyWy[i,4] <- worms$order[x]
    RyWy[i,5] <- worms$family[x]
    }
  }
}

##### what is in WoRMs, but not the RedList?

#setup
RnWy <- c()
counterc <- 0

for (x in 1:length(worms$scientificName)) {
  # WORMS AND RED LIST
  if ((worms$scientificName[x] %in% red$scientificName)==F) {
    counterc <- counterc + 1
    RnWy[counterc] <- worms$scientificName[x]
  }
}

length(worms$scientificName) - (length(RyWy$X1) + length(RnWy)) # 39 species unaccounted for
length(red$scientificName) - (length(RyWy$X1) + length(RyWn$V1)) # this is zero - all spp accounted for





###########################



RnB <- intersect(red$scientificName, fb$ScientificName)
length(RnB) #16584

RaT <- intersect(red$scientificName, tree)
length(RnT) # 16386


##### matches within matches?

a <- intersect(RnW, RnB)
length(a) # 9250

b <- intersect (RnW, RnT)
length(b) # 9173

c <- intersect(RnB, RnT)
length(c) # 16320


##########################################

