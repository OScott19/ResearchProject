#clear workspace
rm(list=ls())
graphics.off()

# library the required packages

library(fishtree)
library(rfishbase)
library(picante)
library(geiger)
library(ape) # analyses of phylogenetics and evolution

library(dplyr)
library(tidyr)
library(ggplot2)
library(rredlist)


#set my working directory

setwd("Documents/ResearchProject/Code/")

# store Red list

my.key <- c("30c68b19cedcc7f1cee81aa9b07e1cd235c49d9af8c184d76998b05e59a77c22") 

# source(file = "Documents/CMEECourseWork/ResearchProject/Code/RedList_test.R")

head(redlist.all)

# initalising a dataframe of appropriate size
# we are going to capture: fish tree of life species name, WoRMS species, Red List species, RL rank 
# and whether there is genetic data present for each species. 

# as I'm not sure how many the total number of spp will be (as there may be lots of differences)
# I am going to initalist with 50k rows, and then add zeroes on to the end of each columns to 
# make them up to 50k (to start with)

master.db <- data.frame(matrix(ncol=6, nrow = 50000))

colnames(master.db) <- c("All.spp", "FToL.spp", "WoRMS.spp", "RL.spp", "RL.Rank", "GeneticData")

#################################################### 
########### DATA COLLECTION 1: FISH TREE OF LIFE 
####################################################


# initialise new frame (required for tidyr)
fishtree <- data.frame()

# subset fishbase for the correct class of fish
act.data <- subset(fishbase, fishbase$Class=="Actinopterygii")

head(act.data)

# this creates a new col called GS and combines genus and species
fishtree <- tidyr::unite(act.data, GS, Genus, Species, sep = " ")  

fishtree0 <- as.vector(rep(0, length(master.db$FToL.spp)-length(fishtree$GS)))
fishtree.50 <- c() # initialise a storage vector
fishtree.50 <- c(fishtree$GS, fishtree0)
# tail(fishtree.50) # checking that zeroes have been added to the edge

# add the species data to the correct column in the master database

master.db$FToL.spp <- fishtree.50

#################################################### 
########### DATA COLLECTION 2: RED LIST 
###################################################

#import whole dataset
# this was downloaded from the IUCN website, version 2019-3. If this version number
# changes, then re-download the data (freely available)
redlist.all <- read.csv("../Data/redlist_species_data_84c02514-9a98-4c0b-92de-465f9605a0e3/assessments.csv", stringsAsFactors = F)

#create data frame to hold the relevant redlist data
redlist <- data.frame(matrix(ncol=2, nrow= length(master.db$All.spp)))
colnames(redlist) <- c("RedList.species", "RedList.status")

# create the zeroes seperate vector, and make the legnth of the columns the right length 
redlist0 <- as.vector(rep(0, length(master.db$FToL.spp)-length(redlist.all$assessmentId)))

redlist[,1] <- c(redlist.all$scientificName, redlist0)
redlist[,2] <- c(redlist.all$redlistCategory, redlist0)

master.db$RL.spp <- redlist$RedList.species

# need to be careful here - these rankings will only correspond to the RED list 
# species, not the species in any other column 
master.db$RL.Rank <- redlist$RedList.status 

#################################################### 
########### DATA COLLECTION 3: WoRMS
###################################################

Actinop.all <- read.csv(file = "../Data/Full_Actiopterygii_WoRMS_data.csv")
Actinop.all <- Actinop.all[,-1]

usefulcols <- c(6,14,15,16)

WoRMs.use <- Actinop.all[,usefulcols]

length(WoRMs.use$scientificName) # 45978

#### do the usual things with zeroes, and add to the master database

Worms0 <- as.vector(rep(0, length(master.db$FToL.spp)-length(WoRMs.use$scientificName)))

Worms50 <- c(WoRMs.use$scientificName, Worms0)

master.db$WoRMS.spp <- Worms50

#################################################### 
########### CREATING THE MASTER LIST (list of all species)
###################################################

#add species from the fish tree
all.spp <- c()

# add species referenced in the Red List
length(WoRMs.use$scientificName)
all.all <- c(fishtree$GS, redlist$RedList.species,WoRMs.use$scientificName)
length(all.all)
unique.all <- as.data.frame(unique(all.all))
colnames(unique.all) <- "All.Species"
length(unique.all$All.Species) # 33167 # with WoRMS: 77389


sorted <- unique.all$All.Species

### add zeroes to the end so it fits into into the master db

all0 <- as.vector(rep(0, length(master.db$FToL.spp)-length(unique.all)))

master.db$All.spp <- c(unique.all, all0)


# get list of unique values 


#####################################h#### Processing the data##


# We can't perform an EDGE analysis on species that do not have IUCN data
# We are going to strip out the species that are not present on the Red List 
# i.e. just use the species on the Red List
# and also look at the species that are present on the Red List but not the FToL


#### Fish species that are present on both RedList and FToL:
RLnFT <- intersect(fishtree$GS, redlist$RedList.species)
length(RLnFT) # 16548 species are present on both lists 

# Fish tree & Worms
FTnW <- intersect(fishtree$GS, WoRMs.use$scientificName)
length(FTnW) # 16773 species overlap

# Red list & Worms
RLnW <- intersect(redlist$RedList.species, WoRMs.use$scientificName)
length(RLnW) # 9850 species overlap

# All three!

all3 <- intersect(RLnFT, WoRMs.use$scientificName)
length(all3) # 9253 spp
length(unique(all3)) # 9252


### Fish species on Red list but NOT fish tree of life:

# initialisea storage vector and a counter
RLonly <- c()
counter <- 0

# work through each entry in the red list 
for (i in 1:length(redlist$RedList.species)) {
  # if the red list species is not found on the FToL species list (i.e. match is False)
  if ((redlist$RedList.species[i] %in% master.db$FToL.spp)==F) {
    # then add one to the counter, and add the species to the RedList only vector
    counter <- counter + 1
    RLonly[counter] <- redlist$RedList.species[i]
  } 
}

length(RLonly) #1407
length(unique(RLonly)) #1407

###############################
# Species found in the Fish Tree of life, but not the red list (many more!)
  
FTonly <- c()
counter <- 0

# work through each entry in the red list 
# CLEAR THE COUNTER BEFORE RUNNING
for (i in 1:length(fishtree$GS)) {
  # if the red list species is not found on the FToL species list (i.e. match is False)
  if ((fishtree$GS[i] %in% redlist$RedList.species)==F && fishtree$GS[i] !="0") {
    # then add one to the counter, and add the species to the RedList only vector
    counter <- counter + 1
    FTonly[counter] <-fishtree$GS[i]
  } 
}

# check we have the number of entries that we are expecting, and that they're all unique
length(FTonly) #15211
length(unique(FTonly)) #15211 # it works! 

check <- 0
for (x in FTonly) {
  if (x == "0") {
    check <- check + 1
  }
}
check

##########################################################
# Now, we are going to check if any of the RedList entries "nearly" match those 
# in the Fish tree of life (i.e. - we are looking for typoes etc)

# this list is searching through all 1407 entries in the Red List only vector
# to see if there is anything approximately similar in the fish tree of life
# this search only saves the search terms that have results (exludes anything with 
# absolutey no matches).
# after the search is done, we manually check through to find matches 
fuzzy.list2 <- list()
counter <- 0
for (i in 1:length(RLonly)) {
  temp.data <- agrep(RLonly[i], FTonly, value=T, ignore.case = T, max.distance = 0.2)
  if (length(temp.data != 0)) {
  counter <- counter + 1
  fuzzy.list2[[counter]] <- i
  fuzzy.list2[[counter]][2] <- RLonly[i]
  fuzzy.list2[[counter]][3] <- temp.data
  }
}

####################
# What happened next?
# Went through and manually checked all of the entries in fuzzy.list2
# Created two lists: 
  #one contains the vector reference of all of the names that can be discarded (not a match)
  #second contains vector references of species to look up (names looked v similar but could be sep spp) 

non.match <- c(52,57,61, 89,113, 104,105,106,113,114,115,117,122,126,127,
               128,129,131,132,136139,141,144,151,154,156,157,158,159,164,165,166,
               167,168,169,170,175,187,192,195,207,208,209,212,215,218,220,221,231,
               232,233,238,241,243,244,247,248,252,253,268,269,271,272,274,275,278,
               283,288,294,296,297,300,302,303,304,305,306,307,309,316,323,324,327,328,
               330,334,336,337,338,342,343,344,345,346,347,353,355,359,360,361,362,
               363,364,369,371,372,376,379,385,386,389,392,393,395,396,397,399,404,
               406,408,409,410,412,414,415,416,422,423,428,429,432,433,434,436,438,
               439,441,443,444,445,447,448,450,451,452,453,455,456,457,458,459,464,
               466,467,468,469,472,473,474,478,481,487,488,489,493,495,498,512,516,
               517,519,520,521, 522,525,526,527,529,531,535,547,548,549,553,558,562,
               563,567,570,573,574,578,580,582,583,584,585,586,588,589,590,591,592,
               593,595,596,598,599,600,601,603,606,608,609,610,611,612,614,615,616,
               620,621,622,623,624,625,626,627,629,630,631,632,634,636,640,641,644,
               645, 646,647,648,649,650,651,652,653,654,656,657,658,659,660,661,662,
               663,664,665,666,667,668,669,670,672,673,674,675,676,677,678,681,682,
               683,685,686,687,688,689,690,691,692,693,694,695,696,697,698,700,701,
               703,704,705,706,708,708,710,711,712,717,722,723,724,725,726,728,729,
               730,731,732,735,736,738,739,740,741,742,743,744,745,746,747,750,751,
               753,760,761,762,763,764,765,766,767,768,769,772,773,774,775,776,777,
               778,779,780,781,782,783,784,785,786,787,788,789,800,801,802,803,804,
               805,806,807,808,809,812,813,814,815,816,817,818,819,820,821,823,824,
               825,826,827,828,829,830,831,832,833,836,837,838,839,840,843,844,846,
               847,848,849,852,853,853,854,855,856,857,858,861,864,867,869,877,883,
               887,888,889,894,895,896,898,901,902)

investigate <- c(39,40, 58, 87,174,184,280,282,298,356,546, 576,604,655,
                 671,684,727,810,841,884)

dont.add.straight.away <- c(non.match, investigate)

length(fuzzy.list2)-length(dont.add.straight.away)




##### RED LIST: KEEP SPECIES ID & SPECIES NAMES TOGETHER 
