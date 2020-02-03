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


#source("ExplorationAllData.R")
source("FunctionsforComparisons.R")

### IMPORT THE DATA THAT WE ARE COMPARING

#WoRMS: worms$scientificName
worms <- read.csv(file= "../Data/Full_Actiopterygii_WoRMS_data.csv",sep = ",", 
                  fill = T, stringsAsFactors = F)

#FishBase: fb$ScientificName
fb <- read.csv(file = "../Data/fb.csv", header = T, sep = ",", stringsAsFactors = F, fill = T)
fb <- fb[,-1]

#FishTree: tree

tree <- read.csv(file = "../Data/tree.csv", 
                               header = T, sep = ",", stringsAsFactors = F, fill = T)
tree <- as.data.frame(tree[,-1])
colnames(tree) <- c("ScientificName")

#IUCN: red$scientificName

red <- read.csv(file = "../Data/red.csv", 
                 header = T, sep = ",", stringsAsFactors = F, fill = T)
red <- as.data.frame(red[,-1])
#################
################# WORMS AND RED LIST 

# quick check to see how many species match for the two data lists
RnW <- intersect(red$scientificName, worms$scientificName)  # worms has fewest matches, despite having largest database! 
length(RnW) # 9580

# Red List & Worms

WormsandRed <- data.frame()
WormsandRed <- RedOverlap(RedName = red$scientificName, RedCategory = red$redlistCategory, RedID = red$assessmentId, 
                          DBName = worms$scientificName, counter = 0, dataframe = WormsandRed)

#length(RnW) - length(WormsandRed$V1) # check we have all the data we expect! 

# Now we add the family & order data (to help with later analysis)

WormsandRed <- AddFam(DF1= WormsandRed, DFName = worms$scientificName, DFOrder = worms$order, DFFam = worms$family)
#length(RnW) - length(WormsandRed$V1) # check we have all the data we expect! 

# and how many are in RL but not Worms & vice versa? 

RednotWorms <- data.frame()
RednotWorms <- RedOnly(RedName = red$scientificName, RedCategory = red$redlistCategory, RedID = red$assessmentId, 
                       DBName = worms$scientificName, counter = 0, dataframe = RednotWorms)
length(RednotWorms$V1) # 8375

##### what is in WoRMs, but not the RedList?
WormsnotRed <- data.frame()
WormsnotRed <-NotRed(RedName = red$scientificName, DBName = worms$scientificName, 
                     DBOrder = worms$order, DBFam = worms$family, counter = 0, dataframe = WormsnotRed) 

length(WormsnotRed$V1) #  to check how many entries there are  #36359

length(worms$taxonID) - (length(WormsandRed$V1) + length(WormsnotRed$V2)) # MISSING 39 SPECIES - WHYYYY. in the WnR file. 

## look for unique values in species 
###############39 species discrepancy - whyyyyyyy

length(unique(worms$scientificNameID)) - length(worms$scientificNameID)

##################################
length(red$scientificName) - (length(RednotWorms$V1) + length(WormsandRed$V1)) # this list works

######################################################################################
######################################################################################

# let's compare the fishbase dataset 

FBnR <- intersect(red$scientificName, fb$ScientificName)
length(FBnR) # 16584 species - decent!

FBandRed <- data.frame()
FBandRed <- RedOverlap(RedName = red$scientificName, RedCategory = red$redlistCategory, 
                       RedID = red$assessmentId, DBName = fb$ScientificName, counter = 0, dataframe = FBandRed)

colnames <- c("ScientificName", "RedStatus", "RedID", "Order", "Family")
colnames(FBandRed) <-colnames


### add the family data
FBandRed <- AddFam(DF1 = FBandRed, DFName = fb$ScientificName, DFOrder = fb$Order, DFFam = fb$Family)
length(FBandRed$V1) # 16548

###### 

RednotFB <- data.frame()
RednotFB <- RedOnly(RedName = red$scientificName, RedCategory = red$redlistCategory, RedID = red$assessmentId, 
                     DBName = fb$ScientificName, counter = 0, dataframe = RednotFB   )

length(RednotFB$V1) # 1407

## and FB not red

FBnotRed <- data.frame()
FBnotRed <- NotRed(RedName = red$scientificName, DBName = fb$ScientificName, DBOrder = fb$Order, 
                   DBFam = fb$Family, counter = 0, dataframe = FBnotRed)
length(FBnotRed$V1) # 15211

## now let's do a cheeky lil' check

length(red$assessmentId) - (length(FBandRed$V1) + length(RednotFB$V1)) # 0 - all checks out!
length(fb$ScientificName) - (length(FBandRed$V1) + length(FBnotRed$V1)) # 0 - also checks out! 



###############################################################
################## finally let's check out the overlap between  the phylogenetic tree and red list 

tree <- as.data.frame(tree)
tree[,1] <- as.character(tree[,1])
tree[,2] <- 0
tree[,3] <- 0

TnR <- intersect(tree$ScientificName, red$scientificName)
length(TnR) # 16386


### Create dem lists
#############

TreeandRed <- data.frame()
TreeandRed <- RedOverlap(RedName = red$scientificName, RedCategory = red$redlistCategory, 
                       RedID = red$assessmentId, DBName = tree$ScientificName, counter = 0, dataframe = TreeandRed)

length(TreeandRed$V1) # 16386 - checks out with the intersect above :)

###### 

RednotTree <- data.frame()
RednotTree <- RedOnly(RedName = red$scientificName, RedCategory = red$redlistCategory, RedID = red$assessmentId, 
                    DBName = tree$ScientificName, counter = 0, dataframe = RednotTree   )
length(RednotTree$V1) # 1569 (before came up as 1407 - interesting!)

## and Tree not red
TreenotRed <- data.frame()
TreenotRed <- NotRed(RedName = red$scientificName, DBName = tree$ScientificName, DBOrder = tree$V2, 
                   DBFam = tree$V2, counter = 0, dataframe = TreenotRed)
length(TreenotRed$V1) # 15130

## now let's do a cheeky lil' check

length(red$assessmentId) - (length(TreeandRed$V1) + length(RednotTree$V1)) # 0 - all checks out!
length(tree$ScientificName) - (length(TreeandRed$V1) + length(TreenotRed$V1)) # 0 - also checks out! 

############################################################################################
############################################################################################
############################################################################################

#We now have several sets of data. The data we are mainly working with will be species on the
# rest list that are in the phylogenetic tree (fish tree).

# The next step is to see if there is an overlap between these species, and WoRMs and/or FishBase
# we will pull out the taxonomic data (fam & order), and we can then inspect it

######
#Step one: look at the intersect between the species in TreeandRed and WormsandRed (and WoRMS - 
# should be the same), and then FBandREd (and fb)

RedTreeandWorms.intersect <- intersect(TreeandRed$V1, WormsandRed$V1)
length(RedTreeandWorms.intersect) # 9173

RedTreeandWorms.all.intersect <- intersect(TreeandRed$V1, worms$scientificName)
length(RedTreeandWorms.all.intersect) # 9173 - good, checks out

RedTreeandWorms.finalcheck <- intersect(RedTreeandWorms.all.intersect, RedTreeandWorms.intersect)
length(RedTreeandWorms.finalcheck) # 9173 - so it's the same species that have been pulled out 

#### and now how about with fishbase!?


RedTreeandFB.intersect <- intersect(TreeandRed$V1, FBandRed$V1) 
length(RedTreeandFB.intersect) # 16320

RedTreeandFB.all.intersect <- intersect(TreeandRed$V1, fb$ScientificName)
length(RedTreeandFB.all.intersect) # 16320 - good, checks out

RedTreeandFB.finalcheck <- intersect(RedTreeandFB.all.intersect, RedTreeandFB.intersect)
length(RedTreeandFB.finalcheck) # 16320 - so it's the same species that have been pulled out 



###################  what about - species present in BOTH!?!?


every.df <- intersect(RedTreeandWorms.intersect, RedTreeandFB.intersect)
length(every.df) # 9149

### CONCLUSION: there are 24 species in Worms that aren't in FB. There are a lot more that aren't in Worms but are in FB. 

### We are now going to add in the families and genus of the species that are found in every list and have a wee look-see. 
### We need lots of cols: 
# species name
# RedID
# RedStatus
# family
# order 

# I've already added in the species and order to two. So, I can go through WoRMS and FishBase, 
# add in a "y" to species that are found in the every.df list, and then subset that dataframe by this amount.
# I am then going to add the two lists together, and check the families & orders are the same for the overlapping species. 

#worms
for (x in 1:length(WormsandRed$V1)) {
  if (WormsandRed$V1[x] %in% every.df) 
  {
    WormsandRed[x,6] <- c("Y")
  }
}

WormsandRed.subset <- subset(WormsandRed, WormsandRed$V6=="Y")
length(WormsandRed.subset$V1)
length(every.df) # THEY ARE THE SAME <3

#### and fb
for (x in 1:length(FBandRed$V1)) {
  if (FBandRed$V1[x] %in% every.df) 
  {
    FBandRed[x,6] <- c("Y")
  }
}

FBandRed.subset <- subset(FBandRed, FBandRed$V6=="Y")
colnames <- c("ScientificName", "RedStatus", "RedID", "Order", "Family", "Y")
colnames(FBandRed.subset) <-colnames
length(FBandRed.subset$ScientificName)
length(every.df) # THEY ARE THE SAME <3


#### Now, we stitch the two together to create a master, monster list
# We will use the WormsandRed df as a base 
Summary <- data.frame(matrix(ncol = 9, nrow = length(every.df)))
all.cols <- c("ScientificName", "RedStatus", "RedID", "WormsOrder", "WormsFam", "FBOrder", "FBFam", "OrderCheck", "FamCheck")
colnames(Summary) <- all.cols


Summary[,1:5] <- WormsandRed.subset[,1:5]

# unfortunately, it's lazy just to  pull in the last two columns so we will use the species in col 1 as reference for 
# the FishBase.subset, and pull it in from there

for (x in 1:length(Summary$ScientificName)) {
  ref <- match(Summary$ScientificName[x], FBandRed.subset$ScientificName)
  Summary$FBOrder[x] <-  FBandRed.subset$Order[ref]
  Summary$FBFam[x] <- FBandRed.subset$Family[ref]
}

#### now we check if the families & orders are the same within FB and Worms - if there are, then this dataframe is *useful*
match.count.order <- 0
match.no.order <- 0
for (x in 1:length(Summary$ScientificName)) {
  if (Summary$WormsOrder[x] == Summary$FBOrder[x]) {
    Summary$OrderCheck[x] <- c("match")
    match.count.order <- match.count.order + 1
  }
  else {
    Summary$OrderCheck[x] <- c("not")
    match.no.order <- match.no.order + 1
  }
}

match.count.order + match.no.order == length(Summary$ScientificName) # check all are accounted for 


#### and now with families 

match.count.fam <- 0
match.no.fam <- 0
for (x in 1:length(Summary$ScientificName)) {
  if (Summary$WormsFam[x] == Summary$FBFam[x]) {
    Summary$FamCheck[x] <- c("match")
    match.count.fam <- match.count.fam + 1
  }
  else {
    Summary$FamCheck[x] <- c("not")
    match.no.fam <- match.no.fam + 1
  }
}

match.count.fam + match.no.fam == length(Summary$ScientificName) # check all are accounted for 

###################################### SOME SUMMARY DATA

length(unique(Summary$WormsOrder)) # 43
length(unique(Summary$FBOrder)) # 43 # so, at least the number of families is the same 

merge.order <- c(Summary$WormsOrder, Summary$FBOrder)
length(merge.order)
length(unique(merge.order)) # so there are 2 extra orders. They do not contain the same 43 orders!

length(unique(Summary$WormsFam)) # 344
length(unique(Summary$FBFam)) # 342

merge.fam <- c(Summary$WormsFam, Summary$FBFam)
length(merge.fam)
length(unique(merge.fam)) # so there are 2/4 extra fams. They do not contain the same 344 orders!

#########################################################################################
## at this point it feels prudent to save down the tables we have created, so we don't
## need to re-run the whole script above before progressing

write.csv(FBandRed, file="../Data/FBandRed.csv", col.names = T)
write.csv(FBnotRed, file="../Data/FBnotRed.csv", col.names = T)
write.csv(RednotFB, file="../Data/RednotFB.csv", col.names = T)

write.csv(WormsandRed, file="../Data/WormsandRed.csv", col.names = T)
write.csv(WormsnotRed, file="../Data/WormsnotRed.csv", col.names = T)
write.csv(RednotWorms, file="../Data/RednotWorms.csv", col.names = T)

write.csv(TreeandRed, file="../Data/TreeandRed.csv", col.names = T)
write.csv(TreenotRed, file="../Data/TreenotRed.csv", col.names = T)
write.csv(RednotTree, file="../Data/RednotTree.csv", col.names = T)

write.csv(Summary, file="../Data/Summary.csv", col.names = T)


####################################################################
####################################################################


##### test this later - code from Adam
data<-read.csv("Copy of JetzTree_seabirds_TClay.csv",h=T)
matchfile<-read.csv("GBD_2019_Jan_13_passworded.csv",h=T)
#create a list of species to cross-check
Required_list<-data$Scientific
Required_list<-as.character(Required_list)
#Match required list with species matchfile, omit non-matches
c(match(Required_list, matchfile$New.Jetz_name..Tree.Tip.Name.)) ->extracted_list_PRESENT
extracted_list_PRESENT<-na.omit(extracted_list_PRESENT)
Record_subset<-matchfile[extracted_list_PRESENT,]
#Identify species not found
c(match(Required_list, matchfile$New.Jetz_name..Tree.Tip.Name.)) ->extracted_list_ABSENT
missing.numbers <- c()
for(i in 1:length(extracted_list_ABSENT)){
  if(is.na(extracted_list_ABSENT[i]) == T){
    missing.numbers <- c(missing.numbers, i)}}
missing.numbers
(missing.species <- as.character(data$Scientific[missing.numbers]))
