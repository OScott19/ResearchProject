# This script reads in all the data I've already sorted out and compares the results in each one 

# Housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code")

## LOOKING AT WORMS DATA

worms <- read.csv(file= "../Data/Full_Actiopterygii_WoRMS_data.csv",sep = ",", fill = T, stringsAsFactors = F)

head (worms)

head(worms[,usefulcols])

usefulcols <- c(7, 8, 14, 15, 16, 17)

WoRMs.use <- worms[,usefulcols]

######## SUMMARY OF WORMS DATA  
#class
length(unique(WoRMs.use$class)) # 1

#order
length(unique(WoRMs.use$order)) # 44

#family
length(unique(WoRMs.use$family)) # 416

#genus
length(unique(WoRMs.use$genus)) # 3279

#species
length(unique(WoRMs.use$scientificName)) #17613

######## SUMMARY OF FISHBASE DATA 

library(rfishbase)
fishbase.data <- fishbase
fb <- subset(fishbase.data, fishbase.data$Class == "Actinopterygii")

#class
length(unique(fb$Class)) #1

#order
length(unique(fb$Order)) # 46

#family
length(unique(fb$Family)) # 487

#genus
length(unique(fb$GenCode)) # 4833 # use code rather than genus as specise can share genus names

#species
length(unique(fb$SpecCode)) # 31759 - here we use species code rather than species
# have the same binomial names



######## SUMMARY OF DATA FROM FISHTREE 

fishtreetax.all <- read.csv(file = "../Data/PFC_taxonomy.csv", stringsAsFactors = F)

#class
length(unique(fishtreetax.all$class)) #1
unique(fishtreetax$class)

# subset to get rid of the second class
fishtreetax <- subset(fishtreetax.all, fishtreetax.all$class == "Actinopteri")

write.csv(fishtreetax, file = "../Data/FishTree_Actinopterygii.csv", sep = ",")

#order
length(unique(fishtreetax$order)) # 67

#family
length(unique(fishtreetax$family)) # 489

#genus
length(unique(fishtreetax$genus)) # 4823 # use code rather than genus as specise can share genus names

#species
length(unique(fishtreetax$genus.species)) # 31759 - here we use species code rather than species
# have the same binomial names

##### and how about in the phylogenetic trees?

library(phytools) # need this to read in trees

load(file = "../Data/one_tree.Rdata") # load in one tree

tree.species <- one.tree[4]

# change to a vector, as I like vectors
tree <- as.vector(tree.species[[1]])

length(tree) # 31516 species on the fish tree species list

# remove underscores & tidy up bits & bobs
tree <- as.character( sub("_", " ", tree))
tree <- as.character( sub("  ", " ", tree))
tree <- as.character( sub("/t", " ", tree))
tree <- as.character( sub("/n", " ", tree))

save(tree, file = "../Data/Tree_speciesList.Rdata") # save down the species list 

######## SUMMARY OF RED LIST DATA

redlist.all <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)

head(redlist.all)

usefulred <- c(2,3,4,5,15,27,28,29)

red <- redlist.all[,usefulred]

# species
length(unique(red$scientificName)) #19097

length(unique(red$redlistCategory)) # there are 11 categories

categories <- unique(red$redlistCategory) # this is a list of all of these 11 categories

conserv.cols <- c("Category", "#species", "endangered")
conserv.status <- data.frame(matrix(nrow = length(categories), ncol = 3))
colnames(conserv.status) <- conserv.cols
conserv.status[,1] <- categories
conserv.status[,2] <- as.numeric(0)

y <- c("Y")
n <- c("N")

conserv.status[,3] <- c(y, n, y, n, y, y, y, n, n, n, n)
conserv.status

for (i in red$redlistCategory) {
  for (x in 1:length(conserv.status[,1])) {
    if (conserv.status[x,1] == i) {
      temp <- as.numeric(conserv.status[x,2])
      temp <- temp + 1
      conserv.status[x,2] <- temp
    }
  }
}

save(conserv.status, file = "../Data/RedList_status_actinopterygii.Rdata")

Sp.endangered <- subset(conserv.status, conserv.status[,3]=="Y")
sum(as.numeric(Sp.endangered[,2])) # 2569

Sp.notendangered <- subset(conserv.status, conserv.status[,3]=="N")
sum(as.numeric(Sp.notendangered[,2])) # 16528

#### run this again with the fishtree & redlist overlap 



