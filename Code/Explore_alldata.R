# This script reads in all the data I've already sorted out and compares the results in each one 
# instead it looks 

# Housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code")

##

james <- read.csv(file = "../Data/query_result.csv", sep = ",", fill = T, stringsAsFactors = F)

worms <- read.csv(file= "../Data/Full_Actiopterygii_WoRMS_data.csv",sep = ",", fill = T, stringsAsFactors = F)

head (worms)

head(worms[,usefulcols])

usefulcols <- c(7, 8, 14, 15, 16, 17)

WoRMs.use <- worms[,usefulcols]

######## SUMMARY OF WORMS DATA  
#class
length(unique(WoRMs.use$class)) # 1

#order
length(unique(WoRMs.use$order)) # 48

#family
length(unique(WoRMs.use$family)) # 426

#genus
length(unique(WoRMs.use$genus)) # 6300

#species
length(unique(WoRMs.use$scientificName)) #45629

######## SUMMARY OF FISHBASE DATA 

fb <- act.data

#class
length(unique(fb$Class)) #1

#order
length(unique(fb$Order)) # 46

#family
length(unique(fb$Family)) # 467

#genus
length(unique(fb$GenCode)) # 4833 # use code rather than genus as specise can share genus names

#species
length(unique(fb$SpecCode)) # 31759 - here we use species code rather than species
# have the same binomial names



######## SUMMARY OF TREE DATA

treefile <- read.tree(file = "../Data/actinopt_full.trees")

tree.species <- tree[[1]][4]

# change to a vector, as I like vectors
tree <- tree.species[[1]]

length(tree.v) # 31516 species on the fish tree species list

# remove underscores & tidy up bits & bobs
tree <- as.character( sub("_", " ", tree))
tree <- as.character( sub("  ", " ", tree))
tree <- as.character( sub("/t", " ", tree))
tree <- as.character( sub("/n", " ", tree))


######## SUMMARY OF RED LIST DATA

redlist.all <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)

head(redlist.all)

usefulred <- c(2,3,4,5,15)

red <- redlist.all[,usefulred]

# species
length(unique(red$scientificName)) #17955

length(unique(red$redlistCategory)) # there are 11 categories

categories <- unique(red$redlistCategory) # this is a list of all of these 11 categories

conserv.cols <- c("Category", "#species", "endangered")
conserv.status <- matrix(nrow = 11, ncol = 3)
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
 

Sp.endangered <- subset(conserv.status, conserv.status[,3]=="Y")
sum(as.numeric(Sp.endangered[,2])) # 2520

Sp.notendangered <- subset(conserv.status, conserv.status[,3]=="N")
sum(as.numeric(Sp.notendangered[,2])) # 15445





