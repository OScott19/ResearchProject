setwd("~/Documents/ResearchProject/Code/")

james <- read.csv(file = "../Data/query_result.csv", sep = ",", fill = T, stringsAsFactors = F)

worms <- read.csv(file= "../Data/Full_Actiopterygii_WoRMS_data.csv",sep = ",", fill = T, stringsAsFactors = F)

head (worms)

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

fb <- subset(fishbase, fishbase$Class=="Actinopterygii")

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

#Finally, add a column with the binomial name (useful for later comparisons)
fb <- tidyr::unite(fb, "ScientificName", Genus, Species, sep = " ")  
length(unique(fb$ScientificName)) # 31795

######## SUMMARY OF TREE DATA

treefile <- read.tree(file = "../Data/actinopt_full.trees")

tree.species <- treefile[[1]][4]

# change to a vector, as I like vectors
tree <- tree.species[[1]]

length(tree) # 31516 species on the fish tree species list

# remove underscores & tidy up bits & bobs
tree <- as.character( sub("_", " ", tree))
tree <- as.character( sub("  ", " ", tree))
tree <- as.character( sub("/t", " ", tree))
tree <- as.character( sub("/n", " ", tree))


######## SUMMARY OF RED LIST DATA

redlist.all <- read.csv("../Data/redlist_species_data_84c02514-9a98-4c0b-92de-465f9605a0e3/assessments.csv", stringsAsFactors = F)

head(redlist.all)

usefulred <- c(1,2,3,4,14)

red <- redlist.all[,usefulred]

# species
length(unique(red$scientificName)) #17955

length(unique(red$redlistCategory)) # there are 11 categories

categories <- unique(red$redlistCategory) # this is a list of all of these 11 categories

conserv.cols <- c("Category", "#species", "endangered", "%oftotal")
conserv.status <- matrix(nrow = 11, ncol = 4)
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

for (x in 1:11) {
  percent <- as.numeric(conserv.status[x,2]) / sum(as.numeric(conserv.status[,2]))*100
  conserv.status[x,4] <- round(percent, digits = 2) 
}

Sp.endangered <- subset(conserv.status, conserv.status[,3]=="Y")
sum(as.numeric(Sp.endangered[,2])) # 2520
sum(as.numeric(Sp.endangered[,4])) # 14.05%


Sp.notendangered <- subset(conserv.status, conserv.status[,3]=="N")
sum(as.numeric(Sp.notendangered[,2])) # 15445






