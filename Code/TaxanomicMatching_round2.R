# matching Chondrichthyes data from fishbase with the trees downloaded from the shark website

# housekeeping
graphics.off()
rm(list=ls())
setwd("~/Documents/ResearchProject/Code/")



library(rfishbase)
library(phylobase)
library(phytools)

library(caper) #installed

library(data.table) #installed
library(geiger) #installed
library(pez)  #installed


###################

##### Import the three data sources we're going to be looking at

red <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)
red <- red[-1]

tree <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)
tree$scientificName <- tree$genus.species

fishbase <- load_taxa() # this may be a list of everything
fishbase <- subset(fishbase, fishbase$Class == "Actinopterygii")

# nb - all binomial names have SPACES rather than "_". Leave like this for the comparison. 

# We are using the  fishbase species as the the ones that we trust
# we validate the tree species and the red list species

#########################################
#### Red & Fishbase 

fb.red  <- intersect(fishbase$Species, red$scientificName) # 17,999 species

# overlap between fishbase & species in the tree

fb.tree <- intersect(fishbase$Species, tree$scientificName) # 30,022 species

fb.val <- validate_names(fishbase$Species) # somehow I have more species here? What!

###################################
# which species are in the tree but not in fishbase? 

Treeonly <- subset(tree, (tree$scientificName %in% fishbase$Species == F))
# 1494 species: check to see if any of them have been renamed - hopefully this reduces
# when we validate the tree species & check

Redonly <- subset(red, (red$scientificName %in% fishbase$Species == F))
# 1098 species are in the red list but not in FB - hopefully this reduces
# when we validate the tree species & check


############## validate names? 

chond.red$gs <- gsub("_", " ", chond.red$scientificName)
red.val <- validate_names(chond.red$gs) # of 1124 names there are 1112 valid names, i.e. 12 invalid ones!
red.val <- gsub(" ", "_", red.val)

# let's make it so it's obvious what is replaced for what
red.validated <- data.frame(red = unique(red$scientificName), validated = NA)
red.validated$red <- as.character(red.validated$red)

for (i in 1:length(red.validated$red)) {
  print(i)
  try(red.validated$validated[i] <- validate_names(red.validated$red[i]))
}


### now we look up the names that are so wrong that can't be automatically validated
# manual validations



# so now we can create a df that has the species names that align with fishbase, and get the red list assessment for them
# However, now we need to check what's going on with the tree species 

##########

# let's make it so it's obvious what is replaced for what

tree.validated <- data.frame(tree = unique(tree$scientificName), validated = NA)
tree.validated$tree <- as.character(tree.validated$tree)

for (i in 1:length(unique(tree.validated$tree))) {
  print(i)
  try(tree.validated$validated[i] <- validate_names(tree.validated$tree[i]))
}


### now let's manually check the names that aren't matching and fill them in 

save(red.validated, tree.validated, file = "ValidatedData.Rdata")

# how many NAs are there
length(red.validated$validated) - length(unique(red.validated$validated)) # 586

# how many NAs are there
length(tree.validated$validated) - length(unique(tree.validated$validated)) # 346

length(unique(fishbase$Species)) - length(tree.validated$validated) # 1356 
# So there are <1.5k species that would need to be imputed

######################### RESTART FROM HERE   

### gone through and fixed all of the species that I can. Let's now check the overlap

tree.validated$validated <- gsub(" ", "_", tree.validated$validated)

# now let's re-check the overlap

length(intersect(tree.validated$validated, C2$scientificName)) #1165
length(tree.validated$validated) # 1192
length(unique(tree.validated$validated)) # 1116 - worth noting that we still have 10 entries that are NA


##############


c2.red.v <- intersect(C2$scientificName, red.val2) # 1102 overlap
c2.tree.v <- intersect(C2$scientificName, tree.val2) # 1162 validated

###################

# so, what species do we need to impute? Species that are found in C2 but *not* in the tree

to.impute <- subset(C2, (C2$scientificName %in% tree.validated$validated == F)) # 125 species

#####################################
#############################################

# Putting it all together

### Assuming the fishbase species list (C2) is correct:
### Remove the species from the tree that cannot be found in fishbase (the 10 'NA' species)
### Rename the species in the tree so their names match the species in fishbase
### Add in the missing species to the tree - IMPUTATION! 
### Create a df with all of the fish-base species and their associated Red List scores (or, assign an NA if they don't have one)

master.list <- data.frame(Species = C2$scientificName, RLScore = NA, TreeSpp = NA, Impute = NA)
master.list$Species <- as.character(master.list$Species)

chond.red$validated <- red.validated$validated

# add in the RL assessment

for (i in 1:length(master.list$Species)) {
  ref <- match(master.list$Species[i], chond.red$validated)
  if (length(ref) != 0) {
    master.list$RLScore[i] <- chond.red$redlistCategory[ref]
  }
}

# now add the names of that species in the tree

for (i in 1:length(master.list$Species)) {
  ref <- match(master.list$Species[i], tree.validated$validated)
  if (length(ref) != 0) {
    master.list$TreeSpp[i] <- tree.validated$tree[ref]
  }
}


master.list$Impute <- 1

for (i in 1:length(master.list$Species)) {
  if (master.list$Species[i] %in% tree.validated$validated) {
    master.list$Impute[i] <- 0
  }
}

## link up the names of the species in the tree and the names of the fishbase species



save(master.list, file = "Data/AllSpecies.Rdata")
save(red.validated, tree.validated, Redonly2, Treeonly2, to.impute, file = "TaxMatchingResults.Rdata")

