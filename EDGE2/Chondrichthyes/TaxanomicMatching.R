# matching Chondrichthyes data from fishbase with the trees downloaded from the shark website

# housekeeping
graphics.off()
rm(list=ls())
setwd("~/Documents/EDGE2/Chondrichthyes/")



library(rfishbase)
library(phylobase)
library(phytools)

library(caper) #installed

library(data.table) #installed
library(geiger) #installed
library(pez)  #installed


fb <- fishbase
classes <- unique(fb$Class) 
# the ones are want are Elasmobranii (shark), Holocephali (teeth fish?), 

# we dont want Actinopterigyii (ray finned fish), Myxini (hagfish) or Cephalaspidomorphi(these are either considerred extinct or Lamphreys), 
# Sarcopterygii (colocants & hagfish)

classes.use <- classes[2:3]

Sharks <- subset(fb, fb$Class %in% classes.use) # 1212 species!
Sharks$scientificName <- paste(Sharks$Genus, Sharks$Species, sep = "_")

library(rfishbase)
rays <- species_list(Class == classes.use[1])
available_releases()

##################### now read in the red list data

chond.red <- read.csv("Data/Cond_RedListDownload.csv", stringsAsFactors = F)

#################### read in the shark tree data

## species info : 

tax.species <- read.csv("Data/Species.list.csv", stringsAsFactors = F)

ts <- gsub("_", " ", tax.species$Species_list)

########################################################################################
### how many species are the same? 

## we are taking fishbase as the authority on what the species are (the tree may need to be updated)

# overlaps between fishbase ("Sharks") and the red list
chond.red$scientificName <- gsub(" ", "_", chond.red$scientificName)

Overlap.red <- intersect(Sharks$scientificName, chond.red$scientificName) # 983 species

# overlap between fishbase & species in the tree

Overlap.tree <- intersect(Sharks$scientificName, tax.species$Species_list) # 1121

length(Sharks$SpecCode) # 1212 total species

length(Overlap.red) # 938 appear on the red list

length(Overlap.tree) #  and 1121 are in t   he tree!

###################################
# which species are in the tree but not in fishbase? 

Treeonly <- subset(tax.species, (tax.species$Species_list %in% Sharks$scientificName == F))
# 71 species: check to see if any of them have been renamed

Redonly <- subset(chond.red, (chond.red$scientificName %in% Sharks$scientificName == F))
# 141 species are in the red list but not in FB - check to see if these have been renamed

both <- intersect(Treeonly$Species_list, Redonly$scientificName) # and 30 of these species are found in both (but not fishbase)

#################
#################


########## now compare with C2master.list$tree.spp <- NAmaster.list$tree.spp <- NA



b <- load_taxa() # this may be a list of everything
c <- species_list() # this is also a list of all of the species - same length as the tibble above

unique(b$SuperClass)

classes <- unique(b$Class)

C2 <- subset(b, b$Class %in% classes.use) # 1290. Interesting. 
# lets use C2 - there are more species included
C2$scientificName <- gsub(" ", "_", C2$Species)

c2.red  <- intersect(C2$scientificName, chond.red$scientificName) # 1070 spp

# overlap between fishbase & species in the tree

C2.tree <- intersect(C2$scientificName, tax.species$Species_list) # 1043

C2.val <- validate_names(C2$Species) # so, all of the names in this list are valid! That's good :) 


###################################
# which species are in the tree but not in fishbase? 

Treeonly2 <- subset(tax.species, (tax.species$Species_list %in% C2$scientificName == F))
# 149 species: check to see if any of them have been renamed

Redonly2 <- subset(chond.red, (chond.red$scientificName %in% C2$scientificName == F))
# 54 species are in the red list but not in FB - check to see if these have been renamed

both2 <- intersect(Treeonly2$Species_list, Redonly2$scientificName) # and 32 of these species are found in both (but not fishbase)


############## validate names? 

chond.red$gs <- gsub("_", " ", chond.red$scientificName)
red.val <- validate_names(chond.red$gs) # of 1124 names there are 1112 valid names, i.e. 12 invalid ones!
red.val <- gsub(" ", "_", red.val)

# let's make it so it's obvious what is replaced for what
red.validated <- data.frame(red = unique(chond.red$gs), validated = NA)
red.validated$red <- as.character(red.validated$red)

for (i in 1:length(red.validated$red)) {
  try(red.validated$validated[i] <- validate_names(red.validated$red[i]))
}


### now we look up the names that are so wrong that can't be automatically validated
# manual validations
red.validated$validated[58] <- "Raja_velezi" # yes, included in C2
red.validated$validated[127] <- "Raja_ackleyi"# this is the name used in fishbase
red.validated$validated[176] <- "Raja_bahamensis"
red.validated$validated[251] <- "Raja_eglanteria"
red.validated$validated[266] <- "Raja_texana"
red.validated$validated[271] <- "Raja_equatorialis"
red.validated$validated[458] <- "Himantura_schmardae"
red.validated$validated[582] <- "Raja_cervigoni"
red.validated$validated[889] <- "Rhynchobatus_immaculatus"
red.validated$validated[898] <- "Hemitrygon_bennettii"
red.validated$validated[922] <- "Fluvitrygon_oxyrhynchus"
red.validated$validated[934] <- "Urogymnus_lobistoma"
red.validated$validated[957] <- "Himantura_randalli"
red.validated$validated[1123] <- "Stegostoma_fasciatum"
# now, all of the species in the red list have names that match fishbase :) 

red.validated$validated <- gsub(" ", "_", red.validated$validated)


# checker
x = 957
red.validated$validated[x] %in% C2$scientificName

# 
length(intersect(red.validated$validated, C2$scientificName)) # 1115 overlap
length(unique(red.validated$validated)) # 1115 - so 100% of the species list are validated! WOOHOO!
length(unique(red.validated$red)) # 1124 


# so now we can create a df that has the species names that align with fishbase, and get the red list assessment for them
# However, now we need to check what's going on with the tree species 

##########

# let's make it so it's obvious what is replaced for what

tax.species$name <- gsub("_", " ", tax.species$Species_list)
tree.validated <- data.frame(tree = unique(tax.species$name), validated = NA)
tree.validated$tree <- as.character(tree.validated$tree)

for (i in 1:length(unique(tree.validated$tree))) {
  try(tree.validated$validated[i] <- validate_names(tree.validated$tree[i]))
}


### now let's manually check the names that aren't matching and fill them in 

tree.validated$validated[147] <- "Mobula_mobular"
tree.validated$validated[1115] <- NA # unknooooooooown
tree.validated$validated[1111] <- NA  # who bloody knows
tree.validated$validated[729] <- NA # isn't a defined species
tree.validated$validated[666] <- "Torpedo_alexandrinsis"
tree.validated$validated[688] <- NA # cna't fine 
tree.validated$validated[640] <- NA # can't find
tree.validated$validated[628] <- NA # can't find
tree.validated$validated[577] <- "Rhinobatos_spinosus"
tree.validated$validated[552] <- "Dipturus_argentinensis"
tree.validated$validated[532] <- "Raja_ackleyi" # not certain
tree.validated$validated[516] <- "Dipturus_batis"
tree.validated$validated[504] <- NA
tree.validated$validated[498] <- NA
tree.validated$validated[425] <- "Dipturus_intermedius"
tree.validated$validated[214] <- "Rhinoptera_jayakari"
tree.validated$validated[213] <- "Rhinoptera_marginata"
tree.validated$validated[208] <- NA
tree.validated$validated[171] <- NA # "Myliobatis_rhombus"

x <- 171
tree.validated$validated[x] %in% C2$scientificName


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

