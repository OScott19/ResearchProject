# this script is going to calculate the expected PD loss for the entire actinopterygii tree
# we are going to use 100 trees and see what happens

# housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

###

# To calculated expected PD loss:
# 1) sum length of every branch in tree, which is the total PD of clade
# 2) multiply each branch by the product of the probabilities of extinction of each descentant species
# 3) Sum the branch lengths of the transformed tree to get the expected PD loss is all of the pexts are true
# 4) Subtract from the original tree to get the expected pd saved into the future


#######################


# We are going to do 1-4 above with one three, then modify the script for HPC to run for all 100


# Let's read in that tree!

load("../Data/one_tree.Rdata")

# 1: sum the length of every branch in the tree

totalPD <- sum(one.tree$edge.length) # 321,612 MY of evolutionary history


## read in data

tree <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)
red <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)
red$CategoryMod <- red$redlistCategory

# Remove all of the "old" ways of analysing species
red$CategoryMod[red$CategoryMod == "Data Deficient"] <- NA
red$CategoryMod[red$CategoryMod == "Lower Risk/least concern"] <- "Least Concern"
red$CategoryMod[red$CategoryMod == "Lower Risk/near threatened"] <- "Least Concern"
red$CategoryMod[red$CategoryMod == "Lower Risk/conservation dependent"] <- "Vulnerable"
red$CategoryMod[red$CategoryMod == "Extinct in the Wild"] <- "Critically Endangered"
red$CategoryMod[red$CategoryMod == "Extinct"] <- "Critically Endangered"

unique(red$CategoryMod) # All of these categories can now be assigned scores


save(red, file = "../Data/Red_Categorymod.Rdata") # save this down - this can be used going forward




## create species/ PExt

pext <- data.frame(matrix(ncol = 3, nrow = length(tree$superclass)))
colnames(pext) <- c("Species", "category", "pext")

pext$category <- NA
pext$Species <- tree$genus.species

for (x in 1:length(pext$Species)) {
  ref <- match(pext$Species[x], red$scientificName)
  if (length(ref) != 0) {
    pext$category[x] <- red$CategoryMod[ref] # use the modified categories, or the changes above are pointless!
  } 
}

pext$pext <- NA

# add pext scores

scores <- data.frame(matrix(ncol = 2, nrow = length(unique(red$CategoryMod))))
colnames(scores) <- c("category", "score")
scores$category <- unique(red$CategoryMod)
scores$score <- c(0.4, NA, 0.1, 0.025, 0.2, 0.05)

for (i in 1:length(pext$pext)) {
  ref <- match(pext$category[i], scores$category)
  pext$pext[i] <- scores$score[ref]
}

save(pext, file = "../Data/pext_for_HPC.Rdata")

##################

# now we need to run the code to fill in the missing values (EDGE_PD_LOSS_NONNY_v2.R)

load(file = "../Data/ValuesforNA.Rdata")

pext$pext[which(is.na(pext$category))] <- sample(pext.NA, length(pext$category[which(is.na(pext$category))]), replace = T)

pext$scientificName <- pext$Species

pext$Species <- gsub(" ", "_", pext$scientificName)


# Now we save down this pext file - we will use it to run EDGE 2 laterr

save(pext, file = "../Data/Sp_pext_ready.Rdata")




# get trees in the right order
tree.phylo <- one.tree
tree.phylo4 <- as(one.tree, "phylo4")


store <- as.data.frame(one.tree$edge)
colnames(store) <- c("start", "end")
store$length <- one.tree$edge.length
store$multiplier <- 1

# starting with node 1: only need to do this with edge tips

#one.tree$tip.label[1]

# find the multiplier for this node: 
ref <- as.numeric(match(one.tree$tivp.label[1], pext$Species))
p.ext <- pext$pext[ref] # here it's 0.0025

ancestors <- ancestors(phy = tree.phylo4, node = 1, type = "all")
ancestors <- c(ancestors, 1)

temp <- subset(store, store$start %in% ancestors)  
temp <- subset(temp, temp$end %in% ancestors)
length(temp$start) == (length(ancestors) - 1) # perfect

# now - we add the multiplier to the "multiplier" column 

temp$multiplier <- temp$multiplier * p.ext

##### however, need to do this for all things- so can't just be editing a subset, need to edit full list

for (x in 1:length(store$start)) {
  if (store$start[x] %in% ancestors && store$end[x] %in% ancestors) {
    store$multiplier[x] <- store$multiplier[x] * p.ext
  }
}


##### Put this in a big loop for everyone to deal with 


###################### FROM RIKKI: for allocating p(ext) for groups that don't have a pext

############### RUN THE NONNY_2 CODE TO GENERATE THE PEXT 

#CANGE:
#pext.NA <- data$rank.pext
#TO:
#pext.NA <- data$rank.pext[data$pext > pext[2]]

#THEN ASSIGN IT HERE
#Species$pext[which(is.na(Species$GE))] <- sample(pext.NA,length(Species$GE[which(is.na(Species$GE))]),replace = T)



