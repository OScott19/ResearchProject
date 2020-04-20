# This script compares the differences between the two chosen data sorces
# Red list and taxanomic information from the fish-tree-of-life

# This script replaces the old 'ComparisonAllData' and 'AnalysisAllData' scripts for two reasons: 
# Firstly, that was an exploratory script and does a lot of time-consuming analysis that isn't required now we know which data we're using
# Secondly, we have access to a lot more data (we have taxanomic data from all four sources now) making large parts of it redundant. 

##### Housekeeping 
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")


##### Import the two data sources we're going to be looking at

red <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)
red <- red[-1]

tree <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)
tree$scientificName <- tree$genus.species

## first thing we do is add a boolean category for whether the species is considered endangered

for (x in 1:length(red$assessmentId)) {
  if (red$redlistCategory[x] == "Extinct" | red$redlistCategory[x] == "Extinct in the Wild"
      | red$redlistCategory[x] == "Endangered" | red$redlistCategory[x] =="Critically Endangered" |
      red$redlistCategory[x] == "Vulnerable" ) {
    red$Endangered[x] <- TRUE 
  }
  
  else {
    red$Endangered[x] <- FALSE
  }
  
}


### Step one: compare
# Looking for: species that overlap, then species that are found in only one of the databases
# Then: compare the taxanomies of the species that overlap, and check their families/ order match up

# quick check
intersec <- intersect(tree$scientificName, red$scientificName)
length(intersec) # 17355 - a decent number of species, eh!

# now let's create the actual list of overlapped species
# include: species name, taxanomic info (order, family) from both databases, plus Redlist ID

intersec.red <- subset(red, red$scientificName %in% intersec)
intersec.tree <- subset(tree, tree$scientificName %in% intersec)

# trim uncessary columns
use.red <- cbind.data.frame(intersec.red$scientificName, intersec.red$redlistCategory, 
                               intersec.red$className, intersec.red$orderName, intersec.red$familyName, intersec.red$Endangered, stringsAsFactors = F)
colnames(use.red) <- c("scientificName", "red.Category", "red.Class", "red.Order", "red.Family", "Endangered")

use.tree <- cbind.data.frame(intersec.tree$scientificName, intersec.tree$class, intersec.tree$order, intersec.tree$family, stringsAsFactors = F) 
colnames(use.tree) <- c("scientificName", "tree.Class", "tree.Order", "tree.Family")


intersec.both <- c()

for (x in 1:length(intersec)) {
  temp.red <- subset(use.red, use.red$scientificName == intersec[x])
  temp.tree <- subset(use.tree, use.tree$scientificName == intersec[x])
  temp <- cbind.data.frame(temp.red, temp.tree, stringsAsFactors = F)
  intersec.both <- rbind(intersec.both, temp)
}

# Next, we create the data.frames  of the species that are only on one of the lists

red.not.tree <- subset(red, (red$scientificName %in% intersec == F))
length(red.not.tree$assessmentId)

tree.not.red <- subset(tree, (tree$scientificName %in% intersec ==F))
length(tree.not.red$superclass)

# Check that everything has been accounted for (at least at surface level)
length(red$assessmentId) - (length(intersec) + length(red.not.tree$assessmentId))
# 0 
length(tree$superclass) - (length(intersec) + length(tree.not.red$superclass))
# 0 

# this checks out

###################################################################

# next section:
# check the taxanomic data for the overlapping data: do the family & order match up? 
# order breakdown of the overlapping
# family breakdown of the overlap,
# red list categorisation of the overlapping data

intersec.both$ClassMatch <- 0
intersec.both$OrderMatch <- 0 
intersec.both$FamilyMatch <- 0

for (x in 1:length(intersec.both$scientificName)) {
  if (toupper(intersec.both$red.Class[x]) == "ACTINOPTERYGII" && intersec.both$tree.Class[x] == "Actinopteri") {
    intersec.both$ClassMatch[x] <- c("1")
  }
  if (intersec.both$red.Order[x] == toupper(intersec.both$tree.Order[x])) {
    intersec.both$OrderMatch[x] <- c("1")
  }
  if (intersec.both$red.Family[x] == toupper(intersec.both$tree.Family[x])) {
    intersec.both$FamilyMatch[x] <- c("1")
  }
}
# We regard "Actinoperygii" and "Actinoperi" as the same class for the purpose of this analysis as they are synonymns

# Let's count how many differences there are in each of the categories

class.diff <- as.numeric(length(intersec)) - sum(as.integer(intersec.both$ClassMatch))  # 11 instances
order.diff <- as.numeric(length(intersec)) - sum(as.integer(intersec.both$OrderMatch))   # 7288
fam.diff <- as.numeric(length(intersec)) - sum(as.integer(intersec.both$FamilyMatch)) # 803


## Class differences: All spp inthe family POLYPTERIDAE. Fishbase puts in Actinopterygii, as does RedList. 
# Fishtree put as Cladista, however some trees put Cladista as a sub-set of Actinopterygii
# For the purpose of this study, we are going to keep these species as matching

# when we do the family analysis, we are going to chose the fishtree ones as they align with the 

# let's have a look at what is left if we only look at groups which align in all aspects (ignoring class):

all.aligned <- subset(intersec.both, intersec.both$OrderMatch == "1")
all.aligned <- subset(all.aligned, all.aligned$FamilyMatch == "1") # 9414 species

all.aligned.familes <- as.data.frame(table(all.aligned$tree.Family), stringsAsFactors = F)
all.aligned.orders <- as.data.frame(table(all.aligned$red.Order), stringsAsFactors = F)
all.aligned.categories <- as.data.frame(table(all.aligned$red.Category), stringsAsFactors = F)

# now the same with the overlapping species - we're going to use the fishtree families
spp.table <- as.data.frame(table(intersec.both$tree.Family), stringsAsFactors = F)

# and now the base information from the fishtree

fishtree.fam.table <- as.data.frame(table(tree$family), stringsAsFactors = F)


### now create the final analysis tables that we need
# we are using the fishtree taxanomic information for the 'overall' data
coln2 <- c("Family", "TotalSpp", "NoSpeciesAssessed",  "ProportionAssessed", "NoEndangeredSp", 
           "ProportionEndangered-Assessed", "ProportionEndangered-Total")

tax.matched.table <- data.frame(matrix(ncol= length(coln2), nrow = length(fishtree.fam.table$Var1)))
colnames(tax.matched.table) <- coln2

spp.matched.table <- data.frame(matrix(ncol = length(coln2), nrow = length(fishtree.fam.table$Var1)))
colnames(spp.matched.table) <- coln2


## Col 1 and 2 are the same for both tables (family, total number of species)

tax.matched.table$Family <- fishtree.fam.table$Var1
tax.matched.table$TotalSpp <- fishtree.fam.table$Freq


spp.matched.table$Family <- fishtree.fam.table$Var1
spp.matched.table$TotalSpp <- fishtree.fam.table$Freq


# Column 3: nummber of species assessed

# tax matched table
for (x in 1:length(tax.matched.table$Family)) {
  temp <- as.numeric(match(tax.matched.table$Family[x], all.aligned.familes$Var1))
  if (is.na(temp) == F ) {
    tax.matched.table$NoSpeciesAssessed[x] <- all.aligned.familes$Freq[temp]
  }
  if (is.na(temp) == T ) {
    tax.matched.table$NoSpeciesAssessed[x] <- NA
  }
}


# spp table
for (x in 1:length(spp.matched.table$Family)) {
  temp <- as.numeric(match(spp.matched.table$Family[x],spp.table$Var1))
  if (is.na(temp) == F ) {
    spp.matched.table$NoSpeciesAssessed[x] <- spp.table$Freq[temp]
  }
  if (is.na(temp) == T ) {
    spp.matched.table$NoSpeciesAssessed[x] <- NA
  }
}

# proportion assessed

tax.matched.table$ProportionAssessed <- tax.matched.table$NoSpeciesAssessed / tax.matched.table$TotalSpp * 100

spp.matched.table$ProportionAssessed <- spp.matched.table$NoSpeciesAssessed / spp.matched.table$TotalSpp * 100

## no endangered species

# tax matched
for (x in 1:length(tax.matched.table$Family)) {
    temp <- subset(all.aligned, all.aligned$tree.Family == tax.matched.table$Family[x])
    temp2 <- subset(temp, temp$Endangered == TRUE)
    if (length(temp2$scientificName) != 0) {
    temp.endangered.count <- as.numeric(length(temp2$scientificName))
    tax.matched.table$NoEndangeredSp[x] <- temp.endangered.count
  }
  else {
    tax.matched.table$NoEndangeredSp[x] <- NA
  }
}

# species matched

for (x in 1:length(spp.matched.table$Family)) {
  temp <- subset(intersec.both, intersec.both$tree.Family == spp.matched.table$Family[x])
  temp2 <- subset(temp, temp$Endangered == TRUE)
  if (length(temp2$scientificName) != 0) {
    temp.endangered.count <- as.numeric(length(temp2$scientificName))
    spp.matched.table$NoEndangeredSp[x] <- temp.endangered.count
  }
  else {
    spp.matched.table$NoEndangeredSp[x] <- NA
  }
}


## proportion endangered - assessed

tax.matched.table$`ProportionEndangered-Assessed` <- tax.matched.table$NoEndangeredSp / tax.matched.table$NoSpeciesAssessed * 100

spp.matched.table$`ProportionEndangered-Assessed` <- spp.matched.table$NoEndangeredSp / spp.matched.table$NoSpeciesAssessed * 100

## proportion endangered - total spp

tax.matched.table$`ProportionEndangered-Total` <- tax.matched.table$NoEndangeredSp / tax.matched.table$TotalSpp * 100

spp.matched.table$`ProportionEndangered-Total` <- spp.matched.table$NoEndangeredSp / spp.matched.table$TotalSpp * 100


######################## We are now going to whittle this down into groups that might be worth looking at

tax.matched.table.edge <- subset(tax.matched.table, tax.matched.table$ProportionAssessed >= 50)
spp.matched.table.edge <- subset(spp.matched.table, spp.matched.table$ProportionAssessed >= 50)


tax.matched.table.edge <- subset(tax.matched.table.edge, tax.matched.table.edge$NoEndangeredSp >= 1)
spp.matched.table.edge <- subset(spp.matched.table.edge, spp.matched.table.edge$NoEndangeredSp >= 1)


################### LAST BUT DEFINITELY NOT LEAST: SAVE DOWN ALL OF THIS BEAUTIFUL INFORMATION


save(all.aligned, file = "../Data/TreeRed_tax_aligned_species.Rdata")
# categories, families and orders can be easily re-calculated once this data is rebootsed

save(intersec.both, file = "../Data/TreeRed_spp_aligned_species.Rdata")

save(red.not.tree, file = "../Data/RedList_notTree.Rdata")
save(tree.not.red, file = "../Data/Tree_notRedList.Rdata")

# and our results!

save(spp.matched.table, file = "../Data/Spp_matched_results.Rdata")
save(spp.matched.table.edge, file = "../Data/Spp_matched_foredge.Rdata")

save(tax.matched.table, file = "../Data/tax_matched_results.Rdata")
save(tax.matched.table.edge, file = "../Data/tax_matched_foredge.Rdata")

