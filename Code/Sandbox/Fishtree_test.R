# This script is an exploration of the Fishtree r package, and some of the tree packages

graphics.off()
rm(list=ls())
setwd("~/Documents/ResearchProject/Code/Sandbox/")

### install required packages

#install.packages("fishtree")
#install.packages("rfishbase")
#install.packages("picante")
#install.packages("geiger")
#install.packages("ape")

# load required packages
library(fishtree)
library(rfishbase)
library(picante)
library(geiger)
library(ape) # analyses of phylogenetics and evolution

library(dplyr)
library(tidyr)
library(ggplot2)

library(rredlist)
#install.packages("caper")
library(caper)



####

#useful fishtree commands

fishtree_alignment() # get aligned sequences from the FToL
fishtree_complete_phylogeny() # get complete (stochiastically-resolved) phylogenies from FToL
fishtree_phylogeny() # get a phylogeny from the FToL
fishtree_rogues() # get rogue taxa that break the monophyly of defined taxa
fishtree_taxonomy() # get taxonomies and other data from the FToL
fishtree_tip_rates() # get tip rates for the FToL


##### playing with the package
# fish I am interested in: Class- Actinopterygii 

head(fishbase) # this is a list of all of species in the fishbase database 

# exploration of data
length(unique(fishbase$FamCode)) # 549 families
length(unique(fishbase$SpecCode)) # 33 104 species
length(unique(fishbase$Class)) # 6
unique(fishbase$Class) #  we are only interested in Actinopterygii 

## split out the data

act.data <- subset(fishbase, fishbase$Class=="Actinopterygii") # crucial: spell it correctly! 
## and now explore this new subset 

head(act.data) # check there is data there
length(unique(act.data$FamCode)) # 487 families
length(unique(act.data$SpecCode)) # 31 759 species
length(unique(act.data$Species)) # however there are only 17720 unique species names. Orgs may share a species name but have different genuses?
length(act.data$Species) # as there are 31759 species entries

# combine genus and species and then check for unique values?

# initialise new frame (required for tidyr)
gvs <- data.frame()

# this creates a new col called GS and combines genus and species
gvs <- tidyr::unite(act.data, GS, Genus, Species, sep = " ")  
head(gvs$GS)
length(gvs$GS) # 31759 entries, as expected
length(unique(gvs$GS)) # also 31759! 


# more exploration
class(fishbase) # a data frame (and also a table?)

fish.tax <- fishtree_taxonomy(rank== class)

length(fish.tax)

fish.fam <- subset(fish.tax, rank=="family")

fish.fam

unique(fish.tax$rank) # there are 8 18 different ranks of fish 
# [1] class       subclass    infraclass  megacohort  supercohort cohort      subcohort   infracohort section     subsection  division    subdivision
#[13] series      superorder  order       suborder    infraorder  family    



fishtree_phylogeny()



Fish.orders.vector <- read.csv(file="../../Data/Fish_orders.csv", header = F, sep ="", stringsAsFactors = F)
#Fish.orders.list <- as.list(Fish.orders.vector, sep="/n")


fishlist.breakdown <- read.csv(file="../../Data/Fishlist_breakdown.CSV", header = T, sep = ",", stringsAsFactors = F)
fishlist.breakdown[,2] <- as.numeric(fishlist.breakdown[,2])
fishlist.breakdown[,3] <- as.numeric(fishlist.breakdown[,3])
fishlist.breakdown <- as.data.frame(fishlist.breakdown[,-4])



#################### base plot - when I have time 



fishlist.breakdown

total.num.species <- fishlist.breakdown[2:no(fishlist.breakdown),2]

total.num.species


act.tree <- read.tree("../../Data/actinopt_full.trees")

act.tree <- act.tree[[1]]
#act.tree <- or_this

act.tree <- drop.tip(act.tree, act.tree$tip.label[grep("Hippocampus_", act.tree$tip.label)])

grep("Hippocampus_", act.tree$tip.label)

act.tree$tip.label

a <- grep("Polypterus_",act.tree$tip.label)
act.tree.2 <- drop.tip(act.tree, act.tree$tip.label[-a])

a <- getMRCA(act.tree,a)

install.packages("phylobase")
library(phylobase)
act.tree$edge.length
act.tree$edge
act.tree$Nnode


a.ed <- ed.calc(act.tree)$spp

sum(fb.ctable$TotalSpp)
half.red <- subset(fb.ctable, fb.ctable$ProportionAssessed <= 50)
sum(half.red$TotalSpp)
HALF.ENDANGERED <- subset(half.red, half.red$NoEndangeredSp > 0)

sum(HALF.ENDANGERED$TotalSpp)
