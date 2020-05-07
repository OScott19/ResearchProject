  # EDGE 2.0 pipeline

# packages
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)

# set working directory
setwd("/Users/gumbs.r/OneDrive - Zoological Society of London/EDGE 2/")
# Read in species data. These are in four columns with the following headings:
# "Family" - the taxonomic family of each species
# "Species" - Reference taxonomy name for all species to be included in analysis
# "Tree.spp" - Name of reference species in the tree (including "_" rather than space for the split in Genus_species). If the 
# reference name is not already in the tree, this should be NA
# "GE" - the GE of the species, from 0 (LC) to 4 (CR and EW). EX should not be listed as valid extant species and
# should be dropped from the analysis. Species listed as DD or with no RL data should be listed as NA

Species <- read.csv("turtle_tax_for_EDGE_2_calculations.csv",stringsAsFactors = F)
head(Species)
# Now load in the phylogeny to be used for the EDGE list in newick format
#phy <- crocs
phy <- read.tree("turtle_tree_Pereira et al 2017.txt")

# phy is treated as a list for easy indexing
phy <- list(phy)

# if consensus tree, make list of 1000
if(length(phy) == 1){
  while (length(phy) < 10000) {
    phy[[length(phy)+1]] <- phy[[1]]
  }
}

tree.samp <- sample(1:length(phy),1000,replace = F)

phy.spare <- phy[sample(c(1:length(phy))[-tree.samp],100,replace = F)]

phy <- phy[tree.samp]



# change the names of the tips to the correct names and drops invalid / extinct species
# this can mess with the ultrametric nature so needs ultrametricising once done

for(i in 1:length(phy)){
  phy[[i]] <- drop.tip(phy[[i]], phy[[i]]$tip.label[!phy[[i]]$tip.label %in% Species$Tree.spp])
  phy[[i]] <- force.ultrametric(phy[[i]],method = "extend")
  for(j in 1:length(phy[[i]]$tip.label)){
    phy[[i]]$tip.label[j] <- Species$Species[which(Species$Tree.spp == phy[[i]]$tip.label[j])]
  }
  print(i)
}

# same for the spare phylos

for(i in 1:length(phy.spare)){
  phy.spare[[i]] <- drop.tip(phy.spare[[i]], phy.spare[[i]]$tip.label[!phy.spare[[i]]$tip.label %in% Species$Tree.spp])
  phy.spare[[i]] <- force.ultrametric(phy.spare[[i]],method = "extend")
  for(j in 1:length(phy.spare[[i]]$tip.label)){
    phy.spare[[i]]$tip.label[j] <- Species$Species[which(Species$Tree.spp == phy.spare[[i]]$tip.label[j])]
  }
  print(i)
}

# need to insert missing species into the tree on genus affiliation. If no congeners are in the tree, species are
# inserted based on familial relationships

# Concatenate the "Family" column with the species name for family-level imputation
# e.g. the snake "Crotalus_horridus" from the family "Viperidae" would now have the name "Viperidae | Crotalus_horridus"

Species$New.name <- NA
Species$New.name.2 <- NA
Species$Genus <- unlist(strsplit(Species$Species,"_"))[seq(1,length(Species$Species)*2,2)]

for(i in 1:length(Species$Family)){
  Species$New.name[i] <- paste(Species$Family[i],"|",Species$Species[i],sep="")
  Species$New.name.2[i] <- paste("Clade~",Species$New.name[i],sep="")
}

head(Species)

# identify which families aren't in the tree
phy.gen <- unlist(strsplit(phy[[1]]$tip.label,"_"))[seq(1,length(phy[[1]]$tip.label)*2,2)]

mis.gen <- unique(Species$Genus[!Species$Genus %in% phy.gen])

mis.fam <- data.frame(Family = Species$Family[!Species$Family %in% Species$Family[Species$Genus %in% phy.gen]],
                      Species = Species$Species[!Species$Family %in% Species$Family[Species$Genus %in% phy.gen]])

u.mis.fam <- as.character(unique(mis.fam$Family))

if(length(u.mis.fam) > 0){
  for(i in 1:length(phy)){
    for(j in 1:length(phy[[i]]$tip.label)){
      phy[[i]]$tip.label[j] <- Species$New.name.2[which(Species$Species == phy[[i]]$tip.label[j])]
    }
    for(j in 1:length(u.mis.fam)){
      spp.to.insert <- as.character(mis.fam$Species[mis.fam$Family == u.mis.fam[j]][sample(1:NROW(mis.fam$Species[mis.fam$Family == u.mis.fam[j]]),1)])
      spp.to.insert <- Species$New.name.2[Species$Species == spp.to.insert]
      while (!spp.to.insert %in% phy[[i]]$tip.label) {
        phy[[i]] <- congeneric.impute(phy[[i]],spp.to.insert,split = "~")
      }
    }
    for(j in 1:length(phy[[i]]$tip.label)){
      phy[[i]]$tip.label[j] <- Species$Species[which(Species$New.name.2 == phy[[i]]$tip.label[j])]
    }
  }
}

# same for the spare phylos
if(length(u.mis.fam) > 0){
  for(i in 1:length(phy.spare)){
    for(j in 1:length(phy.spare[[i]]$tip.label)){
      phy.spare[[i]]$tip.label[j] <- Species$New.name.2[which(Species$Species == phy.spare[[i]]$tip.label[j])]
    }
    for(j in 1:length(u.mis.fam)){
      spp.to.insert <- as.character(mis.fam$Species[mis.fam$Family == u.mis.fam[j]][sample(1:NROW(mis.fam$Species[mis.fam$Family == u.mis.fam[j]]),1)])
      spp.to.insert <- Species$New.name.2[Species$Species == spp.to.insert]
      while (!spp.to.insert %in% phy.spare[[i]]$tip.label) {
        phy.spare[[i]] <- congeneric.impute(phy.spare[[i]],spp.to.insert,split = "~")
      }
    }
    for(j in 1:length(phy.spare[[i]]$tip.label)){
      phy.spare[[i]]$tip.label[j] <- Species$Species[which(Species$New.name.2 == phy.spare[[i]]$tip.label[j])]
    }
  }
}

# which genera aren't in tree and insert a species

phy.gen <- unlist(strsplit(phy[[1]]$tip.label,"_"))[seq(1,length(phy[[1]]$tip.label)*2,2)]

mis.gen <- data.frame(Genus = Species$Genus[!Species$Genus %in% phy.gen],
                      Species = Species$Species[!Species$Genus %in% phy.gen])

u.mis.gen <- as.character(unique(mis.gen$Genus))

if(length(u.mis.gen) > 0){
  for(i in 1:length(phy)){
    for(j in 1:length(phy[[i]]$tip.label)){
      phy[[i]]$tip.label[j] <- Species$New.name[which(Species$Species == phy[[i]]$tip.label[j])]
    }
    j <- 1
    for(j in 1:length(u.mis.gen)){
      spp.to.insert <- as.character(mis.gen$Species[mis.gen$Genus == u.mis.gen[j]][sample(1:NROW(mis.gen$Species[mis.gen$Genus == u.mis.gen[j]]),1)])
      spp.to.insert <- Species$New.name[Species$Species == spp.to.insert]
      while (!spp.to.insert %in% phy[[i]]$tip.label){
        phy[[i]] <- congeneric.impute(phy[[i]],spp.to.insert,split = "|")
      }
    }
    for(j in 1:length(phy[[i]]$tip.label)){
      phy[[i]]$tip.label[j] <- Species$Species[which(Species$New.name == phy[[i]]$tip.label[j])]
    }
  }
}

# same for the spare phylos
if(length(u.mis.gen) > 0){
  for(i in 1:length(phy.spare)){
    for(j in 1:length(phy.spare[[i]]$tip.label)){
      phy.spare[[i]]$tip.label[j] <- Species$New.name[which(Species$Species == phy.spare[[i]]$tip.label[j])]
    }
    j <- 1
    for(j in 1:length(u.mis.gen)){
      spp.to.insert <- as.character(mis.gen$Species[mis.gen$Genus == u.mis.gen[j]][sample(1:NROW(mis.gen$Species[mis.gen$Genus == u.mis.gen[j]]),1)])
      spp.to.insert <- Species$New.name[Species$Species == spp.to.insert]
      while (!spp.to.insert %in% phy.spare[[i]]$tip.label) {
        phy.spare[[i]] <- congeneric.impute(phy.spare[[i]],spp.to.insert,split = "|")
      }
    }
    for(j in 1:length(phy.spare[[i]]$tip.label)){
      phy.spare[[i]]$tip.label[j] <- Species$Species[which(Species$New.name == phy.spare[[i]]$tip.label[j])]
    }
  }
}

# which species aren't in the tree and insert


mis.spp <- Species$Species
mis.spp[!mis.spp %in% phy[[1]]$tip.label]

i <- 1

if(length(mis.spp) > 0){
  for(i in 1:length(phy)){
    for(j in 1:10){
      if(length(phy[[i]]$tip.label) < length(mis.spp)){
        print(paste("Attempt",j,"of 10 to impute all species for tree",i,":",length(mis.spp)-length(phy[[i]]$tip.label),"species missing",sep=" "))
        phy[[i]] <- congeneric.impute(phy[[i]],mis.spp,sep="_")
      }
    }
    if(length(phy[[i]]$tip.label) < length(mis.spp)){
      print(paste("Attempts failed. Now replacing tree",i,"and attempting again",sep=" "))
      phy[[i]] <- phy.spare[[sample(1:length(phy.spare),1)]]
      for(j in 1:10){
        if(length(phy[[i]]$tip.label) < length(mis.spp)){
          print(paste("Attempt",j,"of 10 to impute all species for tree",i,":",length(mis.spp)-length(phy[[i]]$tip.label),"species missing",sep=" "))
          phy[[i]] <- congeneric.impute(phy[[i]],mis.spp,sep="_")
        }
      }
    }
    if(length(phy[[i]]$tip.label) < length(mis.spp)){
      stop(paste("Failed to impute all species - please replace tree ",i," and try again!"))
    }
    print(paste(i,"of",length(phy),sep = " "))
  }
}



# We only want 1,000 trees for the EDGE analysis. So we select a random sample of 1,000 from the large distribution
# for the EDGE analysis and save our 1,000 trees

phy.block.1000 <- phy
#save(phy.block.1000, file = "1000_full_phylos_turtles_19-01-20.RData")
#load("1000_full_phylos_turtles_19-01-20.RData")


