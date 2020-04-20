#################

# In this script we are going to create a df with the pext values for all species, 1000 times
# We will use this df when we calculate EDGE 2 using 100 trees
# We are going to use a variety of outlooks for probabilities of extincion


# housekeeping
graphics.off()
rm(list = ls())
setwd("~/Documents/ResearchProject/Code/")




#### list of pexts

pextISAAC <- c(0.025, 0.05, 0.1, 0.2, 0.4)
pext100 <- c(0.0001, 0.01, 0.1, 0.667, 0.999)
pext50 <- c(0.00005, 0.004, 0.05, 0.42, 0.97)
pext500 <- c(0.0005, 0.02, 0.39, 0.996, 1)
pextPess <- c(0.2, 0.4, 0.8, 0.9, 0.99)


pexts.df <- data.frame(ISAAC = pextISAAC, p100 = pext100, p50 = pext50, p500 = pext500, pPess = pextPess)


# load packages
library(caper) #installed
library(phytools) #installed
library(phylobase) #installed
library(data.table) #installed
library(geiger) #installed
library(pez)  #installed


######## load in the data that we need
red <- read.csv("../Data/RedListDownload.csv", stringsAsFactors = F)
tax <- read.csv("../Data/PFC_taxonomy.csv", stringsAsFactors = F)
tax$scientificName <- gsub(" ", "_", tax$genus.species)

load("../Data/UpdatedTrees/Act_tree_updated_1.Rdata")

### create df with: species, category & ge score

Species <- data.frame(matrix(ncol = 3, nrow = length(tree$tip.label)))
colnames(Species) <- c("species", "category", "GE")


Species$species <- tree$tip.label

Species$species <- gsub("_", " ", Species$species)

for (x in 1:length(Species$species)) {
  ref <- match(Species$species[x], red$scientificName)
  if (length(ref) != 0) {
    Species$category[x] <- red$redlistCategory[ref]
  } 
}

# now we replace species with the format that matches the tree
Species$species <- gsub(" ", "_", Species$species)

# now we add in the ge scores - use "add ge" function

### load in functions we need

source("EDGE_functions.R")

Species <- calculate.GE(Species)
Species$GE <- as.numeric(Species$GE)


############ now we need to create the pext values to fill the table with
treesim <- sim.bdtree(n=10000) # took this out of the loop as it takes loads of time

# setup
res.list <- list()


for (i in 1:length(pexts.df)) {
        print(i)
        iucn <- sample(1:5, size=length(treesim$tip.label), replace=TRUE)
        pext <- as.numeric(pexts.df[,i])
        data <- data.frame(species=treesim$tip.label, pext=pext[iucn])
        data <- data[order(data$pext),]
        data$rank <- seq_len(nrow(data))
        
        rank <- c(0, with(data, tapply(rank, pext, median)))
        pext <- c(0, pext)
        rank.sq <- rank^2; rank.cub <- rank^3; rank.qu <- rank^4; rank.quu <- rank^5
        model <- lm(pext ~ rank + rank.sq + rank.cub + rank.qu)
        data$rank.sq <- data$rank^2; data$rank.cub <- data$rank^3; data$rank.qu <- data$rank^4; data$rank.quu <- data$rank^5
        
        data$rank.pext <- predict(model, data)
        data$rank.pext[data$rank.pext <= 0] <- 0.0001
        
        
        #plot(data$rank,data$rank.pext)
        #plot(predict(model,data))
        
        # Assign pext values to each GE
        # These will be randomised for each GE among the species and we will draw NE/DD pexts from entire pext distribution
        
        pext.LC <- data$rank.pext[data$pext == pext[2]]
        pext.NT <- data$rank.pext[data$pext == pext[3]]
        pext.VU <- data$rank.pext[data$pext == pext[4]]
        pext.EN <- data$rank.pext[data$pext == pext[5]]
        pext.CR <- data$rank.pext[data$pext == pext[6]]
        pext.NA <- data$rank.pext[data$pext > pext[2]]
        # GE = 5 == "Extinct in the wild"
        pext.ENCR <- c(pext.EN, pext.CR) # mixture of endangered & critically endangered
        # GE ## 6 "Lower Risk/ least concern" 
        pext.NTLC <- c(pext.NT, pext.LC)
        # GE == 7 "Lower Risk/conservation dependent"
        pext.NTVUEN <- c(pext.NT, pext.VU, pext.EN)
        
        ### We allocate these pexts into the Species df
        
        Species$pext <- NA
        
        ## allocating
        
        ######### Now, we are going to run the loop above 100 times to create 100 pext values for each species
        
        # holding df
        
        pext.100 <- data.frame(matrix(ncol = 100, nrow = length(Species$species)))
        pext.100[,1] <- Species$species
        
        temp <- Species[,1:3]
        
        for (j in 1:100) {
          Species <- temp
          Species$pext <- NA
          # allocate the scores
          Species$pext[which(Species$GE == 0)] <- sample(pext.LC,length(Species$GE[which(Species$GE == 0)]),replace = T)
          Species$pext[which(Species$GE == 1)] <- sample(pext.NT,length(Species$GE[which(Species$GE == 1)]),replace = T)
          Species$pext[which(Species$GE == 2)] <- sample(pext.VU,length(Species$GE[which(Species$GE == 2)]),replace = T)
          Species$pext[which(Species$GE == 3)] <- sample(pext.EN,length(Species$GE[which(Species$GE == 3)]),replace = T)
          Species$pext[which(Species$GE == 4)] <- sample(pext.CR,length(Species$GE[which(Species$GE == 4)]),replace = T)
          Species$pext[which(Species$GE == 5)] <- sample(pext.ENCR,length(Species$GE[which(Species$GE == 5)]),replace = T)
          Species$pext[which(Species$GE == 6)] <- sample(pext.NTLC,length(Species$GE[which(Species$GE == 6)]),replace = T)
          Species$pext[which(Species$GE == 7)] <- sample(pext.NTVUEN,length(Species$GE[which(Species$GE == 7)]),replace = T)
          Species$pext[which(Species$GE == 8)] <- sample(pext.NTLC,length(Species$GE[which(Species$GE == 8)]),replace = T)
          Species$pext[which(is.na(Species$GE))] <- sample(pext.NA,length(Species$GE[which(is.na(Species$GE))]),replace = T)
          
          # now we store the score
          pext.100[,(j+1)] <- Species$pext
          
        }
        res.list[[i]] <- pext.100
        print("Saved one pext list down")
        
}


################ save 


save(res.list, file =  "../Data/100_pext_vals_5_outlooks.Rdata")

