
# EDGE 2.0 pipeline - squamates

# packages
library(caper)
library(phytools)
library(phylobase)
library(data.table)
library(geiger)
library(pez)

# set working directory
setwd("~/Documents/ResearchProject/Code/")

#phy.block.1000 <- #your phylo

#phy.block.1000 <- actinopt_full.trees # load it before!

# get pext for simulated GE distributions that has median pext = to doubling
# we model the distribution of scores to get a smooth curve.
treesim <- sim.bdtree(n=10000)
iucn <- sample(1:5, size=length(treesim$tip.label), replace=TRUE)
pext <- c(0.0025, .05, .1, .2, .4)
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


plot(data$rank,data$rank.pext)
plot(predict(model,data))

save(data,pext,file = "../Data/pext_scores_for_EDGE2.RData")
#load("pext_scores_for_EDGE2.RData")



# Assign pext values to each GE
# These will be randomised for each GE among the species and we will draw NE/DD pexts from entire pext distribution
pext.LC <- data$rank.pext[data$pext == pext[2]]
pext.NT <- data$rank.pext[data$pext == pext[3]]
pext.VU <- data$rank.pext[data$pext == pext[4]]
pext.EN <- data$rank.pext[data$pext == pext[5]]
pext.CR <- data$rank.pext[data$pext == pext[6]]
pext.NA <- data$rank.pext[data$pext > pext[2]]



# Now we calculate EDGE 2.0 for each phylogeny and combine the results

# EDGE 2.0 function
# tree is a single phylo object and pext must be a dataframe with columns: 
# Species - the names of all species in the tree
# pext - the pext of each species
tree <- phy[[i]]
pext <- sp.pext
EDGE.only.calc <- function(tree, pext){
  require(phylobase)
  require(data.table)
  # converts tree to phylo4 object
  if(!class(tree) == "phylo4"){
    tree <- as(tree, "phylo4")
  }
  # create df for data
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), EDGE = NA)
  # calculate IWE and TBL for each species
  nodes <- descendants(tree, rootNode(tree), "all")
  for(i in 1:length(nodes)){
    tips <- descendants(tree, nodes[i], "tips")
    tree@edge.length[which(tree@edge[,2] == nodes[i])] <- edgeLength(tree, nodes[i])*prod(pext$pext[pext$Species %in% tips])
    print(paste("Node",i,"of",length(nodes),"transformed!", sep = " "))
  }
  for(i in 1:length(tree_dat$Species)){
    tree_dat$EDGE[i] <- sum(ancestors(tree, which(tipLabels(tree) == tree_dat$Species[i]), "ALL"), na.rm=T)
    print(paste("EDGE 2.0 calculated for species",i,"of",length(tipLabels(tree)),"!",sep=" ")) 
  }
  head(tree_dat)
  edge.res <- list(tree_dat,tree)
  return(edge.res)
}

# List to store EDGE scores across distribution of trees
EDGE.2.list <- list()

# loop through each of the 1000 trees and calculate EDGE
i <- 1
for(i in 1:length(phy.block.1000)){
  # randomly select pext scores for all species in tree based on their GE
  Species$pext <- 0
  Species$pext[which(Species$GE == 0)] <- sample(pext.LC,length(Species$GE[which(Species$GE == 0)]),replace = T)
  Species$pext[which(Species$GE == 1)] <- sample(pext.NT,length(Species$GE[which(Species$GE == 1)]),replace = T)
  Species$pext[which(Species$GE == 2)] <- sample(pext.VU,length(Species$GE[which(Species$GE == 2)]),replace = T)
  Species$pext[which(Species$GE == 3)] <- sample(pext.EN,length(Species$GE[which(Species$GE == 3)]),replace = T)
  Species$pext[which(Species$GE == 4)] <- sample(pext.CR,length(Species$GE[which(Species$GE == 4)]),replace = T)
  Species$pext[which(is.na(Species$GE))] <- sample(pext.NA,length(Species$GE[which(is.na(Species$GE))]),replace = T)
  col.num.1 <- which(colnames(Species) == "Species")
  col.num.2 <- which(colnames(Species) == "pext")
  sp.pext <- Species[,c(col.num.1,col.num.2)]
  names(sp.pext) <- c("Species","pext")
  # calculate EDGE 2
  res  <- data.frame(EDGE.2.calc(phy.block.1000[[i]],sp.pext),above.median = 0, Iteration = i)
  res$above.median[res$EDGE > median(res$EDGE)] <- 1
  EDGE.2.list[[length(EDGE.2.list)+1]] <- res[order(res$EDGE,decreasing = T),]
  print(i)
}




save(EDGE.2.list, file = "Fish_EDGE_2_list.RData")



# get summary stats for all 1,000 EDGE calculations  

EDGE.2.df <- rbindlist(EDGE.2.list)

EDGE.2.scores <- data.frame(Species = unique(EDGE.2.df$Species), 
                            IWE.median = NA, IWE.lwr.95.CI = NA,IWE.uppr.95.CI = NA, 
                            TBL.median = NA, TBL.lwr.95.CI = NA,TBL.uppr.95.CI = NA, 
                            ED.median = NA, ED.lwr.95.CI = NA,ED.uppr.95.CI = NA, 
                            EDGE.median = NA, EDGE.lwr.95.CI = NA,EDGE.uppr.95.CI = NA,
                            no.above.median = 0, EDGE.species = as.character("NO"))

EDGE.2.scores$EDGE.species <- as.character(EDGE.2.scores$EDGE.species)

# set the threshold for EDGE species - currently 95% of iterations need to be above median EDGE to be EDGE spp
threshold <- 950

for(i in 1:length(EDGE.2.scores$Species)){
  EDGE.2.scores$IWE.median[i] <- median(EDGE.2.df$IWE[EDGE.2.df$Species == EDGE.2.scores$Species[i]])
  ab <- t.test(EDGE.2.df$IWE[EDGE.2.df$Species == EDGE.2.scores$Species[i]],conf.level = 0.95)$conf.int[c(1:2)]
  EDGE.2.scores$IWE.lwr.95.CI[i] <- ab[1]
  EDGE.2.scores$IWE.uppr.95.CI[i] <- ab[2]
  EDGE.2.scores$TBL.median[i] <- median(EDGE.2.df$TBL[EDGE.2.df$Species == EDGE.2.scores$Species[i]])
  if(length(unique(EDGE.2.df$TBL[EDGE.2.df$Species == EDGE.2.scores$Species[i]])) > 1){
    ab <- t.test(EDGE.2.df$TBL[EDGE.2.df$Species == EDGE.2.scores$Species[i]],conf.level = 0.95)$conf.int[c(1:2)]
    EDGE.2.scores$TBL.lwr.95.CI[i] <- ab[1]
    EDGE.2.scores$TBL.uppr.95.CI[i] <- ab[2]
  }
  EDGE.2.scores$ED.median[i] <- median(EDGE.2.df$ED[EDGE.2.df$Species == EDGE.2.scores$Species[i]])
  if(length(unique(EDGE.2.df$ED[EDGE.2.df$Species == EDGE.2.scores$Species[i]])) > 1){
    ab <- t.test(EDGE.2.df$ED[EDGE.2.df$Species == EDGE.2.scores$Species[i]],conf.level = 0.95)$conf.int[c(1:2)]
    EDGE.2.scores$ED.lwr.95.CI[i] <- ab[1]
    EDGE.2.scores$ED.uppr.95.CI[i] <- ab[2]
  }
  EDGE.2.scores$EDGE.median[i] <- median(EDGE.2.df$EDGE[EDGE.2.df$Species == EDGE.2.scores$Species[i]])
  ab <- t.test(EDGE.2.df$EDGE[EDGE.2.df$Species == EDGE.2.scores$Species[i]],conf.level = 0.95)$conf.int[c(1:2)]
  EDGE.2.scores$EDGE.lwr.95.CI[i] <- ab[1]
  EDGE.2.scores$EDGE.uppr.95.CI[i] <- ab[2]
  EDGE.2.scores$no.above.median[i] <- sum(EDGE.2.df$above.median[EDGE.2.df$Species == EDGE.2.scores$Species[i]])
  if(EDGE.2.scores$no.above.median[i] > threshold){
    EDGE.2.scores$EDGE.species[i] <- "YES"
  }
  print(paste("EDGE statistics calculated for",i,"of",length(EDGE.2.scores$Species),"Species!",sep=" "))
}

# order dataframe by EDGE score

EDGE.2.scores <- EDGE.2.scores[order(EDGE.2.scores$EDGE.median, decreasing = T),]

EDGE.2.scores[,c(1,8,11,14,15)]

# refine EDGE species to be VU, EN, CR, EW

EDGE.2.scores$EDGE.species[which(EDGE.2.scores$Species %in% Species$Species[Species$GE < 2])] <- "NO"
EDGE.2.scores$EDGE.species[which(EDGE.2.scores$Species %in% Species$Species[is.na(Species$GE)])] <- "NO"

length(which(EDGE.2.scores$EDGE.species == "YES"))/length(EDGE.2.scores$Species)
length(which(EDGE.2.scores$no.above.median > threshold))

# save out EDGE results

EDGE.2.list.turtles <- EDGE.2.list
EDGE.2.scores.turtles <- EDGE.2.scores
EDGE.2.df.turtles <- EDGE.2.df
save(EDGE.2.df.turtles,EDGE.2.scores.turtles,EDGE.2.list.turtles,file = "EDGE_2_results_squam.RData")

# plot EDGE scores for each species sorted by median EDGE score
edge.2.boxplot <- EDGE.2.df
edge.2.boxplot$Species <- factor(edge.2.boxplot$Species,levels=EDGE.2.scores$Species)
boxplot(edge.2.boxplot$EDGE ~ edge.2.boxplot$Species,outline = F)

# get cumulative EDGE scores and plot
EDGE.2.scores$cum.EDGE <- 0

for(i in 1:length(EDGE.2.scores$cum.EDGE)){
  EDGE.2.scores$cum.EDGE[i] <- max(EDGE.2.scores$cum.EDGE) + EDGE.2.scores$EDGE.median[i]
}
EDGE.2.scores$EDGE.Rank <- c(1:length(EDGE.2.scores$Species))
EDGE.2.scores[EDGE.2.scores$cum.EDGE < 0.5*max(EDGE.2.scores$cum.EDGE),]
plot(EDGE.2.scores$cum.EDGE ~ EDGE.2.scores$EDGE.Rank)




