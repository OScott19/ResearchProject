
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

phy.block.1000 <- #your phylo
  
  
  
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

save(pext.NA, file = "../Data/ValuesforNA.Rdata")
