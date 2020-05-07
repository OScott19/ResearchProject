# we are going to compre the edge 1 and edge 2 results

# Housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/EDGE2/Chondrichthyes/")


# first things first: packages
#install.packages("wesanderson")
library(wesanderson)
library(ape)
library(caper)
library(data.table)
require(geiger)
require(pez)
require(phytools)
require(phylobase)



# load in the edge 1 results

load("EDGE1_run_outputs_CH.Rdata") # only need top 50

edge1.top.50 <- top.50

# load in edge 2 results

load("Results/Ch_EDGE2_Scores.Rdata") # all edge scores 

# now get top 50 of the edge 2

edge2.top.50 <- all.edge.scores[order(all.edge.scores$meanEDGE, decreasing = T),]

edge2.top.50 <- edge2.top.50[1:50,]
cols.keep <- c(1,1002,1003)
edge2.top.50 <- edge2.top.50[,cols.keep]

edge1.top.50$rank <- 1:50
edge2.top.50$rank <- 1:50


# compare

compare <- data.frame(E1 = edge1.top.50$Species, E1rank = edge1.top.50$rank)
compare$E1 <- as.character(compare$E1)

for (i in 1:length(compare$E1)) {
  # now we add in the ranks of the E2 species 
  ref <- match(compare$E1[i], edge2.top.50$X1)
  compare$E2rank[i] <- edge2.top.50$rank[ref]
}



####

plot(x = compare$E1rank, y = compare$E2rank, 
     main = "EDGE 1 and EDGE 2 ranking of top 50 Ch spp", 
     xlab = "EDGE 1 ranking", 
     ylab = "EDGE 2 ranking", 
     col = "steelblue1", 
     frame = F)



## do the spearman test

cor.test(x = compare$E1rank, y = compare$E2rank, method = "spearman")

#######################################################

# Compare & run spearman for all species, not just top 50


load("EDGE1_run_outputs_CH.Rdata")

# rank 'all edge data' : only 

edge1.ranked <- all.edge.data[order(all.edge.data$means, decreasing = T),]
edge1.ranked <- edge1.ranked[1:684,]
edge1.ranked$rank <- 1:length(edge1.ranked$Species)


compare.all <- data.frame(Species = edge1.ranked$Species, E1rank = edge1.ranked$rank, E2rank = NA)
compare.all$Species <- as.character(compare.all$Species)
# load in edge 2

load("Results/Ch_EDGE2_Scores.Rdata") # all edge scores 

# rank them 

edge2.ranked <- all.edge.scores[order(all.edge.scores$meanEDGE, decreasing = T),]
edge2.ranked$rank <- 1:length(edge2.ranked$X1)


for (i in 1:length(compare.all$Species)) {
  # now we add in the ranks of the E2 species 
  print(i)
  ref <- match(compare.all$Species[i], edge2.ranked$X1)
  compare.all$E2rank[i] <- edge2.ranked$rank[ref]
}


###


plot(x = compare.all$E1rank, y = compare.all$E2rank, 
     main = "EDGE 1 and EDGE 2 ranking of all 608 Ch spp with both scores", 
     xlab = "EDGE 1 ranking", 
     ylab = "EDGE 2 ranking", 
     col = "steelblue1", 
     frame = F)



## do the spearman test

cor.test(x = compare.all$E1rank, y = compare.all$E2rank, method = "spearman")
