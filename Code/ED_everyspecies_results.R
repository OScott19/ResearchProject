
## housekeeping
rm(list = ls())
graphics.off()
#setwd("../ojs19/Documents/ResearchProject/Code/")

print("Loading packages")

# load packages
library(rfishbase)
library(phylobase)
library(phytools)
library(caper)



### in this script we are going to read in the results from working out the order-wise ED of each species
### We are then going to make a table of all species with the ED scores from each 100 runs
### then we're going to create a table with min, mode, max, mean, median, standard deviation
print("Loading data")

load(file = "data100trees.Rdata")
data.set.1 <- data.100.trees
load(file = "data100trees_take2.Rdata")
data.set.2 <- data.100.trees
load(file = "data100trees_take3.Rdata")
data.set.3 <- data.100.trees


data.all <- list()
data.all <- data.set.1

for (i in 27:52) {
  data.all[[i]] <- data.set.2[[i]] }

for (i in 53:100) {
  data.all[[i]] <- data.set.3[[i]] }


results <- data.frame(matrix(ncol = 1, nrow = length(data.set.1[[1]]$order)))

results$species <- data.set.1[[1]]$genus.species

## columns are tree number
# in each column, search through the 'data all' 
# open the list, find the name in the $genus species and then find the $ed score using match
# then add the ED score to the column/ row that is associated with the list entry/genus.species

print("Creating data frame of results")

for(x in 1:100) {
  j <- x + 2
  for (i in 1:length(results$species)) {
    name <- results$species[i]
    row <-  match(name, data.all[[x]]$genus.species)
    ed <- data.all[[x]]$ED[row]
    results[i,j] <- ed
    print(x)
  }
}

print("Saving results")
save(results, file = "ED_everyspecies_results.Rdata")


print("Creating summary")

summary <- data.frame(matrix(nrow=length(results$species), ncol = 1))
summary$species <- results$species

remove <- c(1,2)
results.clean <- results[,-remove]



summary$mean <- rowMeans(results.clean)
summary$min <- apply(results.clean, 1, FUN = min)
summary$max <- apply(results.clean, 1, FUN = max)
summary$stddev <- apply(results.clean, 1, FUN = sd)

summary <- summary[,-1]

print("Saving summary")
save(summary, file = "ED_everyspecies_summary.Rdata")


print("Script has finished running.")


load("ED_everyspecies_results.Rdata")
load("ED_everyspecies_summary.Rdata")
