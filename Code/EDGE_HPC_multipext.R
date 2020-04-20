#########


# Walk through each step of the process

# Housekeeping
rm(list=ls())
graphics.off()


# packages

library(ape)
library(caper)
library(data.table)
require(geiger)
require(pez)
require(phytools)
require(phylobase)

# load up struff

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))

set.seed <- iter

print(iter)

# tree number

tn <- iter %% 100
tn <- tn + 1 # add one, as the answers will be  between 0 -99, and our trees are labelled 1-100

print(tn)

# which pext value to pick

load("100_pext_vals_5_outlooks.Rdata")

if (iter <= 100) {
  pext.100 <- res.list[[1]]
  ptype <- "Isaac"
}

if (iter > 100 && iter <= 200) {
  pext.100 <- res.list[[2]]
  ptype <- "pext100"
}

if (iter > 200 && iter <= 300) {
  pext.100 <- res.list[[3]]
  ptype <- "pext50"
}

if (iter > 300 && iter <= 400) {
  pext.100 <- res.list[[4]]
  ptype <- "pext500"
}

if (iter > 400 && iter <= 500) {
  pext.100 <- res.list[[5]]
  ptype <- "pextPessimistic"
}

# and make sure it's in the right format

pext.100$X1 <- as.character(pext.100$X1)


##############################



# load in the data that we need
tree.to.load <- paste("Trees/Act_tree_updated_", tn, ".Rdata", sep = "")

print(tree.to.load)

load(tree.to.load) # they're called 'tree'


# select the pext vals to use

pext <- data.frame(Species = as.character(pext.100$X1), pext = NA, stringsAsFactors = F)
pext$pext <- pext.100[,(tn + 1)]

# load up the ED function


ED.only.calc <- function(tree, pext){
  require(phylobase)
  require(data.table)
  # converts tree to phylo4 object
  if(!class(tree) == "phylo4"){
    tree <- as(tree, "phylo4")
  }
  names(pext) <- c("species","pext")
  plot(tree)
  # create df for data
  tree_dat <- data.frame(Species = as.character(unique(tipLabels(tree))), pext = NA)
  for(i in 1:length(tree_dat$Species)){
    tree_dat$pext[i] <- pext$pext[pext$species == tree_dat$Species[i]]
  }
  # calculate IWE and TBL for each species
  nodes <- descendants(tree, rootNode(tree), "all")
  for(i in 1:length(nodes)){
    tips <- descendants(tree, nodes[i], "tips")
    tips <- names(tips)
    tipscores <- which(pext$species %in% tips)
    tree@edge.length[which(tree@edge[,2] == nodes[i])] <- edgeLength(tree, nodes[i])*prod(pext$pext[tipscores])
    #print(paste("Node",i,"of",length(nodes),"transformed!", sep = " "))
  }
  tree <- as(tree, "phylo")
  edge.res <- list(tree_dat, tree)
  return(edge.res)
}

### use the edge function

print("Using ED function")

res <- ED.only.calc(tree, pext)

print("ED finished")

print("Saving results")

name_to_save <- paste("ED_only_HPC_24hr_", ptype, "_", tn, ".Rdata", sep = "")

save(res, file = name_to_save )

print("All done")