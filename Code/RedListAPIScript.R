#Red list API script - this script calls the Red List API and extracts all of the information about the species
# This is because the data downloaded from the website doesn't include taxanomic information - which is what we need for taxanomic matching
# Run this script in the background/ from a terminal as it can take a very long time (depending on how many species' data is required.)

# Housekeeping 
rm(list = ls())
graphicsoff()

# get required packages
library(rredlist)
library(jsonlite)

# this is my key from the IUCN red list - needed to access the API
my.key <- c("30c68b19cedcc7f1cee81aa9b07e1cd235c49d9af8c184d76998b05e59a77c22") # this is my key <3 

# Set the working directory
setwd("~/Documents/ResearchProject/Code/")

# this csv is the result of the downloading the 'Actinopterygii' search from the IUCN website
# Contains species names, IUCN category, IDs, and population trend
red <- read.csv("../Data/red.csv", stringsAsFactors = F)

taxanomic.data <- list()
  
for (i in 1:length(red$scientificName)) {
  taxanomic.data[[i]] <- jsonlite::fromJSON(rl_search_(name = red$scientificName[i], key = my.key, parse = F))
  if (i %% 50 == 0) {
    Sys.sleep(2) # built-in pauses - this is because the API sometimes gets overwhelmed and boots people out
    save(taxanomic.data, file = "../Data/APIScriptOutput.Rdata")
    print(x)
  }
}

