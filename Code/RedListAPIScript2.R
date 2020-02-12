#Red list API script 

# get required packages
library(rredlist)
library(jsonlite)

my.key <- c("30c68b19cedcc7f1cee81aa9b07e1cd235c49d9af8c184d76998b05e59a77c22") # this is my key <3 

setwd("~/Documents/ResearchProject/Code/")

red <- read.csv("../Data/red.csv", stringsAsFactors = F)

taxanomic.data2 <- list()
  
for (i in 1:length(red$scientificName)) {
  x <- i + 14300
  taxanomic.data2[[i]] <- jsonlite::fromJSON(rl_search_(name = red$scientificName[x], key = my.key, parse = F))
  
  if (i %% 50 == 0) {
    Sys.sleep(2)
    save(taxanomic.data2, file = "../Data/APIScriptOutput2.Rdata")
    print(x)
  }
  if (x == 17955) {
    save(taxanomic.data2, file = "../Data/APIScriptOutput2.Rdata")
    print(x)
    print(c("This is the end!"))
  }
}

