## get the habitats of the family groups that we're interested in! 
rm(list=ls())
graphics.off()

# get required packages
library(rredlist)

my.key <- c("30c68b19cedcc7f1cee81aa9b07e1cd235c49d9af8c184d76998b05e59a77c22") # this is my key <3 

setwd("~/Documents/ResearchProject/Code/")

fb.ctable$Family <- as.character(fb.ctable$Family)

### create the vector of family list

a <- subset(fb.ctable, fb.ctable$ProportionAssessed >= 90)
b <- subset(a, a$NoEndangeredSp != 0)
c <- subset(b, b$TotalSpp >= 10)

families <- c()
families <- c$Family

test <- list()

test <- rl_habitats(families[5], key = my.key)

# not going to work  - you have to search for species, can't use family groups. 
# next suggestion: MORE TABLES! 
# using entire RL databse, subset for each family group, and then produce a table for the habitat column
# string all of these columns together to create a table
# include a proportion in reefs col
# col 1: family name 
# col 2:n number of sp present per habitat (each col representing a new habitat)
# col n+1: proportion of species that live in a reef setting


load(file = "../Data/APIScriptOutput.Rdata")

c.names <- colnames(taxanomic.data[[1]]$result)
c.names <- c("spname", c.names)

length(taxanomic.data[[1]]$result)
taxanomic.data[[2]]$name

test.db <- data.frame(matrix(nrow = 10, ncol= 31))
colnames(test.db) <- c.names

for (x in 1:2) {
  test.db[x][1] <- taxanomic.data[[x]]$name
  for (cols in 1:(length(taxanomic.data[[1]]$result)+1)) {
    test.db[x][(cols + 1)] <- taxanomic.data[[x]]$result[cols]
  }

}


