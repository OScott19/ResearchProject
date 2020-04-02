# This script is where I explore how to unwrap json files from the red list 

# Housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/Sandbox")


######## SUMMARY OF RED LIST DATA

redlist.all <- read.csv("../../Data/RedListDownload.csv", stringsAsFactors = F)

head(redlist.all)

usefulred <- c(2,3,4,5,15)

red <- redlist.all[,usefulred]


### create a list to store the results in  
# loop through each of the 'scientific names', do a rl_search for that name (parse = T, want in DF)
# store a) search name (i) b) results 

library(rredlist)
red.tax3 <- list()
red.tax4 <- list ()

library(rredlist)

for (i in 1:2000) {
  x <- i + 7000
  red.tax4[[x]] <- rl_search_(name = red$scientificName[x], key = my.key, parse = F)
  
  if (x %% 50 == 0) {
    Sys.sleep(2)
    print(c("sleep:", i))
    #write.csv(red.tax4, file = "../Data/redtaxa4.Rdata", col.names = T)
  }
}

install.packages("jsonlite")
library(tidyr)
library(dplyr)

test.list <- tax4_lunlist[1:10]
test.list

?tidyjson
install.packages("tidyjson")


test2 <- tidyjson::read_json(test.list, format = "jsonl")

json.convert.test <- as.data.frame(sapply(test.list, jsonlite::fromJSON(test.list)))

a <- jsonlite::fromJSON(rl_search_(name = red$scientificName[1], key = my.key))

b <- rl_search_(name = red$scientificName[1], key = my.key)

cc <-as.data.frame(jsonlite::fromJSON(bb))

bb <- cbind(b,b)

test3 <- red.tax4

ee <- jsonlite::fromJSON(sprintf("[%s]", paste(readLines(test3), collapse = ",")))

load("../Data/redtaxa3.Rdata")


save(red.tax4, file = "../Data/redtaxa4.Rdata", col.names = T)
save(red.tax3, file = "../Data/redtaxa3_2.Rdata", col.names = T) # save the useful list to be used later


load(file = "../Data/")


cheese <- read(file = "../Data/redtaxa3.Rdata", col.names = T)
install.packages("jsonlite")


####


write.csv(red, file = "../Data/red.csv", col.names = T) # save the useful data frame down to be used later
# species
length(unique(red$scientificName)) #17955

length(unique(red$redlistCategory)) # there are 11 categories

categories <- unique(red$redlistCategory) # this is a list of all of these 11 categories

conserv.cols <- c("Category", "#species", "endangered", "%oftotal")
conserv.status <- matrix(nrow = 11, ncol = 4)
colnames(conserv.status) <- conserv.cols
conserv.status[,1] <- categories
conserv.status[,2] <- as.numeric(0)

y <- c("Y")
n <- c("N")

conserv.status[,3] <- c(y, n, y, n, y, y, y, n, n, n, n)
conserv.status

for (i in red$redlistCategory) {
  for (x in 1:length(conserv.status[,1])) {
    if (conserv.status[x,1] == i) {
      temp <- as.numeric(conserv.status[x,2])
      temp <- temp + 1
      conserv.status[x,2] <- temp
    }
  }
}



for (x in 1:11) {
  percent <- as.numeric(conserv.status[x,2]) / sum(as.numeric(conserv.status[,2]))*100
  conserv.status[x,4] <- round(percent, digits = 2) 
}

Sp.endangered <- subset(conserv.status, conserv.status[,3]=="Y")
sum(as.numeric(Sp.endangered[,2])) # 2520
sum(as.numeric(Sp.endangered[,4])) # 14.05%


Sp.notendangered <- subset(conserv.status, conserv.status[,3]=="N")
sum(as.numeric(Sp.notendangered[,2])) # 15445

conserv.status



#### Doing some basic exploration of the phylogenetic tree file data 
FTdata <- read.csv(file = "../Data/PFC_taxonomy.csv", sep = ",", header = T, stringsAsFactors = F)
FTdata2 <- subset(FTdata, FTdata$class=="Actinopteri")

FTdata.famtable <- table(FTdata2$family)
head(FTdata.famtable)

FTdata.famtable <- order(FTdata.famtable$Freq)

library(dplyr)



length(FTdata.famtable)
FTdata.ordertable <- table(FTdata2$order)
length(FTdata.ordertable)
