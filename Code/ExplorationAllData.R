setwd("~/Documents/ResearchProject/Code/")

#jamesdata <- read.csv(file = "../Data/query_result.csv", sep = ",", fill = T, stringsAsFactors = F)

worms <- read.csv(file= "../Data/Full_Actiopterygii_WoRMS_data.csv",sep = ",", fill = T, stringsAsFactors = F)

head (worms)

usefulcols <- c(7, 8, 14, 15, 16, 17)

WoRMs.use <- worms[,usefulcols]

######## SUMMARY OF WORMS DATA  
#class
length(unique(WoRMs.use$class)) # 1

#order
length(unique(WoRMs.use$order)) # 48

#family
length(unique(WoRMs.use$family)) # 426

#genus
length(unique(WoRMs.use$genus)) # 6300

#species
length(unique(WoRMs.use$scientificName)) #45629


worms.fam.table <- table(worms$family)
worms.fam.table <- as.data.frame(worms.fam.table)
worms.fam.table <- arrange(worms.fam.table, Freq)
tail(worms.fam.table, 10)


######## SUMMARY OF FISHBASE DATA 

fb <- subset(fishbase, fishbase$Class=="Actinopterygii")

#class
length(unique(fb$Class)) #1

#order
length(unique(fb$Order)) # 46

#family
length(unique(fb$Family)) # 467

#genus
length(unique(fb$GenCode)) # 4833 # use code rather than genus as specise can share genus names

#species
length(unique(fb$SpecCode)) # 31759 - here we use species code rather than species
# have the same binomial names

#Finally, add a column with the binomial name (useful for later comparisons)
fb <- tidyr::unite(fb, "ScientificName", Genus, Species, sep = " ")  
length(unique(fb$ScientificName)) # 31795
write.csv(fb, file = "../Data/fb.csv", col.names = T) # save the useful data frame down to be used later





fb.fam.table <- table(fb$Family)
fb.fam.table <- as.data.frame(fb.fam.table)
fb.fam.table <- arrange(fb.fam.table, Freq)
tail(fb.fam.table, 10)

######## SUMMARY OF TREE DATA

treefile <- read.tree(file = "../Data/actinopt_full.trees")

tree.species <- treefile[[1]][4]

# change to a vector, as I like vectors
tree <- tree.species[[1]]

length(tree) # 31516 species on the fish tree species list

# remove underscores & tidy up bits & bobs
tree <- as.character( sub("_", " ", tree))
tree <- as.character( sub("  ", " ", tree))
tree <- as.character( sub("/t", " ", tree))
tree <- as.character( sub("/n", " ", tree))

write.csv(tree, file = "../Data/tree.csv", col.names = T) # save the useful data frame down to be used later
######## SUMMARY OF RED LIST DATA

redlist.all <- read.csv("../Data/redlist_species_data_84c02514-9a98-4c0b-92de-465f9605a0e3/assessments.csv", stringsAsFactors = F)

head(redlist.all)

usefulred <- c(1,2,3,4,14)

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
