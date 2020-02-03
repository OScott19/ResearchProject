graphics.off()
rm(list=ls())

setwd("~/Documents/ResearchProject/Code/")

source("ImportData.R")




###############################################


list.of.fam <- unique(c(Summary$WormsFam, Summary$FBFam))

#### let's get a breakdown of family endangered species-ness, shall we?
fam.stats <- data.frame(matrix(nrow = length(list.of.fam), ncol = 5))
fam.stats.cols <- c("Family", "sppFB", "sppWorms", "endangered", "percent_endangered")
colnames(fam.stats) <- fam.stats.cols

fam.stats[,1] <- list.of.fam # add in the list of all of the families accross 

fam.table.fb <- as.data.frame(table(Summary$FBFam))
fam.table.worms <- as.data.frame(table(Summary$WormsFam))

for (i in 1:length(fam.stats$Family)) {
  ref <- match(fam.stats$Family[i], fam.table.fb[,1])
  fam.stats$sppFB[i] <- fam.table.fb[ref,2]
  
  ref2 <- match(fam.stats$Family[i], fam.table.worms[,1])
  fam.stats$sppWorms[i] <- fam.table.worms[ref2,2]
}

##### Add an "endangered" column to the Summary table

Summary[,11] <- 0


for (x in 1:length(Summary$X)) {
  if (Summary$RedStatus[x] == "Extinct" | Summary$RedStatus[x] == "Extinct in the Wild"
      | Summary$RedStatus[x] == "Endangered" | Summary$RedStatus[x] =="Critically Endangered" |
      Summary$RedStatus[x] == "Vulnerable" ) {
    Summary$V11[x] <- c("y") 
    }
}

for (x in 1:length(fam.stats$Family) {
  temp <- subset(Summary, Summary$
}


########################################
##############################################################
#########################################
# PICK BACK UP HERE!!!
# use table to find family numbers numbers 
# write for loop to subset data by family, and then count the number of endangered species
# (tabulate, and then sum the columns with the thre categories of "endangered")
# save this down and then enter it in the master table
# then do the same thing for the other database
# THEN use table to find number of species within whole family group (go back to total database)
# add to table for perspective 


FBorders <- data.frame()
FBorders <- table(FBandRed.subset$Order)

head(FBorders)
