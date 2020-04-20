# What this 

graphics.off()
rm(list=ls())

setwd("~/Documents/ResearchProject/Code/")

source("ImportData.R")



###############################################S

Summary <- read.csv("../Data/Summary.csv")

list.of.fam <- unique(c(Summary$WormsFam, Summary$FBFam))

#### let's get a breakdown of family endangered species-ness, shall we?
fam.stats <- data.frame(matrix(nrow = length(list.of.fam), ncol = 7))
fam.stats.cols <- c("Family", "sppFB", "sppWorms", "endangered_FB", "endangered_Worms", "percent_endangered_FB", "percent_endangered_Worms")
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


### now we want to add the total number of secies within each family group that is marked as endangered 

for (x in 1:length(fam.stats$Family)) {
  if (fam.stats$Family[x] %in% Summary$WormsFam) {
  worm.temp <- subset(Summary, Summary$WormsFam == fam.stats$Family[x])
  worm.temp <- subset(worm.temp, worm.temp$V11 == "y")
  temp.endangered.count <- length(worm.temp$V11)
  fam.stats$endangered_Worms[x] <- temp.endangered.count
  }
  
  if (fam.stats$Family[x] %in% Summary$FBFam) {
    worm.temp <- subset(Summary, Summary$FBFam == fam.stats$Family[x])
    worm.temp <- subset(worm.temp, worm.temp$V11 == "y")
    temp.endangered.count <- length(worm.temp$V11)
    fam.stats$endangered_FB[x] <- temp.endangered.count
  }
}


#######


for (x in 1:length(fam.stats$Family)) {
    fam.stats$percent_endangered_FB[x] <- as.numeric(fam.stats$endangered_FB[x] / fam.stats$sppFB[x]) * 100
    fam.stats$percent_endangered_Worms[x] <- as.numeric(fam.stats$endangered_Worms[x] / fam.stats$sppWorms[x]) * 100
}

#####
###### create global picture 

worms.fam.all <- table(worms$family)
worms.order.all <- table(worms$order)

fb.fam.all <- table(fb$Family)
fb.order.all <- table(fb$Order)


####################

check_list <- c()

for (x in 1:length(fam.stats$Family)) {
  if (fam.stats$endangered_FB[x] != fam.stats$endangered_Worms[x]) {
    print(x)
    check_list <- c(check_list, as.integer(x))
  }
}

#######################################
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
