# This scipt opens the data downloaded from the WoRMS database
# it reads it into R, screens it for the correct data (i.e. only the Actiopterygii) and then re-saves down the relevant information

### housekeeping
rm(list=ls())
graphics.off()
setwd("~/Documents/CMEECourseWork/ResearchProject/Code/")

##### THIS FILE IS AN ABSOLUTE NIGHTMARE
## DIMENSIONS:  approximately 500,000 rows * 32 cols
## The first couple of rows are a complete mess, so skip them. I reccomend skipping 50, but I'm tired. Could explore more. 
## Whenever a spp has been published in a paper, then the 'sep' function mucks up, and it is spread over multiple rows. 
## This may need manual fixing - or maybe I can just cut them all. 
# nrows = 496,984

# Step one: count the number of rows
fields <- count.fields(file="../Data/WoRMS2/taxon.txt", sep = "\t")
no.rows <- as.numeric(length(fields))

#################### EXPLORATION 

# this is to find the col names (can't do headers when you skip)
a <- read.table(file="../Data/WoRMS/taxon.txt", nrows = 1, fill = T, sep = "\t", stringsAsFactors = F)
no.cols <- as.numeric(length(a))

# this is a test- read in the first 100 rows (after the skip period)
d <- read.table(file="../Data/WoRMS/taxon.txt", nrows = 100, skip = 50, header = T, 
                fill = T, sep = "\t", stringsAsFactors = F)

# we then add the col names to the database, which can be explored!
colnames(d) <- a



#### created a loop to try and read in all of the columns in batches of 50k(so we don't freak out )
temp <- data.frame()
total <- data.frame(matrix(ncol = no.cols))
colnames(total) <- a

# we need to split the data into ten batches - so we need to make sure that the data is perfectly divided by 10

leftover <- no.rows %% 10

# we will cut off the first c. 50 rows as they are junk
# we will read in the data in batches of 50k

# how many batches will we use? 
initial.skip <- 40 + leftover

no.runs <- (no.col - initial.skip) / 50000

no.runs <- ceiling(no.runs)

batch.size <- (no.rows - initial.skip) / no.runs

for (x in 1:no.runs) {
  batch <- batch.size
  to.skip <- initial.skip + (x-1)*batch
  temp <- read.table(file="../Data/WoRMS/taxon.txt", 
                     sep="\t", 
                     fill = T, 
                     stringsAsFactors = F, 
                     quote = "",
                     skip = to.skip, nrows = batch)
  colnames(temp) <- a
  total <- rbind(total, temp)
  print(x)
  print("batch is done. wahey!")
}

# now all the data is read in, we just subset for the data we actually want 
Actinop <- subset(total, total$class=="Actinopterygii") # this subset contains a lot of superflous data

# now we only look for rows containing species data
Actinop.spp <- subset(Actinop, Actinop$taxonRank=="species")


# How many species in this dataframe?
all.species <- as.numeric(length(unique(Actinop.spp$scientificName))) # 45 672

# how many of these species are accepted
Accepted <- subset(Actinop.spp, Actinop.spp$taxonomicStatus == "accepted") # 35 226
no.accepted <- as.numeric(length(Accepted$taxonID))
accepted.species <- as.numeric(length(unique(Accepted$scientificName))) # 17 613

# save this beautiful data down 
write.csv(Accepted, file = "../Data/Full_Actiopterygii_WoRMS_data.csv")

worms.data <- read.csv(file = "../Data/Full_Actiopterygii_WoRMS_data.csv", stringsAsFactors = F)

head(worms.data)
#worms.data <- worms.data[,-1]
usefulcols <- c(7, 8, 15,16,17,18,19,20,24)
colnames <- as.character(worms.data[1,])

worms.use <- worms.data[,usefulcols]
worms.accepted <- subset(worms.use, worms.use$taxonomicStatus=="accepted")
length(worms.accepted$scientificName) # 35226
length(unique(worms.accepted$scientificName)) #17613

length(unique(worms.accepted$scientificName))
length(unique(worms.accepted$acceptedNameUsage)) # 17612

# Now, we save down the use useful data so it can be used later
save(worms.accepted, file = "../Data/WoRMS_AcceptedSpecies.Rdata")


 