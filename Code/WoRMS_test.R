setwd("Documents/CMEECourseWork/ResearchProject/Code/")


##### THIS FILE IS AN ABSOLUTE NIGHTMARE
## DIMENSIONS:  approximately 500,000 rows * 32 cols
## The first couple of rows are a complete mess, so skip them. I reccomend skipping 50, but I'm tired. Could explore more. 
## Whenever a spp has been published in a paper, then the 'sep' function mucks up, and it is spread over multiple rows. 
## This may need manual fixing - or maybe I can just cut them all. 
# nrows = 496,984

(496984-34)/10

#################### EXPLORATION 

# this is to find the col names (can't do headers when you skip)
a <- read.table(file="../Data/WoRMS/taxon.txt", nrows = 1, fill = T, sep = "\t", stringsAsFactors = F)

# this is a test- read in the first 100 rows (after the skip period)
d <- read.table(file="../Data/WoRMS/taxon.txt", nrows = 100, skip = 50, header = T, 
                fill = T, sep = "\t", stringsAsFactors = F)

# we then add the col names to the database, which can be explored!
colnames(d) <- a



#### created a loop to try and read in all of the columns in batches of 50k(so we don't freak out )
temp <- data.frame()
total <- data.frame(matrix(ncol = 32))
colnames(total) <- a
for (x in 1:10) {
  batch <- 49695
  to.skip <- 34 + (x-1)*batch
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


Actinop <- subset(total, total$class=="Actinopterygii")


Actiop.spp <- subset(Actinop, Actinop$taxonRank=="species")
# save this beautiful data down 
write.csv(Actiop.spp, file = "../Data/Full_Actiopterygii_WoRMS_data.csv")

worms.data <- read.csv(file = "../Data/Full_Actiopterygii_WoRMS_data.csv", stringsAsFactors = F)
head(worms.data)
worms.data <- worms.data[,-1]
usefulcols <- c(7, 8, 15,16,17,18,19,20,24)
colnames <- as.character(worms.data[1,])

worms.use <- worms.data[,usefulcols]
worms.accepted <- subset(worms.use, worms.use$taxonomicStatus=="accepted")
length(worms.accepted$scientificName) # 17610
length(unique(worms.accepted$scientificName))
length(unique(worms.accepted$acceptedNameUsage))

  # + taxanomic status, 
WoRMs.use <- Actiop.spp[,usefulcols]


head(worms)
worms <- worms[,-1]

worms <- worms[,usefulcols]
length(worms$scientificName)
length(unique(worms$scientificName))
 