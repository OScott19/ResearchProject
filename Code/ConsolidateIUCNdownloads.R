# This script takes the two csv that are downloaded from an IUCN seach
# It combines them into one useful file, and then saves as new csv 

############ housekeeping
graphics.off()
rm(list = ls())
setwd("~/Documents/ResearchProject/Code/")



########### read in the two files: taxonomy.csv, assessments.csv

tax <- read.csv("../Data/taxonomy.csv", stringsAsFactors = F)
assessment <- read.csv("../Data/assessments.csv", stringsAsFactors = F)

## firstly, combine the two into one sheet (for future reference)

# create a list of the columns in tax that aren't presetn in assessument

col.assessment <- colnames(assessment)
col.tax <- colnames(tax)

only.tax <- c()
tax.cols <- c()

for (x in 1:length(col.tax)) {
  xx <- col.tax[x]
  if ((xx %in% col.assessment) == F) {
    only.tax <- c(only.tax, xx)
    tax.cols <- c(tax.cols, x)
  }
}



only.tax.data <- tax[,tax.cols]

# now splice these columns to the end of the assessment dataframe

all.data <- c(assessment, only.tax.data)

# save this data for future use
write.csv(all.data, "../Data/RedListDownload.csv")


# data we want to keep:
# assessmentId, scientificName, redlistCategoy, yearPublished, assessmentDate, populationTrend, possiblyExtinct, PossiblyExctinctInTheWild, className, familyName, genusName, speciesName 

#cols <- as.vector("assessmentId", "scientificName", "redlistCategory", "yearPublished", "assessmentDate", "populationTrend", "possiblyExtinct", 
#                  "PossiblyExctinctInTheWild", "className", "familyName", "genusName", "speciesName")

