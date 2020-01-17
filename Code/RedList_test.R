
# my key:  30c68b19cedcc7f1cee81aa9b07e1cd235c49d9af8c184d76998b05e59a77c22

# install packages

# install.packages("rredlist")


# require packages

library(rredlist)

my.key <- c("30c68b19cedcc7f1cee81aa9b07e1cd235c49d9af8c184d76998b05e59a77c22")


########################## THIS BIT WORKS

# downloaded the Red List data for Actinopterygii - 17995 results!

#import whole dataset
redlist.all <- read.csv("Documents/CMEECourseWork/ResearchProject/Data/redlist_species_data_84c02514-9a98-4c0b-92de-465f9605a0e3/assessments.csv")

#create vector with just the data we need right now
redlist <- c()
redlist <- cbind(as.character(redlist.all$scientificName), as.character(redlist.all$redlistCategory))

colnames(redlist) <- c("RedList.species", "RedList.status")

###################################################################
################ EXPERIMENTAL CODE 

## testing the functionality

rl_common_names('Loxodonta africana', key = my.key)

rl_common_names()



# fish I am interested in: Class- Actinopterygii 

#Kingdom: 	Animalia
#Phylum: 	Chordata
#Superclass: 	Osteichthyes
#Class: 	Actinopterygii

# How many subclasses are there?

a.sub <- subset(fish.tax, fish.tax$name=="Actinopterygii")

head(fish.tax)

fish.names <- rl_common_names('Actinopterygii', key = my.key)
rl_common_names('Actinopterygii', key = my.key)

# this doesn't work
acto.group <- rl_comp_groups(group = "Actinopterygii", key = my.key)


# plan - I could run through the first 10 names in the master database and see if there are results?

master.db.10 <- master.db[1:10,]
master.db.10
storage.list <- 
  
  pag <- "pagrus pagrus"  
a <- rl_search(name = master.db.10[10,1], key = my.key)
c <- rl_search(name = "pagrus pagrus", key = my.key)
d <- rl_search(name = pag, key = my.key)
b <- list()
for (i in 1:10) {
  b[[i]] <- rl_search(master.db.10[i,1], key = my.key)
}
## rl_history procedure
IUCN<-NULL
for (i in redlist.all$scientificName) {
evaluation  <-rl_history(i, key = my.key)
result<-evaluation$result
result$sci_name<-i
IUCN<-rbind(IUCN,result)
print(i)
}
trout<-rl_history("Salmo trutta", key = my.key)
trout$result
  