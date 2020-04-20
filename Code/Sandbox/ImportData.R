# Could improve this file by writing all file names into a loop and then pulling in all together

FBandRed <- read.csv(file="../Data/FBandRed.csv", 
                   sep = ",", 
                   header = T,
                   fill = T, 
                   stringsAsFactors = F,
                   )


FBnotRed <- read.csv(file="../Data/FBnotRed.csv", 
                     sep = ",", 
                     header = T,
                     fill = T, 
                     stringsAsFactors = F,
)


RednotFB <- read.csv(file="../Data/RednotFB.csv",
                     sep = ",", 
                     header = T,
                     fill = T, 
                     stringsAsFactors = F,
)

WormsandRed <- read.csv(file="../Data/WormsandRed.csv",  
                        sep = ",", 
                        header = T,
                        fill = T, 
                        stringsAsFactors = F,
)

WormsnotRed <- read.csv(file="../Data/WormsnotRed.csv",  
                        sep = ",", 
                        header = T,
                        fill = T, 
                        stringsAsFactors = F,
)

RedNotWorms <- read.csv(file="../Data/RednotWorms.csv",  
                        sep = ",", 
                        header = T,
                        fill = T, 
                        stringsAsFactors = F,
)

TreeandRed <- read.csv(file="../Data/TreeandRed.csv",         
                       sep = ",", 
                       header = T,
                       fill = T, 
                       stringsAsFactors = F,
)


WormsnotRed <- read.csv(file="../Data/WormsnotRed.csv",  
                        sep = ",", 
                        header = T,
                        fill = T, 
                        stringsAsFactors = F,
)

RedNotWorms <- read.csv(file="../Data/RednotWorms.csv",  
                        sep = ",", 
                        header = T,
                        fill = T, 
                        stringsAsFactors = F,
)

TreeandRed <- read.csv(file="../Data/TreeandRed.csv",         
                       sep = ",", 
                       header = T,
                       fill = T, 
                       stringsAsFactors = F,
)


WormsnotRed <- read.csv(file="../Data/WormsnotRed.csv",  
         sep = ",", 
         header = T,
         fill = T, 
         stringsAsFactors = F,
)

RedNotWorms <- read.csv(file="../Data/RednotWorms.csv",  
         sep = ",", 
         header = T,
         fill = T, 
         stringsAsFactors = F,
)

TreeandRed <- read.csv(file="../Data/TreeandRed.csv",         
                       sep = ",", 
                       header = T,
                       fill = T, 
                       stringsAsFactors = F,
)


TreenotRed <- read.csv(file="../Data/TreenotRed.csv", 
                       sep = ",", 
                       header = T,
                       fill = T, 
                       stringsAsFactors = F,
)
                       
RednotTree <- read.csv(file="../Data/RednotTree.csv",         
                       sep = ",", 
                       header = T,
                       fill = T, 
                       stringsAsFactors = F,
)

Summary <- read.csv(file="../Data/Summary.csv",         
                    sep = ",", 
                    header = T,
                    fill = T, 
                    stringsAsFactors = F,
)


##################################


### import the main databases

#WoRMS: worms$scientificName
worms <- read.csv(file= "../Data/Full_Actiopterygii_WoRMS_data.csv",sep = ",", 
                  fill = T, stringsAsFactors = F)

#FishBase: fb$ScientificName
fb <- read.csv(file = "../Data/fb.csv", header = T, sep = ",", stringsAsFactors = F, fill = T)
fb <- fb[,-1]

#FishTree: tree

tree <- read.csv(file = "../Data/tree.csv", 
                 header = T, sep = ",", stringsAsFactors = F, fill = T)
tree <- as.data.frame(tree[,-1])
colnames(tree) <- c("ScientificName")

#IUCN: red$scientificName   

red <- read.csv(file = "../Data/red.csv", 
                header = T, sep = ",", stringsAsFactors = F, fill = T)
red <- as.data.frame(red[,-1])


#######################