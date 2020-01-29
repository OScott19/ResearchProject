### Useful functions

### need to define the two data-frames outside of the function, and then call them  - wise can't save?

print("To use these functions, first initialise a dataframe, which will be called in the function. Then, assign the 
      function to a variable. E.G:
      test <- dataframe()
      output <- function(arguments)
      The results of the function will then be stored in the output variable.")


#This function creates a list of species that are found both in the IUCN red list, and an dataframe. 
RedOverlap <- function(RedName, RedCategory, RedID, DBName, counter, dataframe) {
  start.time <- proc.time()[3]
  print("Starting function")
  for (x in 1:length(RedName)) {
    # if the red list species is found on the  species list (i.e. match is True)
    if ((RedName[x] %in% DBName)==T) {
      # then add one to the counter, and add the species, its red list ID & category to the df
      counter <- counter + 1
      dataframe[counter,1] <-RedName[x]
      dataframe[counter,2] <-RedCategory[x]
      dataframe[counter,3] <-RedID[x] 
    }
  }
  run.time <- proc.time()[3] - start.time
  print(run.time)
  print("funcion ending")
  return(dataframe) # this will be stored in the variable the function is assigned to. 
  }

  
#### Add in family information 
AddFam <- function(dataframe, DBName, DBOrder, DBFam) {
    for (x in 1:length(dataframe[,1])) {
    ref <- match(x, DBName[x])
    dataframe[x,4] <- DBOrder[ref]
    dataframe[x,5] <- DBFam[ref]
    }
  print("Family added!")
  return(dataframe)
  }

AddFam <- function(RedName, RedCategory, RedID, DBName, counter, dataframe) {
  start.time <- proc.time()[3]
  print("Starting function")
  for (x in 1:length(RedName)) {
    # if the red list species is found on the  species list (i.e. match is True)
    if ((RedName[x] %in% DBName)==T) {
      # then add one to the counter, and add the species, its red list ID & category to the df
      counter <- counter + 1
      dataframe[counter,1] <-RedName[x]
      dataframe[counter,2] <-RedCategory[x]
      dataframe[counter,3] <-RedID[x] 
    }
  }
  run.time <- proc.time()[3] - start.time
  print(run.time)
  print("funcion ending")
  return(dataframe) # this will be stored in the variable the function is assigned to. 
}




  
#####
#This function stores a list of species that are found in the Red list but not in an dataframe
RedOnly <- function(RedName, RedCategory, RedID, DBFam, counter, dataframe) {
  print("Starting function")
  for (x in 1:length(RedName)) {
    if ((RedName[x] %in% DBName)==F) {
      # then add one to the counter, and add the species to the RedList only vector
      counter <- counter + 1
      dataframe[counter,1] <-RedName[x]
      dataframe[counter,2] <-RedCategory[x]
      dataframe[counter,3] <-RedID[x] 
      #print("Oh no")
    }}
  print("function finished")
  return(dataframe)
}

#This function stores a list of species are in the  dataframes but not in the red list
NotOnly <- function(RedName, DBName, DBOrder, DBFam, counter, dataframe) {
  print("Starting function")
  for (x in 1:length(DBName)) {
    if ((DBName[x] %in% RedName)==F) {
      # then add one to the counter, and add the species to the RedList only vector
      counter <- counter + 1
      dataframe[counter,1] <-DBName[x]
      dataframe[counter,2] <-DBOrder[x]
      dataframe[counter,3] <-DBFam[x]
      #print("Oh no")
    }}
  print("function finished")
  return(dataframe)
}



  
###### TESTS

test2variable <- RedOverlap(RedName = red$scientificName, RedCategory = red$redlistCategory, RedID = red$assessmentId, 
     DBName = worms$scientificName, counter = 0, dataframe = test_yes2)

fam.added <- AddFam(dataframe = test2variable, DBName = worms$scientificName, DBFam = worms$family, DBOrder = worms$order)


#testnovariable <- RedOnly(RedName = red$scientificName, RedCategory = red$redlistCategory, RedID = red$assessmentId, 
#                            DBName = worms$scientificName, counter = 0, dataframe = test_no2)




  for (x in 1:length(test2variable[,1])) {
    x = 10
    
    test2variable$v1[x]
    worms$scientificName[ref]
    ref
    ref <- match(test2variable$V1[x], worms$scientificName)
    test2variable[x,4] <- worms$order[ref]
    test2variable[x,5] <- worms$family[ref]
  }
  print("Family added!")
  return(dataframe)
}
