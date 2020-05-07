# this script just created a tree for each scenario, and didn't do an EDGE analysis
# we are going to read in each score, calculate the tree PD, PDLoss and ePD under each scenario
# we are then going to plot it



# Housekeeping 

rm(list=ls())
graphics.off()
setwd("~/Documents/ResearchProject/Code/")

# getting ready to ready to read everything in 

# long-form storage:

store <- data.frame(TREE = NA, pext = NA, calc = NA, value = rep(0, 1000) )

# tree: 1-100
# calc: PDtotal, PDloss, ePD
# value: length 

ptype <- c("Isaac","pext50", "pext100","pext500", "pextPessimistic")
calc <- c("PDtotal", "PDloss", "ePD")
counter <- 0

##Load up reference data (this contains original PD from each tree)
load(file = "../Results/HPC_EDGE/PDLoss_100.Rdata")


# work through each ptype
for (x in ptype) {
  # and then each tree
    for (i in 1:100) {
      counter <- counter + 1
      res <- NA
      to.load <- paste("../Results/HPC_ED/ED_only_HPC_24hr_", x, "_", i, ".Rdata", sep = "")
      try(load(file = to.load), silent = F) # called res
      if(!is.na(res)) {
      store$TREE[counter] <- i
      store$pext[counter] <- x
      store$calc[counter] <- calc[2]
      store$value[counter] <- sum(res[[2]]$edge.length)
      
      counter <- counter + 1
      store$TREE[counter] <- i
      store$pext[counter] <- x
      store$calc[counter] <- calc[3]
      store$value[counter] <- pd.loss$TreePD[i] - sum(res[[2]]$edge.length)
      }
}
}



store.2 <- data.frame(TREE = NA, pext = NA, calc = NA, 
                      value = rep(0, (length(store$TREE) + 100)))
store.2$TREE <- c(store$TREE, c(1:100))
store.2$pext <- c(store$pext, rep("today", 100))
store.2$calc <- c(store$calc, rep("ePD", 100))
store.2$value <- c(store$value, pd.loss$TreePD)


## what if we try and plot this?

# want to compare ePD
ePD <- subset(store.2, store.2$calc =="ePD")


plot.new()
boxplot(ePD$value ~ ePD$pext, 
        main = "Actinopterygii ePD, using 5 probabilities of extinction forecasts", 
        frame = F,
        xlab = "Probabilities of extinction forecasts", 
        ylab = "PD (millions of years)", 
        col = "hotpink")



### how about percentage remaining? 

ePD <- subset(store.2, store.2$calc =="ePD")
ePD.2 <- subset(ePD, ePD$pext != "today")

ePD.2$pext[ePD.2$pext=="pext50"] <- "pext050" # to ensure its in the right order!

plot.new()
boxplot((ePD.2$value/pd.loss$TreePD[1]*100) ~ ePD.2$pext, 
        main = "Surviving Actinopterygii PD, given \n varying probabilities of extinction", 
        frame = T,
        colour = "steelblue2",
        xlab = "Probabilities of extinction forecasts", 
        ylab = "Percentage of current PD (%)",
        xaxt = 'n',
        )
axis(1,labels = ptype, at = seq(1:length(ptype)))


