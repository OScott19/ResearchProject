#housekeeping
rm(list=ls())
graphics.off()


# set wd
setwd("~/Documents/ResearchProject/Code/")

# required packagse
library(ggplot2)


## read in data

# 
data <- read.csv("../Data/FAOSTAT_data_3-18-2020.csv", stringsAsFactors = F)


years <- unique(data$Year)


df <- data.frame(matrix(nrow = length(years)))
df$years <- years
df$catch <- NA

counter <- 0
for (x in years) {
  temp <- subset(data, data$Year == x)
  temp.sum <- sum(temp$Value)
  counter <- counter + 1
  df$catch[counter] <- temp.sum
}


df$millions <- df$catch / 1000000

plot(x = df$years, y = log(df$catch))   

p <- ggplot(data = df) + 
  geom_point(aes(x = years, y = millions, col = millions)) +
  theme_bw() + 
  labs (x = "Years", y = "Global catch, millions of tonnes") +
        #title = "Annual global catch of fish", subtitle= "(millions of tonnes)") +
  theme(legend.position = "none")  +
  theme(axis.text = element_text(size = 25, colour = "steelblue4")) +
  theme(axis.title = element_text(size = 25, face = "bold"))

  
p  
#theme(plot.title = element_text(lineheight = 1.5, colour = "steelblue4", 
  #                                size = 16, face = "bold", hjust = 0.5)) + 
  #theme(plot.subtitle = element_text(lineheight = 0.9, colour = "steelblue4", 
                                 # size = 12, hjust = 0.5))
  


ggsave(filename = "../WriteUp/Images/GobalCatch.pdf", plot = p)


catch.type <- unique(data$Item)
catch.type
