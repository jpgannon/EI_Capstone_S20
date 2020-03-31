#This script intends to compare different time series gap filling methods

#read in dataset and create well list os lists

library(tidyverse)
library(MTS)
setwd("C:/Capstone/Data")

data <- read_csv("oneHourSummary.csv")

colnames(data) = c("Well", "Time", "Level", "WtDepth")

start_date <- "2012-01-01"
end_date <- "2012-06-01"

target <- c("T1", "Q2", "Q1", "P2", "O2", "O1", "N5", "N4", "N3", "N2",
            "N1", "K9", "K8", "K7S", "K7D", "K6S", "K6D", "K5", "K4S", "K4M",
            "K4D", "K1D", "K11", "K10", "K1", "JD25", "JD24", "JD23",
            "JD22", "JD21", "JD03", "JD02", "I9", "I8", "I7", "I6", "I3", "H4",
            "D1", "A6")

Wells <- data %>% 
  filter(Well %in% target,
         Time >= start_date,
         Time <= end_date)

wellsList <- lapply(split(Wells$WtDepth, Wells$Well), as.list)

#create vector of entries for first sample well
sampleWell1 <- unname(as.vector(unlist(wellsList["I8"])))

#create vector of entries for first sample well
sampleWell2 <- unname(as.vector(unlist(wellsList["I9"]))) 

#create dataframe from two sample wells
wellSample <- data.frame("Well1" = sampleWell1,
                         "Well2" = sampleWell2) 

wellSample$ID <- seq.int(nrow(wellSample))

#create artificially missing data
wellSample["Well1Missing"] <- wellSample$Well1
wellSample$Well1Missing[1000:1999] <- NA
wellSample <- wellSample[c("ID", "Well1", "Well1Missing", "Well2")]
#create plot of missing data

ggplot(data = wellSample, mapping = aes(x = wellSample$ID)) +
  geom_line(aes(y = wellSample$Well1Missing), color = "red") +
  geom_line(aes(y = wellSample$Well2), color = "blue") +
  labs(title = "Sample 1 Data (Red) with Values Artificially Removed and Complete Sample Well 2 (Blue)",
       y = "Depth (cm)",
       x = "Timestamp")


#create synthetic data by one to one match

wellSample["Well1Synthetic"] <- NA
wellSample["SyntheticTest"] <- NA

for(x in 1:nrow(wellSample)){
  if(is.na(wellSample$Well1Missing[x])){
    wellSample$Well1Synthetic[x] <- wellSample$Well2[x]
    wellSample$SyntheticTest[x] <- TRUE
  }
  else{
    wellSample$SyntheticTest[x] <- FALSE
  }
}

ggplot(data = wellSample, mapping = aes(x = wellSample$ID)) +
  geom_line(aes(y = wellSample$Well1Synthetic), color = "red") +
  geom_line(aes(y = wellSample$Well1Missing), color = "blue") +
  labs(title = "Synthetic Data Based on 1:1 Match",
       x = "Timestamp",
       y = "Depth (cm)")


#create synthetic data based on mean(well1) + well2 match

well1mean <- mean(wellSample$Well1Missing, na.rm = TRUE)
well2mean <- mean(wellSample$Well2, na.rm = TRUE)
meanDifference <- well1mean - well2mean

wellSample["Well1SyntheticTest2"] <- NA
wellSample["SyntheticTest"] <- NA


for(x in 1:nrow(wellSample)){
  if(is.na(wellSample$Well1Missing[x])){
    wellSample$Well1SyntheticTest2[x] <- wellSample$Well2[x] + meanDifference
    wellSample$SyntheticTest[x] <- TRUE
  }
  else{
    wellSample$SyntheticTest[x] <- FALSE
  }
}


ggplot(data = wellSample, mapping = aes(x = wellSample$ID)) +
  geom_line(aes(y = wellSample$Well1Synthetic), color = "red") +
  geom_line(aes(y = wellSample$Well1), color = "blue") +
  geom_line(aes(y = wellSample$Well1SyntheticTest2), color = "green")+
  labs(x = "Timestamp",
       y = "Depth (cm)",
       title = "Comparison of Methods (1:1 Match in Red, Mean Difference Method in Green, Real Data in Blue)")



#VARMA test

VARMA(da, p = 0, q = 0, include.mean = T, 
      fixed = NULL, beta=NULL, sebeta=NULL, 
      prelim = F, details = F, thres = 2)



rmse <- function(error){
  sqrt(mean(error^2))
}

rmse(wellSample$Well1[1000:1999] - wellSample$Well1Synthetic[1000:1999])

rmse(wellSample$Well1[1000:1999] - wellSample$Well1SyntheticTest2[1000:1999])
