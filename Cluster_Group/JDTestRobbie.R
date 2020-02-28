#This script creates a list of timeseries of 
#all of the wells in the JD family that share a common timeframe
#to test tsclust function

library(tidyverse)
library(dtwclust)

setwd("C:/Capstone/Data")

sixHourSummary <- read_csv("sixHourSummary.csv")

#For the sake of this test, I will use data from wells 
#in the JD family, excluding JD03, JD08, JD09, and JD31 for lack of data in the test range
#Based on sixHourSummary data from datacleaningV2 File

start_date <- "2008-01-01"
end_date <- "2008-04-15"

target <- c("JD01", "JD02", "JD04", "JD05", "JD06", "JD07", "JD10",
            "JD11", "JD12", "JD13", "JD14", "JD15", "JD16", "JD17", "JD18",
            "JD19", "JD20", "JD21", "JD22", "JD23", "JD24", "JD25", "JD26",
            "JD27", "JD28", "JD29", "JD30")

#filtering test data for tsclust test
JDWells <- sixHourSummary %>% 
  filter(Well %in% target,
         Time >= start_date,
         Time <= end_date) 

#create list of list for tsclust function
JDWellsList <- lapply(split(JDWells$WtDepth, JDWells$Well), as.list)

#run k-shape clustering algorithms with 5 clusters
results <- tsclust(JDWellsList, type = "partitional", 5, distance = "sbd")

#plot cluster results
plot(results)

#create df of cluster results
members <- data.frame(target, results@cluster)


