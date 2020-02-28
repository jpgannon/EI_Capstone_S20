
library(tidyverse)
library(dtwclust)

setwd("C:/Capstone/Data")

data <- read_csv("oneHourSummary.csv")

colnames(data) = c("Well", "Time", "Level", "WtDepth")

start_date <- "2012-01-01"
end_date <- "2012-06-01"


target <- c("T1", "Q2", "Q1", "P2", "O2", "O1", "N5", "N4", "N3", "N2",
            "N1", "K9", "K8", "K7S", "K7D", "K6S", "K6D", "K5", "K4S", "K4M",
            "K4D", "K1D", "K11", "K10", "K1", "JD26", "JD25", "JD24", "JD23",
            "JD22", "JD21", "JD03", "JD02", "I9", "I8", "I7", "I6", "I3", "H4",
            "D1", "A6")

#filtering test data for tsclust test
Wells <- data %>% 
  filter(Well %in% target,
         Time >= start_date,
         Time <= end_date) %>% 
  na.omit()

#create list of list for tsclust function
wellsList <- lapply(split(Wells$WtDepth, Wells$Well), as.list)

#run k-shape clustering algorithms with 5 clusters
results <- tsclust(wellsList, type = "hierarchical", 5, distance = "dtw")

#plot cluster results
plot(results)

#create df of cluster results
members <- data.frame(target, results@cluster)


