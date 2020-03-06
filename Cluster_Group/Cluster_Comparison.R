#Comparison of clustering methods and numbers of clusters
#using internal comparisons and existing HPU clusters

library(tidyverse)
library(dtwclust)

setwd("C:/Capstone/Data")

data <- read_csv("oneHourSummary.csv")
HPU <- read_csv("well_hpu.csv")

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
         Time <= end_date) %>% 
  na.omit()

#create list of list for tsclust function
wellsList <- lapply(split(Wells$WtDepth, Wells$Well), as.list)

#establish emtpy data frame to store internal CVI results
internal_results <- data.frame(Algorithm = character(),
                               NClusters = integer(),
                               Sil = double(),
                               D = double(),
                               COP = double(),
                               DB = double(),
                               DBStar = double(),
                               CH = double(),
                               SF = double())




#K Shape

for (x in 3:10){
  #running k-shape algorithm for each N Cluster in 3 to 10
  k_shape_result <- tsclust(series = wellsList,
                        type = "partitional",
                        k = x,
                        distance = "sbd")
  
  #calculation of each Internal CVI
  
  CVI_results <- cvi(k_shape_result, type = "internal")
  
  Algorithm <-  "K-shape"
  NClusters <- x
  Sil <- CVI_results["Sil"]
  D <- CVI_results["D"]
  COP <- CVI_results["COP"]
  DB <- CVI_results["DB"]
  DBstar <- CVI_results["DBstar"]
  CH <- CVI_results["CH"]
  SF <- CVI_results["SF"]
  
  internal_results <- rbind(internal_results, data.frame(Algorithm, NClusters, Sil, D, COP, DB, DBstar, CH, SF))
}


#DTW

for (x in 3:10){
  #running DTW algorithm for each N Cluster in 3 to 10
  dtw_result <- tsclust(series = wellsList,
                            type = "hierarchical",
                            k = x,
                            distance = "dtw_basic")
  
  #calculation of each Internal CVI
  
  CVI_results <- cvi(dtw_result, type = "internal")
  
  Algorithm <-  "dtw"
  NClusters <- x
  Sil <- CVI_results["Sil"]
  D <- CVI_results["D"]
  COP <- CVI_results["COP"]
  DB <- CVI_results["DB"]
  DBstar <- CVI_results["DBstar"]
  CH <- CVI_results["CH"]
  SF <- CVI_results["SF"]
  
  internal_results <- rbind(internal_results, data.frame(Algorithm, NClusters, Sil, D, COP, DB, DBstar, CH, SF))
}







