#Comparison of clustering methods and numbers of clusters
#using internal comparisons and existing HPU clusters

library(tidyverse)
library(dtwclust)
library(caret)


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


#######compare cluster to clusters generated from soil clusters########


#create clusters based on well HPU
  
HPU <- read_csv("well_hpu.csv") %>% 
  filter(Well %in% target) 
#NOTE: K1S was changed to K1 in the original HPU csv, 
#I believe this was an error in the HPU csv because 
#I could not find a K1S well anywhere else, and K1 appeared to be missing from the HPU csv

HPU$HPU <- as.factor(HPU$HPU)

HPU$WellClass <- as.numeric(HPU$HPU)

#6 clusters are created via this method
#external clustering tests will be based on 6 clusters

external_results <- data.frame(Algorithm = character(),
                               NClusters = integer(),
                               RI = double(),
                               ARI = double(),
                               J = double(),
                               FM = double(),
                               VI = double()
                               )


#Comparing K-shape with 6 clusters to soil HPUs

k_shape_result <- tsclust(series = wellsList,
                          type = "partitional",
                          k = 6,
                          distance = "sbd")

CVI_results <- cvi(k_shape_result, HPU$WellClass, type = "external")

Algorithm <- "K-shape"
NClusters <- 6
RI <- CVI_results["RI"]
ARI <- CVI_results["ARI"]
J <- CVI_results["J"]
FM <- CVI_results["FM"]
VI <- CVI_results["VI"]

external_results <- rbind(external_results, data.frame(Algorithm, NClusters, RI, ARI, J, FM, VI))


#Comparing DTW with 6 clusters to soil HPUs
dtw_result <- tsclust(series = wellsList,
                          type = "hierarchical",
                          k = 6,
                          distance = "dtw_basic")

CVI_results <- cvi(dtw_result, HPU$WellClass, type = "external")

Algorithm <- "DTW"
NClusters <- 6
RI <- CVI_results["RI"]
ARI <- CVI_results["ARI"]
J <- CVI_results["J"]
FM <- CVI_results["FM"]
VI <- CVI_results["VI"]

external_results <- rbind(external_results, data.frame(Algorithm, NClusters, RI, ARI, J, FM, VI))

#creating error matrix to compare k shape and DTW clusters to soil HPUs

confusionMatrix(as.factor(k_shape_result@cluster), as.factor(HPU$WellClass))

#problem with error matrix is that class names are assigned arbitrarily,
#not sure if the accuracy assessments are taking this into account



#manually create confusion matrix to test results

#change this line to change what algorithm is being compared
result <- tsclust(series = wellsList,
                  type = "partitional",
                  k = nlevels(HPU$HPU),
                  distance = "sbd")


result_df <- data.frame(Well = target,
                        Cluster = result@cluster)

#create empty matrix to be populated with either TRUE POSITIVE, FALSE POSITIVE,
#TRUE NEGATIVE, or FALSE NEGATIVE

well_matrix <- matrix(, nrow = length(target), ncol = length(target))
colnames(well_matrix) <- target
rownames(well_matrix) <- target

#calculate outcomes and populate matrix
for(well1 in target){
  for(well2 in target){
    if(well1 != well2){ 
      if(is.na(well_matrix[well2, well1])){
        if(result_df$Cluster[result_df$Well == well1] == result_df$Cluster[result_df$Well == well2]){
          if(HPU$WellClass[HPU$Well == well1] == HPU$WellClass[HPU$Well == well2]){
            well_matrix[well1, well2] <- "TP" 
          }
          else{
            well_matrix[well1, well2] <- "FP" 
          }
        }
        else{
          if(HPU$WellClass[HPU$Well == well1] == HPU$WellClass[HPU$Well == well2]){
            well_matrix[well1, well2] <- "FN" 
          }
          else{
            well_matrix[well1, well2] <- "TN" 
          }
        }
      }
    }
  }
}



TP <- sum(well_matrix == "TP", na.rm = TRUE)
FP <- sum(well_matrix == "FP", na.rm = TRUE)
FN <- sum(well_matrix == "FN", na.rm = TRUE)
TN <- sum(well_matrix == "TN", na.rm = TRUE)

contingency_table <- matrix(data = c(TP, FP, FN, TN), nrow = 2, ncol = 2)

rownames(contingency_table) <- c("Same Class", "Different Class")
colnames(contingency_table) <- c("Same Cluster", "Different Cluster")

rand_index <- (TP + TN)/(TP + TN + FP + FN)





