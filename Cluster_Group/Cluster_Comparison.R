#Comparison of clustering methods and numbers of clusters
#using internal comparisons and existing HPU clusters

library(tidyverse)
library(dtwclust)

setwd("C:/Capstone/Data")

data <- read_csv("oneHourSummary.csv")
HPU <- read_csv("well_hpu.csv")

colnames(data) = c("Well", "Time", "Level", "WtDepth")

start_date <- "2012-08-09"
end_date <- "2012-08-11"

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


#DTW interval CVI calculation

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


#manually create confusion matrix to test results

#DTW contigency table and RI index calculation

dtw_result_df <- data.frame(Well = rev(target),
                 Cluster = dtw_result@cluster)

#create empty matrix to be populated with either TRUE POSITIVE, FALSE POSITIVE,
#TRUE NEGATIVE, or FALSE NEGATIVE

dtw_well_matrix <- matrix(nrow = length(target), ncol = length(target))
colnames(dtw_well_matrix) <- target
rownames(dtw_well_matrix) <- target

#calculate outcomes and populate matrix
for(well1 in target){
  for(well2 in target){
    if(well1 != well2){ 
      if(is.na(dtw_well_matrix[well2, well1])){
        if(dtw_result_df$Cluster[dtw_result_df$Well == well1] == dtw_result_df$Cluster[dtw_result_df$Well == well2]){
          if(HPU$WellClass[HPU$Well == well1] == HPU$WellClass[HPU$Well == well2]){
            dtw_well_matrix[well1, well2] <- "TP" 
          }
          else{
            dtw_well_matrix[well1, well2] <- "FP" 
          }
        }
        else{
          if(HPU$WellClass[HPU$Well == well1] == HPU$WellClass[HPU$Well == well2]){
            dtw_well_matrix[well1, well2] <- "FN" 
          }
          else{
            dtw_well_matrix[well1, well2] <- "TN" 
          }
        }
      }
    }
  }
}

TP_dtw <- sum(dtw_well_matrix == "TP", na.rm = TRUE)
FP_dtw <- sum(dtw_well_matrix == "FP", na.rm = TRUE)
FN_dtw <- sum(dtw_well_matrix == "FN", na.rm = TRUE)
TN_dtw <- sum(dtw_well_matrix == "TN", na.rm = TRUE)

dtw_contingency_table <- matrix(data = c(TP_dtw, FP_dtw, FN_dtw, TN_dtw), nrow = 2, ncol = 2)

rownames(dtw_contingency_table) <- c("Same Class", "Different Class")
colnames(dtw_contingency_table) <- c("Same Cluster", "Different Cluster")

dtw_rand_index <- (TP_dtw + TN_dtw)/(TP_dtw + TN_dtw + FP_dtw + FN_dtw)


#KSHAPE#

k_shape_result_df <- data.frame(Well = rev(target),
                     Cluster = k_shape_result@cluster)

#create empty matrix to be populated with either TRUE POSITIVE, FALSE POSITIVE,
#TRUE NEGATIVE, or FALSE NEGATIVE

k_shape_well_matrix <- matrix(nrow = length(target), ncol = length(target))
colnames(k_shape_well_matrix) <- target
rownames(k_shape_well_matrix) <- target

#calculate outcomes and populate matrix
for(well1 in target){
  for(well2 in target){
    if(well1 != well2){ 
      if(is.na(k_shape_well_matrix[well2, well1])){
        if(k_shape_result_df$Cluster[k_shape_result_df$Well == well1] == k_shape_result_df$Cluster[k_shape_result_df$Well == well2]){
          if(HPU$WellClass[HPU$Well == well1] == HPU$WellClass[HPU$Well == well2]){
            k_shape_well_matrix[well1, well2] <- "TP" 
          }
          else{
            k_shape_well_matrix[well1, well2] <- "FP" 
          }
        }
        else{
          if(HPU$WellClass[HPU$Well == well1] == HPU$WellClass[HPU$Well == well2]){
            k_shape_well_matrix[well1, well2] <- "FN" 
          }
          else{
            k_shape_well_matrix[well1, well2] <- "TN" 
          }
        }
      }
    }
  }
}

TP_k_shape <- sum(k_shape_well_matrix == "TP", na.rm = TRUE)
FP_k_shape <- sum(k_shape_well_matrix == "FP", na.rm = TRUE)
FN_k_shape <- sum(k_shape_well_matrix == "FN", na.rm = TRUE)
TN_k_shape <- sum(k_shape_well_matrix == "TN", na.rm = TRUE)

k_shape_contingency_table <- matrix(data = c(TP_k_shape, FP_k_shape, FN_k_shape, TN_k_shape), nrow = 2, ncol = 2)

rownames(k_shape_contingency_table) <- c("Same Class", "Different Class")
colnames(k_shape_contingency_table) <- c("Same Cluster", "Different Cluster")

k_shape_rand_index <- (TP_k_shape + TN_k_shape)/(TP_k_shape + TN_k_shape + FP_k_shape + FN_k_shape)




