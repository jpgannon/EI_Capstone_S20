library(tidyverse)
library(dtwclust)
library(fpc)
library(NbClust)

setwd("D:/Capstone/Data")

data <- read_csv("hourly.csv")

colnames(data) = c("Well", "Time", "Level", "WtDepth")

start_date <- "2012-01-01"
end_date <- "2012-06-01"


target <- c("T1", "Q2", "Q1", "P2", "O2", "O1", "N5", "N4", "N3", "N2",
            "N1", "K9", "K8", "K7S", "K7D", "K6S", "K6D", "K5", "K4S", "K4M",
            "K4D", "K1D", "K11", "K10", "K1", "JD25", "JD24", "JD23",
            "JD22", "JD21", "JD03", "JD02", "I9", "I8", "I7", "I6", "I3", "H4",
            "D1", "A6")

#filtering test data for tsclust test
Wells <- data %>% 
  filter(Well %in% target,
         Time >= start_date,
         Time <= end_date) %>% 
  na.omit()

negatives <- Wells %>% 
  filter(WtDepth < 0)

#create list of list for tsclust function
wellsList <- lapply(split(Wells$WtDepth, Wells$Well), as.list)

#run hierarchical clustering algorithm with dtw distance with 5 clusters
dtw_results <- tsclust(wellsList, type = "hierarchical", 5, distance = "dtw")

#dtw_basic is  much faster but might not be as "accurate" - seems to produce same results as dtw
dtw_basic_results <- tsclust(wellsList, type = "hierarchical", 5, distance = "dtw_basic")

#dtw2 distance which uses L2 norm which is Euclidean. dtw uses L1 which is Manhattan
dtw2_results <- tsclust(wellsList, type = "hierarchical", 5, distance = "dtw2")

#euclidean distance
euclidean_results <- tsclust(wellsList, type = "hierarchical", k = 5, distance = "euclidean")

?compare_clusterings

#saving and loading tscluster objects
# save(dtw_results, file = "D:/Capstone/data/clustering_results/dtw_results")
# load("D:/Capstone/data/clustering_results/dtw_results")
# save(dtw_basic_results, file = "D:/Capstone/data/clustering_results/dtw_basic_results")
# save(dtw2_results, file = "D:/Capstone/data/clustering_results/dtw2_results")

#plot cluster results
plot(results)
plot(results2, type = "series")
plot(results, type = "centroid", clus = 1)

#create df of cluster results
dtw_members <- data.frame(target, dtw_results@cluster)
basic_members <- data.frame(target, dtw_basic_results@cluster)
euclidean_members <- data.frame(target, euclidean_results@cluster)

#looking at spread of cluster groups for dtw and dtw_basic - seems the same
plot(dtw_members)
plot(basic_members)
plot(euclidean_members)

compare_clusterings(series = wellsList,
                    types = c("p", "h", "f"),
                    configs = compare_clusterings_configs(types),
                    return.objects = TRUE,
                    score.clus = clus_score,
                    pick.clus = clus_pick
                    )

?cvi
cvi(a = dtw_results,
    type = "internal")
cvi(a = dtw_basic_results,
    type = "internal")

#compare hierarchical clustering results
plot(dtw_results, main = "DTW") 
plot(dtw_basic_results, main = "DTW_BASIC")
plot(dtw2_results, main = "DTW2")
plot(euclidean_results, main = "EUCLIDEAN")
