#the purpose of this script is to creat tile diagram to compare the results of K-Shape algorithm to 
#soil HPU classes

library(tidyverse)
library(reshape2)
library(ggplot2)
library(viridis)
setwd("C:/Capstone/Data")

#select target wells
target <- c("T1", "Q2", "Q1", "P2", "O2", "O1", "N5", "N4", "N3", "N2",
            "N1", "K9", "K8", "K7S", "K7D", "K6S", "K6D", "K5", "K4S", "K4M",
            "K4D", "K1D", "K11", "K10", "K1", "JD25", "JD24", "JD23",
            "JD22", "JD21", "JD03", "JD02", "I9", "I8", "I7", "I6", "I3", "H4",
            "D1", "A6")

#load HPu data for selected wells
HPU <- read_csv("well_hpu.csv") %>% 
  filter(Well %in% target) 

#load k_shape results
k_shape_results <- read_csv("k_shape_result.csv")

#create dataframe of k shape results and HPU classes
data <- left_join(HPU, k_shape_results, by = "Well")

#create vectors for each HPU class populated by what Wells belong to that HPU class
E <- c()
Bh <- c()
Typical <- c()
Bimodal <- c()
Aquept <- c()
Bhs <- c()

for (x in 1:nrow(data)){
  if (data$HPU[x] == "E"){
    E <- c(E, data$Well[x])
  }
  if (data$HPU[x] == "Bh"){
    Bh <- c(Bh, data$Well[x])
  }
  if (data$HPU[x] == "Typical"){
    Typical <- c(Typical, data$Well[x])
  }
  if (data$HPU[x] == "Bimodal"){
    Bimodal <- c(Bimodal, data$Well[x])
  }
  if (data$HPU[x] == "Aquept"){
    Aquept <- c(Aquept, data$Well[x])
  }
  if (data$HPU[x] == "Bhs"){
    Bhs <- c(Bhs, data$Well[x])
  }
}

#create vectors for each generated cluster populated by what wells belong to that cluster
cluster1 <- c()
cluster2 <- c()
cluster3 <- c()
cluster4 <- c()
cluster5 <- c()
cluster6 <- c()

for (x in 1:nrow(data)){
  if (data$Cluster[x] == 1){
    cluster1 <- c(cluster1, data$Well[x])
  }
  if (data$Cluster[x] == 2){
    cluster2 <- c(cluster2, data$Well[x])
  }
  if (data$Cluster[x] == 3){
    cluster3 <- c(cluster3, data$Well[x])
  }
  if (data$Cluster[x] == 4){
    cluster4 <- c(cluster4, data$Well[x])
  }
  if (data$Cluster[x] == 5){
    cluster5 <- c(cluster5, data$Well[x])
  }
  if (data$Cluster[x] == 6){
    cluster6 <- c(cluster6, data$Well[x])
  }
}


####create intersect counts matrix

clusters <- c(1,2,3,4,5,6)
HPUs <- c("E", "Bh", "Typical", "Bimodal", "Aquept", "Bhs")

#create empty matrix to be populated
intersect_matrix <- matrix(NA, nrow = 6, ncol = 6)
colnames(intersect_matrix) <- c("1", "2", "3", "4", "5", "6")
rownames(intersect_matrix) <- c("E", "Bh", "Typical", "Bimodal", "Aquept", "Bhs")

#count intersect value for each cluster and HPU combination
intersect_matrix["E", "1"] <- length(intersect(E, cluster1))
intersect_matrix["E", "2"] <- length(intersect(E, cluster2))
intersect_matrix["E", "3"] <- length(intersect(E, cluster3))
intersect_matrix["E", "4"] <- length(intersect(E, cluster4))
intersect_matrix["E", "5"] <- length(intersect(E, cluster5))
intersect_matrix["E", "6"] <- length(intersect(E, cluster6))

intersect_matrix["Bh", "1"] <- length(intersect(Bh, cluster1))
intersect_matrix["Bh", "2"] <- length(intersect(Bh, cluster2))
intersect_matrix["Bh", "3"] <- length(intersect(Bh, cluster3))
intersect_matrix["Bh", "4"] <- length(intersect(Bh, cluster4))
intersect_matrix["Bh", "5"] <- length(intersect(Bh, cluster5))
intersect_matrix["Bh", "6"] <- length(intersect(Bh, cluster6))

intersect_matrix["Typical", "1"] <- length(intersect(Typical, cluster1))
intersect_matrix["Typical", "2"] <- length(intersect(Typical, cluster2))
intersect_matrix["Typical", "3"] <- length(intersect(Typical, cluster3))
intersect_matrix["Typical", "4"] <- length(intersect(Typical, cluster4))
intersect_matrix["Typical", "5"] <- length(intersect(Typical, cluster5))
intersect_matrix["Typical", "6"] <- length(intersect(Typical, cluster6))

intersect_matrix["Bimodal", "1"] <- length(intersect(Bimodal, cluster1))
intersect_matrix["Bimodal", "2"] <- length(intersect(Bimodal, cluster2))
intersect_matrix["Bimodal", "3"] <- length(intersect(Bimodal, cluster3))
intersect_matrix["Bimodal", "4"] <- length(intersect(Bimodal, cluster4))
intersect_matrix["Bimodal", "5"] <- length(intersect(Bimodal, cluster5))
intersect_matrix["Bimodal", "6"] <- length(intersect(Bimodal, cluster6))

intersect_matrix["Aquept", "1"] <- length(intersect(Aquept, cluster1))
intersect_matrix["Aquept", "2"] <- length(intersect(Aquept, cluster2))
intersect_matrix["Aquept", "3"] <- length(intersect(Aquept, cluster3))
intersect_matrix["Aquept", "4"] <- length(intersect(Aquept, cluster4))
intersect_matrix["Aquept", "5"] <- length(intersect(Aquept, cluster5))
intersect_matrix["Aquept", "6"] <- length(intersect(Aquept, cluster6))

intersect_matrix["Bhs", "1"] <- length(intersect(Bhs, cluster1))
intersect_matrix["Bhs", "2"] <- length(intersect(Bhs, cluster2))
intersect_matrix["Bhs", "3"] <- length(intersect(Bhs, cluster3))
intersect_matrix["Bhs", "4"] <- length(intersect(Bhs, cluster4))
intersect_matrix["Bhs", "5"] <- length(intersect(Bhs, cluster5))
intersect_matrix["Bhs", "6"] <- length(intersect(Bhs, cluster6))


melted_intersection_matrix <- melt(intersect_matrix)

ggplot(data = melted_intersection_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  labs(x = "HPU Class",
       y = "K-Shape Generated Cluster",
       title = "Heatmap Comparing HPU Classes and K-Shape Results") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6)) +
  scale_fill_viridis(discrete=FALSE) +
  guides(fill=guide_legend(title="Intersection Count", reverse = TRUE))




