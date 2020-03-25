library(patchwork)
library(dtwclust)
library(tidyverse)
setwd("D:/Capstone/data")

#clustering with dates with high precip
data <- read_csv("oneHourSummary.csv")

colnames(data) = c("Well", "Time", "Level", "WtDepth")

start_date <- "2012-08-01"
end_date <- "2012-10-01"


target <- c("T1", "Q2", "Q1", "P2", "O2", "O1", "N5", "N4", "N3", "N2",
            "N1", "K9", "K8", "K7S", "K7D", "K6S", "K6D", "K5", "K4S", "K4M",
            "K4D", "K1D", "K11", "K10", "K1", "JD25", "JD24", "JD23",
            "JD22", "JD21", "JD03", "JD02", "I9", "I8", "I7", "I6", "I3", "H4",
            "D1", "A6")

# 09/17 to 09/19
Wells_slice1 <- data %>% 
  filter(Well %in% target,
         Time >= "2012-09-17",
         Time <= "2012-09-19") %>% 
  na.omit()

slice1_list <- lapply(split(Wells_slice1$WtDepth, Wells_slice1$Well), as.list)

dtw_basic_slice1 <- tsclust(slice1_list, type = "hierarchical", 6, distance = "dtw_basic")
dtw_slice1 <- plot(dtw_basic_slice1, type = "series")
plot(dtw_basic_slice1, main = "2012-09-17 to 2012-09-19")

kshape_slice1 <- tsclust(slice1_list, type = "partitional", 6, distance = "sbd", centroid = "shape")
kshape_slice1_plot <- plot(kshape_slice1, main = "K-shape: 2012-09-17 to 2012-09-19")

kshape_dtw <- dtw_slice1 + kshape_slice1_plot
kshape_dtw
kshape_dtw + plot_annotation(
  title = "Time Series Cluster Comparisons from 2012-09-17 to 2012-09-19",
  subtitle = "Left: Dynamic Time Warping\nRight: K-Means"
)


# 06-01 to 06-03
Wells_slice2 <- data %>% 
  filter(Well %in% target,
         Time >= "2012-06-01",
         Time <= "2012-06-03") %>% 
  na.omit()
slice2_list <- lapply(split(Wells_slice2$WtDepth, Wells_slice2$Well), as.list)
dtw_slice2 <- tsclust(slice2_list, type = "hierarchical", 6, distance = "dtw_basic")
kshape_slice2 <- tsclust(slice2_list, type = "partitional", 6, distance = "sbd", centroid = "shape")
dtw2 <- plot(dtw_slice2, type = "series")
kshape2 <- plot(kshape_slice2)
kshape_dtw_slice2 <- dtw2 + kshape2
kshape_dtw_slice2 + plot_annotation(
  title = "Time Series Cluster Comparisons from 2012-06-01 to 2012-06-01",
  subtitle = "Left: Dynamic Time Warping\nRight: K-Means"
)

# 01-01 to 04-15
wells_slice3 <- data %>% 
  filter(Well %in% target,
         Time >= "2012-01-01",
         Time <= "2012-04-15") %>% 
  na.omit()
slice3_list <- lapply(split(wells_slice3$WtDepth, wells_slice3$Well), as.list)
dtw_slice3 <- tsclust(slice3_list, type = "hierarchical", 6, distance = "dtw_basic")
kshape_slice3 <- tsclust(slice3_list, type = "partitional", 6, distance = "sbd", centroid = "shape")
dtw3 <- plot(dtw_slice3, type = "series")
kshape3 <- plot(kshape_slice3)
kshape_dtw_slice3 <- dtw3 + kshape3
slice3_patch <- kshape_dtw_slice3 + plot_annotation(
  title = "Time Series Cluster Comparisons from 2012-01-01 to 2012-04-15",
  subtitle = "Left: Dynamic Time Warping\nRight: K-Means",
  caption = 'reached elapsed time limit for K-Means'
)

# 01-01 to 06-01
wells_slice4 <- data %>% 
  filter(Well %in% target,
         Time >= "2012-01-01",
         Time <= "2012-06-01") %>% 
  na.omit()
slice4_list <- lapply(split(wells_slice4$WtDepth, wells_slice4$Well), as.list)
dtw_slice4 <- tsclust(slice4_list, type = "hierarchical", 6, distance = "dtw_basic")
kshape_slice4 <- tsclust(slice4_list, type = "partitional", 6, distance = "sbd", centroid = "shape")
dtw4 <- plot(dtw_slice4, type = "series")
kshape4 <- plot(kshape_slice4)
kshape_dtw_slice4 <- dtw4 + kshape4
slice4_patch <- kshape_dtw_slice4 + plot_annotation(
  title = "Time Series Cluster Comparisons from 2012-01-01 to 2012-06-01",
  subtitle = "Left: Dynamic Time Warping\nRight: K-Means",
  caption = 'reached elapsed time limit for K-Means'
)
slice4_patch
