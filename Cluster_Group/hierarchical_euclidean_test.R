#loading packages
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(ggdendro)
#Cluster analysis in R needs:
#1. Rows are observations (individuals) and columns are variables
#2. Any missing data value in the data must be removed or estimated
#3. The data must be standardized/scaled to make variables comparable

#Trying HC with sample tutorial data -------------------------------------------
#removing NA values
df <- USArrests
df <- na.omit(df)

#scaling/standardizing data
scale(df)

#agglomerative HC: bottom-up, each object is initially a "leaf" and are grouped 
#together at each step of the algorithm
#hclust

#computing dissimilarity values with dist()
d <- dist(df, method = "euclidean")

#clustering using complete linkage
hc1 <- hclust(d, method = "complete")

#plot dendrogram
plot(hc1, cex = 0.6, hang = -1)

#compute with agnes which can give the agglomerative coefficient, which measures
#the amount of clustering structure found (closer to 1 suggests strong clustering structure)
hc2 <- agnes(df, method = "complete")
hc2$ac

#testing different methods
m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")
?map_dbl

#function to compute coefficient
ac <- function(x){
  agnes(df, method = x)$ac
}
?map_dbl
map_dbl(m, ac)
pltree(hc2, cex = 0.6, hang = -1, main = "Agnes complete method")
hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Agnes ward method")
#--------------------------------------------------------------------------------------------
#Divisive (DIANA) HC: top-down, Every object begins as part of a single cluster and the most heterogenous 
#cluster is divided in two at each step of the algorithm; repeated until all objects are in their own cluster
#diana() is used to compute divisive HC; just need to provide data frame
hc4 <- diana(df)

#divisive coefficient: amount of clustering structure found
hc4$dc

#dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

#-------------------------------------------------------------------------------------------------
#Testing with watershed 3 wells data

#02202020
setwd("D:/Capstone/data")
well_df <- read_csv("compiled_well_data.csv")

#removing NAs
well_df <- na.omit(well_df)

#just trying to get date into same lengths
#trying just getting means of each well by month & year
well_means_by_year <- well_df %>% 
  group_by(year(date), Well) %>% 
  summarize(avg = mean(water_depth))

#pivot_wide
well_means_wide <- well_means_by_year %>% 
  pivot_wider(names_from = Well, values_from = avg)

jd_wells <- c("JD01", "JD02", "JD03", "JD04", "JD05", "JD06", "JD07", "JD08", "JD10")

jd_wells_means <- well_means_wide %>% 
  select(jd_wells)

#changing all NAs - need to figure out what to do with NAs in data
jd_wells_means %>% replace_na(list(JD01 = mean(jd_wells_means$JD01, na.rm = TRUE)))

#transpose
meanss <- t(jd_wells_means[-1])
colnames(meanss) <- c("2007":"2018")

?hclust
jd_dist <- dist(meanss, method = "euclidean")
fit_dend <- hclust(jd_dist, method = "ward.D")
plot(fit_dend, family = "Arial")
rect.hclust(fit_dend, k = 5, border = "cyan")

?ggdendrogram
ggdendrogram(fit_dend, rotate = TRUE, theme_dendro = FALSE) +
  theme_minimal() + xlab("") + ylab("Wells")

#shinyapp structure: UI -> server -> function that fuses them together
# All server logic in one file, 
#server global UI**

#trying HC with common date range for JD wells excluding JD03 and JD08
jd_filter <- c("JD01", "JD02", "JD04", "JD05", "JD06", "JD07",
             "JD10", "JD11", "JD12", "JD13", "JD14", "JD15", "JD16", "JD17",
             "JD18","JD19", "JD20", "JD21", "JD22", "JD23", "JD24", "JD25", 
             "JD26", "JD27", "JD28", "JD29", "JD30")


write_csv(jd_hc, "D:/Capstone/data/jd_wells.csv")
jd_hc <- well_df %>% 
  filter(Well %in% jd_filter,
         date >= "2008-01-01",
         date <= "2008-04-15")

ggplot(jd_hc,
       aes(x = date,
           y = water_depth,
           color = Well)) +
  geom_point()

jd_NAs <- jd_hc %>% 
  filter(is.na(water_depth))
#-------------------------------------------------------------------------------------
#trying HC with JD wells from 2008-01-01 to 2008-04-15
setwd("D:/Capstone/data")
jd_wells <- read_csv("jd_wells.csv")

jd_wells %>% 
  arrange(date)

jd_wide <- jd_wells %>%
  select(Well, date, water_depth) %>% 
  pivot_wider(names_from = Well,
              values_from = water_depth)

#write_csv(jd_wide, path = "D:/Capstone/data/jd_wide.csv")

#plotting dendrogram for JD wells
transposed <- t(jd_wide[-1])
jd_dis <- dist(transposed, method = "euclidean")
fit_dend <- hclust(jd_dis, method = "ward.D")
rect.hclust(fit_dend, k = 5, border = "cyan")
plot(fit_dend, family = "Arial")

??ggdendrogam
ggdendrogram(fit_dend, rotate = TRUE, theme_dendro = FALSE) +
  theme_minimal() + xlab("") + ylab("")

#Getting clusters from hclust()
clustered_data <- cutree(fit_dend, k = 4)
clustered_data_tidy <- as.data.frame(as.table(clustered_data))
colnames(clustered_data_tidy) <- c("Well", "Cluster")
clustered_data_tidy$Well <- as.character(clustered_data_tidy$Well)

#Creating new data frame with og data + clusters
joined_clusters <- jd_wells %>% 
  inner_join(clustered_data_tidy, by = "Well")

table(joined_clusters$Cluster)

#plotting avg water depth of each cluster
avg_clusters <- joined_clusters %>% 
  group_by(Cluster, date) %>% 
  summarize(avg_dpth = mean(water_depth))

ggplot(avg_clusters,
       aes(x = date,
           y = avg_dpth,
           color = Cluster)) + geom_line()

#plotting cluster 3
cluster_3 <- joined_clusters %>% 
  filter(Cluster == 3)

ggplot(cluster_3,
       aes(x = date,
           y = water_depth,
           color = Well)) + 
  geom_line(color = "red", size = 1) +
  scale_y_continuous(trans = "reverse") +
  theme_minimal() +
  labs(title = "Cluster 3") +
  facet_wrap(~Well) 

x#plotting cluster 2
cluster_2 <- joined_clusters %>% 
  filter(Cluster == 2)

ggplot(cluster_2,
       aes(x = date,
           y = water_depth,
           color = Well)) + 
  geom_line(color = "red", size = 1) +
  scale_y_continuous(trans = "reverse") +
  theme_minimal() +
  labs(title = "Cluster 3") +
  facet_wrap(~Well) 

#plotting cluster 4
cluster_4 <- joined_clusters %>% 
  filter(Cluster == 4)

ggplot(cluster_4,
       aes(x = date,
           y = water_depth,
           color = Well)) + 
  geom_line(color = "red", size = 1) +
  scale_y_continuous(trans = "reverse") +
  theme_minimal() +
  labs(title = "Cluster 3") +
  facet_wrap(~Well) 

#plotting all clusters
ggplot(joined_clusters,
       aes(x = date,
           y = water_depth,
           color = Cluster)) +
  geom_point() +
  scale_color_gradient(low = "yellow", high = "red") +
  facet_wrap(~Cluster) +
  labs(x = "Date (2008)",
       y = "Water Depth (cm)",
       title = "Water Depth of Four Clusters for JD Wells")



