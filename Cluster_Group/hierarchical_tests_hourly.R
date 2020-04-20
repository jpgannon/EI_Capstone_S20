library(tidyverse)
library(ggplot2)
library(ggdendro)
setwd("D:/Capstone/data")
hourly <- read_csv("hourly.csv")

wells_filter <- c("T1", "Q2", "Q1", "P2", "O2", "O1", "N5", "N4", "N3", "N2",
                  "N1", "K9", "K8", "K7S", "K7D", "K6S", "K6D", "K5", "K4S", "K4M",
                  "K4D", "K1D", "K11", "K10", "K1", "JD26", "JD25", "JD24", "JD23",
                  "JD22", "JD21", "JD03", "JD02", "I9", "I8", "I7", "I6", "I3", "H4",
                  "D1", "A6")

start_date <- "2012-01-01"
end_date <- "2012-06-01"

filtered_wells <- hourly %>% 
  filter(Well %in% wells_filter,
         date. >= start_date,
         date. <= end_date) %>% 
  select(date., Well, wtdepth)

wells_wide <- filtered_wells %>% 
  pivot_wider(names_from = Well,
              values_from = wtdepth)
#transposing 
transposed <- t(wells_wide[-1])
wells_dis <- dist(transposed, method = "maximum")
fit_dend <- hclust(wells_dis, method = "ward.D")
clusters <- rect.hclust(fit_dend, k = 5, border = "green")
plot(fit_dend, family = "Arial")

clusters_cu <- cutree(hclust(wells_dis), 5)

library(fpc)
cluster.stats(d = wells_dis, 
              clustering = clusters_cu,
              )


ggdendrogram(fit_dend, rotate = TRUE, theme_dendro = FALSE) +
  theme_minimal() + xlab("") + ylab("")

#Assigning clusters to well
clustered_data <- cutree(fit_dend, k = 5)
clustered_data_tidy <- as.data.frame(as.table(clustered_data))
colnames(clustered_data_tidy) <- c("Well", "Cluster")
clustered_data_tidy$Well <- as.character(clustered_data_tidy$Well)

#joining clusters to original filtered data
joined_clusters <- filtered_wells %>% 
  inner_join(clustered_data_tidy, by = "Well")

table(joined_clusters$Cluster)

#plotting clusters
ggplot(joined_clusters,
       aes(x = date.,
           y = wtdepth,
           color = Well)) +
  geom_line() +
  labs(x = "Month",
       y = "Water Depth", 
       title = "Water Depth Clusters for 01/01/2012 to 06/01/2012") +
  scale_y_continuous(trans = "reverse") +
  facet_wrap(~Cluster)

clusters <- as.data.frame(table(joined_clusters$Cluster))

#Testing cluster tendency
#install.packages("clustertend")
library(clustertend)
library(factoextra)
?clustertend
res <- get_clust_tendency(joined_clusters, n = nrow(joined_clusters - 1), graph = FALSE)
