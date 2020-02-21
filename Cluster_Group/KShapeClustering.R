#This script implements the K-Shape clustering algorithm to cluster Watershed 3
#wells at the HBEF

library(dtwclust)

setwd("C:/Capstone/Data/WS3_waterlevel_data")

data <- read_csv("allwelldata.csv")

sbd(well_files[[1]]$level, well_files[[2]]$level, znorm = TRUE, return.shifted = FALSE)



sbd(well_files[[3]]$level, well_files[[20]]$level, znorm = TRUE, return.shifted = FALSE)


for (x in 1:length(well_files)){
  for (y in 1:length(well_files)){
    sbd(well_files[[x]]$level, well_files[[y]]$level, znorm = TRUE, return.shifted = FALSE)
  }
}



