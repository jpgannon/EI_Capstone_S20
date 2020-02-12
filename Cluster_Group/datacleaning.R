
#this is script creates a master dataframe containing the cleaned data from 
#all wells in watershed 3

#add tidyverse library and set working directory
library(tidyverse)
setwd("C:/Capstone/Data/WS3_waterlevel_data")

##################import well data and well metadata###################3

#create list of well .csv files 
well_list <- list.files() 

#read in well data into a list of tibbles
well_files <- lapply(well_list, read_csv)

#read in well metadata in welldata.txt (located outside of master data folder)
well_locations <- read_csv("C:/Capstone/Data/w3well_locations.txt")

#create list of wells based on well metadata file
well_names <- c()
for (x in 1:nrow(well_locations)){
  well_names[x] <- well_locations$Well[x]
}

#name each tibble in the list appropriately
#each wells data can now be pulled by calling well_files[["well name"]]
names(well_files) <- well_names

###########loop through each well tibble to clean data#############

for (x in well_names){
  #change readings of -99 to NA
  #well_files[[x]]$level <- na_if(well_files[[x]]$level, -99)
  well_files[[x]]$level[well_files[[x]]$level == -99] <- NA
  
  #change readings greater than 162 (may be 168, must ask JP on this) to NA
  well_files[[x]]$level[well_files[[x]]$level >= 162] <- NA
  
  #subtract pipe height based on well metadata
  PH <- well_locations$PipeHt[well_locations$Well == x]
  well_files[[x]]$level <- well_files[[x]]$level - PH
  
}



