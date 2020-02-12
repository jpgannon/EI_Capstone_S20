
#this is script creates a master dataframe containing the cleaned data from 
#all wells in watershed 3

#add tidyverse library and set working directory
library(tidyverse)
setwd("C:/Capstone/Data/WS3_waterlevel_data")

#create list of well .csv files + w3well_locations.txt
well_list <- list.files() %>% 
  as.factor

#read in welldata.txt (must be last file in folder)
well_locations <- read_csv(well_list[length(well_list)])

#create list of wells 
wells <- c()
for (x in 1:nrow(well_locations)){
  wells[x] <- well_locations$Well[x]
}


#read in well data into a list of dataframes
well_files <- lapply(well_list, read_csv)
  
for (x in 1:length(well_files)-1){
  wells[x] <- as.data.frame(well_files[x])
}



#Choose well being visualized
ID <- "D1"
#Choose beginning of date range 
start <- "2011-07-01"
#Choose end of date range 
end <- "2011-10-31"
#Read in well chosen above when creating the ID variable
well <- read_csv(paste(ID, ".csv", sep = ""))
#Remove wells with a value of -99
well$level <-  na_if(well$level, -99)
#replace the level values above 168 with NA so that they do not show up on the graph 
well$level <- replace(well$level, well$level > 168, NA)
#create new variable containing the "w3well_locations" file 
info <- read_csv("w3well_locations.txt")
PH <- info$PipeHt[info$Well == ID]
#Subtract the pipeheight from the level data 
well$level <- well$level - PH
#Create a new variable that has the date range selected using the "start" and "end" variables
welltime <- filter(well, date >= start, date <= end)


