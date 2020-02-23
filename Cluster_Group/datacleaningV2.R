

#this is script creates a master dataframe containing the cleaned data from 
#all wells in watershed 3

#add tidyverse library and set working directory
library(tidyverse)
library(purrr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
#library(zoo)
#library(timeSeries)
setwd("C:/Capstone/Data/WS3_waterlevel_data")

#############################################################
#This part of the script creates a list of tibbles containing 
#cleaned tibbles for each well
#############################################################


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

#establish empty values for the overall start date and end date
#initial value is start date and end dates from first well in folder
start_date <- min(well_files[[well_names[1]]]$date.)
end_date <- max(well_files[[well_names[1]]]$date.)

for (x in well_names){
  #change readings of -99 to NA
  well_files[[x]]$level[well_files[[x]]$level == -99] <- NA
  
  #change readings greater than 168 to NA
  well_files[[x]]$level[well_files[[x]]$level >= 168] <- NA
  
  #subtract pipe height based on well metadata
  PH <- well_locations$PipeHt[well_locations$Well == x]
  well_files[[x]]$level <- well_files[[x]]$level - PH
  
  #compares the earliest and latest dates to the earliest and latest thus far in the list of tibbles
  #this is to determine the earliest and latest dates in the dataset
  if (min(well_files[[x]]$date. <= start_date)){
    start_date <- min(well_files[[x]]$date.)
  }
  if (max(well_files[[x]]$date. >= end_date)){
    end_date <- max(well_files[[x]]$date.)
  }
  
}


################################################################
#This part of the script is based on the previous project
#creates a .csv in long format containing all of the data points
################################################################

setwd("C:/Capstone/Data/WS3_waterlevel_data")
#Create a variable that contains the w3well_locations file
info <- read_csv("C:/Capstone/Data/w3well_locations.txt")
#create character vector with names of each .txt file that is within the set working directory 
files <- dir(pattern = ".csv")
#bind_rows(welldata, .id = "Well")
welldata <- data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path( .))) # a new data column
  )%>%
  unnest()

#take away .txt at end of well name in the "Well" column 
welldata$Well <- str_replace(welldata$filename, ".csv", "")
#Join W3Well_locations.txt file to the data frame and Keep each row of data assigned to the proper well (i.e. all rows that belong to well A5 will have A5 #in the "Well" column)
welldata <- left_join(welldata, info, by = "Well")

#Remove well data with -99 value 
welldata$level <- na_if(welldata$level, -99)
#replace the level values above 168 with NA so that they do not show up on the graph 
welldata$level <- replace(welldata$level, welldata$level > 168, NA) #not sure if this should be 162 or 168

#Subtract pipe height from level in order to get column that is depth of water table to surface 
welldata$wtdepth <- welldata$level - welldata$PipeHt

#Create a new dataframe that has only the wells wanted (select wells wanting to keep)
welldata <- select(welldata, date., level, Well, PipeHt, wtdepth)

#create csv file with the new data frame (makes app simpler by taking away need to do data wrangling in app)
#(welldata, "C:/Capstone/Data/allwelldata.csv")


###quick plot
welldata %>% filter(Well == "O2") %>%
  ggplot(aes(date., wtdepth))+
  geom_line()



#####group by time frames########
welldata$date. <- ymd_hms(welldata$date.)

welldata %>% group_by(Well, year(date.), month(date.), day(date.), hour(date.)) %>%
  summarize(median(level), median(wtdepth)) -> hourly

colnames(hourly) <- c("Well", "year","month","day", "hour", "level","wtdepth")  

hourly$date. <- dmy_h(paste(hourly$day, hourly$month, hourly$year, hourly$hour))

hourly <- hourly %>% ungroup() %>%
  select(Well, date., level, wtdepth) 

