#Author: Robbie Coulter

#this is script creates hourly and 6 hour summary of water depth for all wells in watershed 3


#add tidyverse library and set working directory
library(tidyverse)
library(lubridate)

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
welldata$level <- replace(welldata$level, welldata$level > 168, 170) #not sure if this should be 162 or 168

#Subtract pipe height from level in order to get column that is depth of water table to surface 
welldata$wtdepth <- welldata$level - welldata$PipeHt

#Create a new dataframe that has only the wells wanted (select wells wanting to keep)
welldata <- select(welldata, date., level, Well, PipeHt, wtdepth)



#####create Hourly summary########
welldata$date. <- ymd_hms(welldata$date.)

welldata %>% group_by(Well, year(date.), month(date.), day(date.), hour(date.)) %>%
  summarize(median(level, na.rm = TRUE), median(wtdepth, na.rm = TRUE)) -> hourly

colnames(hourly) <- c("Well", "year","month","day", "hour", "level","wtdepth")  

hourly$date. <- dmy_h(paste(hourly$day, hourly$month, hourly$year, hourly$hour))

hourly <- hourly %>% ungroup() %>%
  select(Well, date., level, wtdepth) 

write_csv(hourly, "C:/Capstone/Data/oneHourSummary.csv")


########create six hour summary##########
hourly$bySixHours <- cut(hourly$date., breaks = "6 hours")

sixHourSummary <- hourly %>% 
  group_by(Well, bySixHours) %>% 
  summarise(median(level, na.rm = TRUE), median(wtdepth, na.rm = TRUE))

colnames(sixHourSummary) = c("Well", "Time", "Level", "WtDepth")

write_csv(sixHourSummary, "C:/Capstone/Data/sixHourSummary.csv")




