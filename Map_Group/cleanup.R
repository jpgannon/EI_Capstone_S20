library(purrr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)



#Set working directory
setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group")

#Create a variable that contains the w3well_locations file
info <- read_csv("w3well_locations.txt")

#create character vector with names of each .csv file that is within the set working directory 
files <- dir(pattern = ".csv")

welldata <- data_frame(filename = files) %>% # create a data frame
  # holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_csv(file.path( .))) # a new data column
  )%>%
  unnest(cols = c(file_contents))


#replaces column "filename" with "Well" and removes ".csv" from column name
welldata$Well <- str_replace(welldata$filename, ".csv", "")


#renames the date column
welldata <- welldata %>%
  rename(date = date.)

head(welldata)

#Join W3Well_locations.txt file to the data frame and Keep each row of data assigned to the proper well (i.e. all rows that belong to well A5 will have A5 #in the "Well" column)
welldata <- left_join(welldata, info, by = "Well")

#Remove well data with -99 value 
welldata$level <- na_if(welldata$level, -99)

#replace the level values above 168 with NA so that they do not show up on the graph 
welldata$level <- replace(welldata$level, welldata$level > 168, NA)


#Subtract pipe height from level in order to get column that is depth of water table to surface 
welldata$wtdepth <- welldata$level - welldata$PipeHt

#Select only columns wanted and rearranges columns
welldata <- select(welldata, Well, date, wtdepth, level, PipeHt, X, Y)


welldata$date <- ymd_hms(welldata$date)

#Change data to hourly instead of 10-minutely
welldata %>% group_by(Well, year(date), month(date), day(date), hour(date)) %>%
  summarize(median(level), median(wtdepth)) -> hourly

colnames(hourly) <- c("Well", "year","month","day", "hour", "level","wtdepth")  

hourly$date <- dmy_h(paste(hourly$day, hourly$month, hourly$year, hourly$hour))

hourly <- hourly %>% ungroup() %>%
  select(Well, date, level, wtdepth) 

#welldatahourly <- aggregate(welldata$level,
#list(hour = cut(welldata$date, breaks="hour")),
#mean, na.rm = TRUE)

head(hourly)

#creates new csv file with the dataframe
write_csv(hourly, "welldatahourly.csv")



#Read in streamflow data
stream <- read_csv("HBEF_DailyStreamflow_1956-2017_longform.csv")
head(stream)


stream <- stream %>%
  filter(Watershed == 3) %>% #Filter for watershed 3
  filter(Streamflow != -99)  #Removes -99 (missing values)


#Creates new csv with discharge data
write_csv(stream, "stream_discharge_WS3.csv")



#Read in precip data
precip <- read_csv("dailyWatershedPrecip1956-2019.csv")

precipWS3 <- precip %>%
  filter(precip$watershed == 3) #Filter for watershed 3

#Creates new precipitation csv with only watershed 3
write_csv(precipWS3, "dailyprecip_WS3.csv")
