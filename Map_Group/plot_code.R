library(purrr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)

install.packages("data.table", 
                 type = "source",
                 repos = "http://Rdatatable.github.io/data.table" )

library(data.table)

#Set working directory
setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/WS3_waterlevel_data")

#Create a variable that contains the w3well_locations file
info <- read_csv("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/w3well_locations.txt")

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

head(welldata)

#creates new csv file with the dataframe
write.csv(welldata, "well_data.csv")


