library(purrr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)

#set working directory
setwd("D:/Capstone/data/WS3_waterlevel_data")

#read in and create object for well information text file
wells_info <- read_csv("D:/Capstone/data/w3well_locations.txt")

files <- dir(pattern = ".csv")

#creating data frame to hold csv files
wells_df <- tibble(filename = files) %>% 
  mutate(file_contents = map(filename,
                             ~read_csv(file.path(.))) 
  ) %>% 
  unnest()

#looking at well data structure
wells_df %>%
  arrange(level) %>% 
  str()

#Add a Well_ID column with ".csv"s removed
#Selecting out date, well, and levels columns
wells_df <- wells_df %>% 
  mutate(Well = str_replace(wells_df$filename, ".csv", ""),
         date = date.) %>% 
  select(date, Well, level)

#Joining wells_info with a left join and changing
#-99s  to NA
wells_df <- left_join(wells_df, wells_info, by = "Well")

wells_df$level <- replace(wells_df$level, wells_df$level > 168 | wells_df$level == -99, NA)

#Adjusting water depth based on water level and pipe height
wells_long <- wells_df %>%
  mutate(water_depth = level - PipeHt) %>% 
  select(date, Well, level, PipeHt, water_depth) %>% #Selecting usable rows in long data format
  group_by(date)


#looking at final data
wells_long %>% arrange(water_depth)
wells_long %>%
  arrange(water_depth) %>% 
  str()


wells_long %>% 
  count(is.na(water_depth))

#writing data to csv
write_csv(wells_long, "D:/Capstone/data/compiled_well_data.csv")
updated <- read_csv("D:/Capstone/data/compiled_well_data.csv")