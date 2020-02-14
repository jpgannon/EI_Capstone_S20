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
wells_df %>%
  arrange(level) %>% 
  str()

#Add a Well_ID column with ".csv"s removed
wells_df <- wells_df %>% 
  mutate(Well = str_replace(wells_df$filename, ".csv", ""),
         date = date.) %>% 
  select(date, Well, level)

#Joining wells_info to data.frame with a left join and changing
  #-99s to NA
wells_df <- left_join(wells_df, wells_info, by = "Well") %>% 
  replace(. == -99, NA)


#Adjusting water depth based on water level and pipe height
wells_long <- wells_df %>%
  mutate(water_depth = level - PipeHt) %>% 
  select(date, Well, water_depth) %>% #Selecting usable rows in long data format
  group_by(date)


wells_long %>%
  arrange(water_depth) %>% 
  str()


wells_long %>% 
  count(is.na(water_depth))

#writing data to csv
write_csv(wells_long, "D:/Capstone/data/updated_data.csv")
updated <- read_csv("updated_data.csv")



#Making data wide so that rows are date, columns are Wells, and values are level
#Might not be necessary
#Lots of problems with NULL and not enough RAM to create unique row ids and remove them
wells_long %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = Well, values_from = water_depth) %>% 
  select(-row) %>%
  arrange(date)

wells_long %>% 
  pivot_wider(names_from = Well, values_from = water_depth) %>% 
  write.csv("wide_data.csv")

wells_wide <- wells_long %>% 
  pivot_wider(names_from = Well, values_from = water_depth) %>% 
  arrange(date)

write_csv(wells_wide, "wide_data.csv")

#View() wide data
wells_wide %>% 
  arrange(date) %>% 
  View()
  
#testing filtering of data
#does not work with the dataset
a5 <- filter(wells_long, Well == "A5")
a6 <- filter(wells_long, Well == "A6")

ID = c("A5")
start <- ymd("2012-05-12")
end <- ymd("2012-06-02")

filtered <- filter(wells_long, Well == ID & date >= start & date <= end)



