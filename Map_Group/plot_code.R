
library(tidyverse)
library(ggplot2)

#Set working directory
setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/WS3_waterlevel_data")

#Lists files in folder
list.files()

well_ID <- "A5"
start <- "2011-07-01"
end <- "2011-10-31"
well <- read_csv(paste(well_ID, ".csv", sep = ""))

#Remove wells with a value of -99
well$level <-  na_if(well$level, -99)                 

#replace the level values above 168 with NA so that they do not show up on the graph 
well$level <- replace(well$level, well$level > 168, NA)
