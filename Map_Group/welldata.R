library(purrr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(gridExtra)

setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/")

well_data <- read_csv("well_data.csv")

head(well_data)

#Create new variable containing wells being plotted
ID <- c("K9", "N5", "N1", "Q1", "H4", "A5")

#Choose the beginning of the date range by creating a new variable 
start <- ymd("2011-08-15")

#Choose the end of the date range by creating a new variable 
end <- ymd("2011-12-15")

#Create new variable that filters out the wells selected and the date #range #selected for plotting
wells <- filter(welldata, Well == ID, date >= start, date <= end)

#Make a line plot of the wells and dates selected, with a different line color for each well
p1 <- ggplot(data = wells, mapping = aes(x = date, y = wtdepth, color = Well))+
  geom_line()+
  scale_y_reverse()+
  ylab("Water Table Depth (cm)")+
  theme_classic()




#Precipitation data

#reads in daily precipitation by watershed data
precip <- read_csv("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/dailyWatershedPrecip1956-2019.csv")

head(precip)

#filter for only watershed 3
precip <- precip %>%
  filter(precip$watershed == 3)

#Filter for dates selected
precip_select <- filter(precip, Precip == Precip, DATE >= start, DATE <= end)

#creates a plot of precipitation within selected date range
p2 <- ggplot(data = precip_select, mapping = aes(x = DATE, y = Precip))+
  geom_line()+
  ylab("Precipitation (millimeter)")+
  scale_y_reverse()+
  theme_classic()


#stacks plots on top of eachother 
grid.arrange(p1, p2, nrow = 2)


