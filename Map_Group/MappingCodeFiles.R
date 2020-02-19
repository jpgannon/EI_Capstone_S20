setwd("C:/Users/rcm13/OneDrive/Documents/VirginiaTech/Capstone/")

library(leaflet)
library(tidyverse)
library(lubridate)
library(htmltools)

# Load the txt file for mapping
well_locations <- read_csv("MappingFiles/Well_LocationsDD.txt")
head(well_locations)

# Load txt file and convert columns to characters for the labels
well_labels <- read_csv("MappingFiles/Well_LocationsDD.txt")
well_labels$OBJECTID <- as.character(well_labels$OBJECTID)
well_labels$PipeHt <- as.character(well_labels$PipeHt)
well_labels$POINT_X <- as.character(well_labels$POINT_X)
well_labels$POINT_Y <- as.character(well_labels$POINT_Y)
head(well_labels)


#Popup labels
pop_ups <- C(well_labels$Well, well_labels$PipeHt, well_labels$POINT_X, well_labels$POINT_Y)


# Load base map with well locations and popups
map <- leaflet(well_locations) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%  
  addMarkers(lng = well_locations$POINT_X, lat = well_locations$POINT_Y, 
             popup = paste("Well ID:", well_labels$Well,"<br>", 
                           "Pipe Height:", well_labels$PipeHt, "<br>",
                           "X Coordinate:", well_labels$POINT_X, "<br>",
                           "Y Coordinate:", well_labels$POINT_Y))
map

