setwd("C:/Users/lizaw/Desktop/Capstone/")


#install.packages("leaflet")

library(leaflet)
library(tidyverse)
library(lubridate)

# Load the txt file
well_locations <- read_csv("wells/Well_LocationsDD.txt")
head(well_locations)

# Load base map with well locations, circle markers
  leaflet(data = well_locations[1:4,]) %>% 
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%  # Add default OpenStreetMap map tiles
  addCircles(lng = well_locations$POINT_X, lat = well_locations$POINT_Y, weight= 1) %>%
  
# focus map in on Hubbard Brooke's Watershed 3 / zoom level
  setView(lng = -71.7210, lat = 43.9582, zoom = 15.5) %>%
    
# add layers control 
  addLayersControl(overlayGroups = c('Hillshade',
                                       'Slope',
                                       'TWI',
                                       'NDVI'),
                     options = layersControlOptions(collapsed = FALSE),
                     position = 'topright')
#Code adapted from https://hansenjohnson.org/post/interactive-maps-in-r/
  
  
    
 