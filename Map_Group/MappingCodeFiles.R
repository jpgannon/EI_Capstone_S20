library(tidyverse)
library(leaflet)
library(raster)
library(rgdal)



# update raster package
#install.packages('raster', repos = 'http://r-forge.r-project.org/', type = 'source')

# Set working directory
 #setwd("~/VirginiaTech/Capstone/Working_Directory")

# Load the txt file for mapping
well_locations <- read_csv("Well_location_data.csv")


# Load txt file and convert columns to characters for the labels
well_labels <- well_locations %>%
  mutate(OBJECTID = as.character(OBJECTID)) 


# read in TWI raster file 
twi <- raster('ws3cliptwid.tif')

# read in WS3 outline (.shp) and assign coordinate system
 ws3 <- readOGR("~/VirginiaTech/Capstone/Working_Directory/ws3.shp") %>%
         spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
 plot(ws3)
 
# read in WS3 streams (.shp)
 ws3streams <- readOGR("~/VirginiaTech/Capstone/Working_Directory/Shapefiles/ws3streams_proj.shp") %>%
                spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
 plot(ws3streams)
 #ws3streams[ws3streams$StrType == E, "StrType"]
# streams_fix <- ws3streams %>% 
 #  replace(as.character(StrType), "E", "Ephemeral")
 
 
 #set legend/colors for streams
 streams_col <- colorFactor(palette(c("Purple", "Green", "Orange")), domain = ws3streams$StrType, 
                         na.color = 'transparent')
 
# read in UAA raster
 uaa <- raster('ws3uaab.tif')
 plot(uaa)
 
 # set UAA colors
 uaa_pal <- colorBin("Reds", domain = NULL, bins = 5, na.color = "transparent")
 
 

# set twi colors for map
pal <- colorBin("Blues", domain = NULL, bins = 5, na.color = "transparent")

# set color scale for legend
pal2 <- colorNumeric(palette = "Blues", domain = vals)

#Popup labels
pop_ups <- well_raster_vals

# read in slope
ws3slope <- raster('~/VirginiaTech/Capstone/Working_Directory/Raster_files/ws3slope2')

# set slope colors for map
pal_slope <- colorBin("Reds", domain = NULL, bins = 5, na.color = "transparent")

#read in soil hpu
soil <- raster('~/VirginiaTech/Capstone/Working_Directory/Raster_files/ws3hpu_newallc.tif')
pal_soil <- colorBin(c("Blue", "Green", "Brown"), domain = NULL, bins = 5, na.color = "transparent")
plot(soil)

# remove NA's from rasters
vals <- values(na.omit(twi))
slope_vals <- values(na.omit(ws3slope))
uaa_vals <- values(na.omit(uaa))
soil_vals <- values(na.omit(soil))
#stream_vals <- values(na.omit(ws3streams))
  
  # Map; both rasters & shapefile with markers, legend, and layer control
map <- leaflet(well_locations) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>% 
  addRasterImage(twi, colors = pal, opacity = 0.5, group = "Topographic Wetness Index") %>%
  addRasterImage(ws3slope, colors = pal_slope, opacity = 0.5, group = "Slope") %>%
  addRasterImage(uaa, colors = uaa_pal, opacity = 0.5, group = "Upslope Accumulated Area") %>%
  addRasterImage(soil, colors = pal_soil, opacity = 0.5, group = "Soil") %>%
  addPolygons(data = ws3, color = "Black", fill = FALSE) %>%
  addPolylines(data = ws3streams, color = streams_col, group = "Streams") %>%
  addCircleMarkers(lng = well_locations$POINT_X, lat = well_locations$POINT_Y,
             color = "Black", popup = paste("Well ID:", well_locations$Well,
                                             "Distance to Streams:", well_locations$DistanceToStreams, 
                                            "Distance to Bedrock:", well_locations$DistanceToBedrock,
                                            "TWI:", well_locations$TWI,
                                             "Slope:", well_locations$Slope,
                                            "UAA:", well_locations$UAA_Weighted)) %>%
  addLegend(position = 'topright', values = vals, pal = pal2, labFormat = labelFormat(),
            title = "Topographic <br> Wetness Index", group = "Topographic Wetness Index") %>%
  addLegend(position = 'topright', values = ws3streams$StrType, pal = streams_col, 
           labFormat = labelFormat(c("Ephemeral", "Intermittent", "Perennial")), title = "Streams", group = "Streams") %>%
  addLegend(position = 'topright', values = slope_vals, group = "Slope", pal = pal_slope, labFormat = labelFormat(),
            title = "Slope") %>%
  addLegend(position = 'topright', values = uaa_vals, pal = uaa_pal, group = "Upslope Accumulated Area", title = "UAA") %>%
  addLegend(position = 'topright', values = soil_vals, pal = pal_soil, group = "Soil", title = "Soil HPU") %>%
  addLayersControl(baseGroups = c("Topographic Wetness Index", "Slope", "Soil", "Upslope Accumulated Area"),
                     overlayGroups = c("Streams"),
                   options = layersControlOptions(collapsed = TRUE))%>%
  hideGroup(c("Topographic Wetness Index", "Slope", "Upslope Accumulated Area", "Soil", "Streams"))
map

