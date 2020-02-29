#setwd("C:/Users/lizaw/Desktop/Capstone/")

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(leaflet)


# Define UI for application that plots wells for a specific time period
ui <- fluidPage(
  
  #Application title
  titlePanel("Watershed 3 well visualization"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      #Creates calender to select date range
      dateRangeInput("date", "Select date range:", start = "2010-08-10", end = "2018-10-08",
                     separator = "to", startview = "year"),
      #Text input for wells
      textInput("wells", "Enter well names with spaces in between",
                value = "K9")
      
      
      
    ),
    mainPanel(
      
      #Plots water table data and precipitation and map
      leafletOutput("map"),
      plotOutput("wellplot"),
      plotOutput("precplot")
    )
    
  )
)





#define server logic to draw line plot
server <- function(input, output) {
  
  #setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/")
  
  #Read in data
  welldata <- read_csv("well_data.csv") #filter(Well == "A5")
  precip <- read_csv("dailyWatershedPrecip1956-2019.csv")
  
  
  
  
  precipWS3 <- precip %>%
    filter(precip$watershed == 3) %>% #Filter for watershed 3
    mutate(Precip = Precip * .1) #Convert mm to cm to match water level units
  
  
  #Creates water table plot
  output$wellplot <- renderPlot({
    ID <- strsplit(input$wells, " ")[[1]]
    
    start <- input$date[1]
    
    end <- input$date[2]
    
    wells <- filter(welldata, Well == ID, date >= start, date <= end)
    
    ggplot(data = wells, mapping = aes(x = date, y = wtdepth, color = Well))+
      geom_line()+
      scale_y_reverse()+
      ylab("Water Table Depth (cm)")+
      xlab("Date") +
      theme_classic()
    
    
  })
  #Creates precipitation plot
  output$precplot <- renderPlot({
    
    
    start <- input$date[1]
    
    end <- input$date[2]
    
    
    #Filter for dates selected
    precip_select <- filter(precipWS3, Precip == Precip, DATE >= start, DATE <= end)
    
    
    ggplot(data = precip_select, mapping = aes(x = DATE, y = Precip))+
      geom_line()+
      ylab("Precipitation (cm)")+
      xlab("Date") +
      scale_y_reverse()+
      theme_classic()
  })
  
  
  # Load the txt file for mapping
  well_locations <- read_csv("well_locationsDD.txt")
  
  
  # Load txt file and convert columns to characters for the labels
  well_labels <- well_locations %>%
    mutate(OBJECTID = as.character(OBJECTID)) %>%
    mutate(PipeHt = as.character(PipeHt)) %>%
    mutate(POINT_X = as.character(POINT_X)) %>%
    mutate(POINT_Y = as.character(POINT_Y))
  
  
  
  #Popup labels
  pop_ups <- c(well_labels$Well, well_labels$PipeHt, well_labels$POINT_X, well_labels$POINT_Y)
  
  output$map <- renderLeaflet({
    leaflet(well_locations) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%  
      addCircles(lng = well_locations$POINT_X, lat = well_locations$POINT_Y, 
                 weight = 1,
                 popup = paste("Well ID:", well_labels$Well,"<br>", 
                               "Pipe Height:", well_labels$PipeHt, "<br>",
                               "X Coordinate:", well_labels$POINT_X, "<br>",
                               "Y Coordinate:", well_labels$POINT_Y)) %>%
      # focus map in on Hubbard Brooke's Watershed 3 / zoom level
      setView(lng = -71.7210, lat = 43.9582, zoom = 15.5) %>%
      
      # add layers control 
      addLayersControl(overlayGroups = c('Hillshade',
                                         'Slope',
                                         'TWI',
                                         'NDVI'),
                       options = layersControlOptions(collapsed = FALSE),
                       position = 'topright')
  })
  
  
}


app <- shinyApp(ui, server)
runApp(app)


