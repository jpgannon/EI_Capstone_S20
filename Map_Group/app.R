library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(sf)
library(shinythemes)
library(raster)
library(leaflet.extras)

#library(rgdal)



# Define UI for application that plots wells for a specific time period
ui <- fluidPage(
  
  #Set theme
  theme = shinytheme("flatly"),
  
  #Application title
  titlePanel("Watershed 3 Well Visualization"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      
      h6("Precipitation, streamflow, and water table level for watershed 3 of the Hubbard Brook Experimental Forest"),
      h6(strong("Location:"), "White Mountain National Forest, NH, USA"),
      h6(strong("Forest type:"), "Northern hardwood"),
      h6(strong("Area:"), "104.71 acres (42.4 ha)"),
      h6(strong("Elevation:"), "527-732 m"),
      h6(strong("Climate:"), "Temperate continental"),
      h5(strong("To Zoom: On any plot, click and drag and then double click. 
                      Double click again to zoom to full extent.")),
      
      #change the size of the text in the plot using a slider
      sliderInput(inputId = "mag", label = "Plot text size.", min = 11, max = 24, value = 12),
      
      
      #Creates calender to select date range
      dateRangeInput("date", "Select date range:", start = "2007-08-10", end = "2018-10-08",
                     separator = "to", startview = "year"),
      #Text input for wells
      textInput("well_input", "Enter well names with spaces in between or select from map",
                value = "JD29"),
      
      
      # Download Button
      downloadButton("downloadData", "Download"),
      actionButton("appGuide", "Show App User Guide"),
      
      #adds blank line to separate action buttons from map
      h6(""),
      
      #plots the map
      leafletOutput("map")),
  
  
  
  mainPanel(
    
    
    #Plots the graphs
    
    plotOutput("precplot", width = "90%", height = "150px",
               dblclick = "plot1_dblclick",
               brush = brushOpts(
                 id = "plot1_brush",
                 resetOnNew = TRUE)),
    plotOutput("wellplot", width = "90%", height = "225px",
               dblclick = "plot1_dblclick",
               brush = brushOpts(
                 id = "plot1_brush",
                 resetOnNew = TRUE
               )),
    plotOutput("weirplot",width = "90%", height = "225px",
               dblclick = "plot1_dblclick",
               brush = brushOpts(
                 id = "plot1_brush",
                 resetOnNew = TRUE))
    
    
  )))


#define server logic to draw line plot
server <- function(input, output, session) {
  
  setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/test_app")
  
  #Read in data
  welldata <- read_csv("welldatahourly.csv") 
  precip <- read_csv("dailyprecip_WS3.csv")
  weir <- read_csv("stream_discharge_WS3.csv")
  
  #renames level column for clarity when downloading csv of data
  welldata <- welldata %>%
    rename(level_w_pipe_height = level)
  
  #creates date range
  ranges <- reactiveValues(x = c("2007-08-10", "2018-10-08"))
  maxrange <- reactiveValues(x = c("2007-08-10", "2018-10-08"))
  

  
  #Creates water table plot
  output$wellplot <- renderPlot({
    
    #Creates ID from text input
    ID <- strsplit(input$well_input, " ")[[1]]
    
    #Filters for chosen well ID
    wells <- filter(welldata, Well == ID) 
    
    ggplot(data = wells, mapping = aes(x = date, y = wtdepth, color = Well))+
      geom_line()+
      scale_y_reverse()+
      ylab("Water Table Depth (cm)")+
      coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
      theme_classic()+
      theme(legend.position = "bottom") +
      theme(text = element_text(size=input$mag)) +
      theme(axis.title.x=element_blank()) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
    
  })
  
  
  
  #Creates precipitation plot
  output$precplot <- renderPlot({
    
    
    #converts date column to match well data
    precip <- precip %>%
      mutate(DATE = as.POSIXct(DATE))
    
    
   
    (ggplot(data = precip, mapping = aes(x = DATE, y = Precip))+
        geom_bar(stat = "identity", fill = "#0072B2")+
        ylab("Precipitation (mm)")+
        scale_y_reverse()+
        coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
        theme_classic()) +
        theme(text = element_text(size=input$mag)) +
        theme(axis.title.x=element_blank())
    
  })
  
  
  #Creates weir discharge plot
  output$weirplot <- renderPlot({
    
    
    #Converts date column to match well data
    weir <- weir %>%
      mutate(DATE = as.POSIXct(DATE))
    
    
    
    (ggplot(data = weir, mapping = aes(x = DATE, y = Streamflow))+
        geom_line()+
        ylab("Weir Discharge (mm)")+
        coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
        theme_classic()) +
        theme(text = element_text(size=input$mag)) +
        theme(axis.title.x=element_blank())
    
    
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
  
  # read in raster file
  twi <- raster('ws3cliptwid.tif')
  
  # read in WS3 outline (.shp) and assign coordinate system
  ws3 <- st_read("ws3.shp")
  ws3 <- st_transform(ws3, "+proj=longlat +datum=WGS84 +no_defs")
  
  
  # remove NA's for the color scheme
  vals <- values(na.omit(twi))
  
  # set twi colors for map
  pal <- colorBin("Blues", domain = NULL, bins = 5, na.color = "transparent")
  
  # set color scale for legend
  pal2 <- colorNumeric(palette = "Blues", domain = vals, na.color = NA)
  
  # read in hillshade
  ws3hill <- raster('ws3_hillshade2.tif')
  
  # set hillshade colors for map
  pal_hill <- colorBin("Greys", domain = NULL, bins = 5, na.color = NA)
  
  
  output$map <- renderLeaflet({
    leaflet(well_locations) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(twi, colors = pal, opacity = 0.5, group = "Topographic Wetness Index") %>%
      addRasterImage(ws3hill, colors = pal_hill, opacity = 0.7, group = "Hillshade") %>%
      addPolylines(data = ws3, color = "Black", fill = FALSE, weight = 2, fillOpacity = 0.2) %>%
      addCircleMarkers(layerId = well_locations$Well, lng = well_locations$POINT_X, lat = well_locations$POINT_Y,
                       color = "Black",
                       popup = paste("Well ID:", well_labels$Well,"<br>"),
                       radius = 4,
                       fillOpacity = 0.4,
                       stroke = FALSE) %>%
      addLegend(position = 'topright', values = vals, pal = pal2, labFormat = labelFormat(),
                title = "Topographic Wetness Index", 
                group = "Topographic Wetness Index" ) %>%
      addLayersControl(overlayGroups = c("Topographic Wetness Index", "Hillshade"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Hillshade", "Topographic Wetness Index")) %>%
      
      #focus map in on Hubbard Brooke's Watershed 3 / zoom level
      setView(lng = -71.7170, lat = 43.9578, zoom = 15.1) %>%
      
      #Resets map view
      addResetMapButton()
    
    
  })
  
  #Selecting map markers to retrieve well ID
  observeEvent(input$map_marker_click, { 
    site <- input$map_marker_click
    site_id <- site$id
    updateTextAreaInput(session, "well_input", value = paste(input$well_input, site_id))
  })
  
 
  
  #Downloadable csv of selected dataset
  
  make_df <- reactive({
    ID <- strsplit(input$well_input, " ")[[1]] #makes dataframe from user selection of data
    
    #Filter for subset of data from user selection
    wells <- filter(welldata, Well == ID, date >= ranges$x[1], date <= ranges$x[2])
    
  })
  output$results <- renderTable({make_df()})
  
  
  
  #Downloads subset of well data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(make_df(), file, row.names = FALSE)
    })
  
  #Updates date range when date is selected
  observeEvent(input$date, {
    ranges$x <- c(input$date[1], input$date[2])
  })
  
  
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    
    
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
    } 

    
    else {
      ranges$x <- c(input$date[1], input$date[2])
    }
  }
  )

  
}



# Runs the app
app <- shinyApp(ui, server)
runApp(app)

