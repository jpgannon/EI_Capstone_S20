library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(leaflet)
library(sf)
library(shinyWidgets)
library(shinythemes)
library(raster)
library(htmltools)
library(rgdal)
library(leaflet.extras)



# Define UI for application that plots wells for a specific time period
ui <- fluidPage(
  
  #Set theme
  theme = shinytheme("flatly"),
  
  #Application title
  titlePanel("Watershed 3 Well Visualization"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h5("Explore Precipiation, Streamflow, and Water Table level for watershed 3 at the Hubbard Brook Experimental Forest"),
      h5(strong("Location"), "White Mountain National Forest, NH, USA"),
      h5(strong("TO ZOOM: On any plot, click and drag and then double click. 
                      Double click again to zoom to full extent.")),
      
      #Creates calender to select date range
      dateRangeInput("date", "Select date range:", start = "2007-08-10", end = "2018-10-08",
                     separator = "to", startview = "year"),
      #Text input for wells
      textInput("well_input", "Enter well names with spaces in between or select from map",
                value = "JD29"),
      
      # Download Button
      downloadButton("downloadData", "Download")),
    
    mainPanel(
      
      #Plots the map and graphs
      leafletOutput("map", width = "100%", height = 350),
      plotOutput("precplot", width = "100%", height = "150px",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE)),
      plotOutput("wellplot", width = "100%", height = "200px",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                 )),
      plotOutput("weirplot",width = "100%", height = "200px",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE))
      
      
    )))


#define server logic to draw line plot
server <- function(input, output, session) {
  
  setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/")
  
  #Read in data
  welldata <- read_csv("welldatahourly.csv") 
  precip <- read_csv("dailyprecip_WS3.csv")
  weir <- read_csv("stream_discharge_WS3.csv")

  
  
  #creates date range
  ranges <- reactiveValues(x = c("2007-08-10", "2018-10-08"))
  
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
      
    } else {
      ranges$x <- c(input$date[1], input$date[2])
    }
  }
  )
  
 
 
  #Creates water table plot
  output$wellplot <- renderPlot({
    ID <- strsplit(input$well_input, " ")[[1]]
    
    #start <- input$date[1]
    
    #end <- input$date[2]

    
    wells <- filter(welldata, Well == ID) #, date >= start, date <= end)
    
    ggplot(data = wells, mapping = aes(x = date, y = wtdepth, color = Well))+
        geom_line()+
        scale_y_reverse()+
        ylab("Water Table Depth (cm)")+
        xlab("Date") +
        coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
        theme_classic()+
        theme(legend.position = "bottom")

  })
  
 
  #Selecting map markers
  observeEvent(input$map_marker_click, { 
    site <- input$map_marker_click
    site_id <- site$id
    updateTextAreaInput(session, "well_input", value = paste(input$well_input, site_id))
  })
  

  #converts date column to match well data
  precip <- precip %>%
    mutate(DATE = as.POSIXct(DATE))
  
  
  #Creates precipitation plot
  output$precplot <- renderPlot({
    
    
    #start <- input$date[1]
    
    #end <- input$date[2]
    
    
    #Filter for dates selected
    precip_select <- filter(precip, Precip == Precip)# DATE >= ranges$x[1], DATE <= ranges$x[2] ) #, DATE >= start, DATE <= end)
    
    #Calculates precip tota;
    #Ptotal <- round(sum(precip_select$Precip , na.rm = TRUE),2)
    
    # Create text
    #grob <- grobTree(textGrob(paste("Total Precip:", Ptotal, "mm"), x=0.1,  y=0.1, hjust=0,
    #                          gp=gpar(col="black", fontsize=13, fontface="italic")))
    
    
    (ggplot(data = precip_select, mapping = aes(x = DATE, y = Precip))+
        geom_bar(stat = "identity", fill = "#0072B2")+
        ylab("Precipitation (mm)")+
        xlab("Date") +
        scale_y_reverse()+
        coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
        theme_classic())
        #annotation_custom(grob)
    

  })
  
  #Converts date column to match well data
  weir <- weir %>%
    mutate(DATE = as.POSIXct(DATE))
  
  
  #Creates weir discharge plot
  output$weirplot <- renderPlot({
    
    
    #start <- input$date[1]
    
    #end <- input$date[2]
    
    
    #Filter for dates selected
    weir_select <- filter(weir, Streamflow == Streamflow) #, DATE >= start, DATE <= end)
    
    
    (ggplot(data = weir_select, mapping = aes(x = DATE, y = Streamflow))+
        geom_line()+
        ylab("Weir Discharge (mm)")+
        xlab("Date") +
        coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
        theme_classic())
      
    
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
  pal2 <- colorNumeric(palette = "Blues", domain = vals)
  
  # read in hillshade
  ws3hill <- raster('ws3_hillshade2.tif')
  
  # set hillshade colors for map
  pal_hill <- colorBin("Greys", domain = NULL, bins = 5, na.color = "transparent")
  
  output$map <- renderLeaflet({
    leaflet(well_locations) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(twi, colors = pal, opacity = 0.5, group = "Topographic Wetness Index") %>%
      addRasterImage(ws3hill, colors = pal_hill, opacity = 0.7, group = "Hillshade") %>%
      addPolygons(data = ws3, color = "Black", fill = FALSE) %>%
      addCircleMarkers(lng = well_locations$POINT_X, lat = well_locations$POINT_Y,
                       color = "Black",
                       popup = paste("Well ID:", well_labels$Well,"<br>",
                                     "Pipe Height:", well_labels$PipeHt, "<br>",
                                     "X Coordinate:", well_labels$POINT_X, "<br>",
                                     "Y Coordinate:", well_labels$POINT_Y),
                       radius = 2.5) %>%
      addLegend(position = 'topright', values = vals, pal = pal2, labFormat = labelFormat(),
                title = "Topographic Wetness Index") %>%
      addLayersControl(baseGroups = c("Topographic Wetness Index", "Hillshade"),
                       options = layersControlOptions(collapsed = TRUE)) %>%

      #focus map in on Hubbard Brooke's Watershed 3 / zoom level
      setView(lng = -71.7190, lat = 43.9582, zoom = 15.2) %>%
      addResetMapButton()
   
  })
  
 
  
  
  
  #Downloadable csv of selected dataset
  
  make_df <- reactive({
    ID <- strsplit(input$well_input, " ")[[1]] #makes dataframe from user selection of data
    
    #start <- input$date[1]
    
    #end <- input$date[2]
    
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
  
 
  
}



# Runs the app
app <- shinyApp(ui, server)
runApp(app)
 
