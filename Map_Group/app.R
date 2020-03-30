
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(leaflet)
library(sf)
library(shinyWidgets)


# Define UI for application that plots wells for a specific time period
ui <- fluidPage(
  
  #Application title
  titlePanel("Watershed 3 well visualization"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      #Creates calender to select date range
      dateRangeInput("date", "Select date range:", start = "2014-11-01", end = "2014-12-01",
                     separator = "to", startview = "year"),
      #Text input for wells
      textInput("well_input", "Enter well names with spaces in between",
                value = "JD29"),
      
      # Download Button
      downloadButton("downloadData", "Download")),
    
    mainPanel(
      
      #Plots water table data and precipitation and map
      leafletOutput("map"),
      plotOutput("precplot", width = "80%", height = "200px"),
      plotOutput("wellplot", 
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                 )),
      plotOutput("weirplot")
      
      
    )))


#define server logic to draw line plot
server <- function(input, output, session) {
  
  setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/")
  
  #Read in data
  welldata <- read_csv("welldatahourly.csv") 
  precip <- read_csv("dailyprecip_WS3.csv")
  weir <- read_csv("stream_discharge_WS3.csv")
  
  ranges <- reactiveValues(x = c("2014-11-01", "2014-12-01"))
  
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
  
  #Creates precipitation plot
  output$precplot <- renderPlot({
    
    
    start <- input$date[1]
    
    end <- input$date[2]
    
    
    #Filter for dates selected
    precip_select <- filter(precip, Precip == Precip, DATE >= start, DATE <= end)
    
    
    (ggplot(data = precip_select, mapping = aes(x = DATE, y = Precip))+
        geom_bar(stat = "identity", fill = "#0072B2")+
        ylab("Precipitation (mm)")+
        xlab("Date") +
        theme_classic()) 
    
  })
  
  #Creates weir discharge plot
  output$weirplot <- renderPlot({
    
    
    start <- input$date[1]
    
    end <- input$date[2]
    
    
    #Filter for dates selected
    weir_select <- filter(weir, Streamflow == Streamflow, DATE >= start, DATE <= end)
    
    
    (ggplot(data = weir_select, mapping = aes(x = DATE, y = Streamflow))+
        geom_line()+
        ylab("Weir Discharge (mm)")+
        xlab("Date") +
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
  
  output$map <- renderLeaflet({
    leaflet(well_locations) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%  
      addCircleMarkers(lng = well_locations$POINT_X, lat = well_locations$POINT_Y, 
                       weight = 1,
                       popup = paste("Well ID:", well_labels$Well,"<br>", 
                                     "Pipe Height:", well_labels$PipeHt, "<br>",
                                     "X Coordinate:", well_labels$POINT_X, "<br>",
                                     "Y Coordinate:", well_labels$POINT_Y),
                       layerId = well_locations$Well,
                       radius = 4) %>%
      # focus map in on Hubbard Brooke's Watershed 3 / zoom level
      setView(lng = -71.7210, lat = 43.9582, zoom = 15.5) %>%
      
      # add layers control 
      addLayersControl(overlayGroups = c('Hillshade',
                                         'Slope',
                                         'TWI',
                                         'NDVI'),
                       options = layersControlOptions(collapsed = TRUE),
                       position = 'topright')
  })
  
 
  
  
  
  #Downloadable csv of selected dataset
  
  make_df <- reactive({
    ID <- strsplit(input$well_input, " ")[[1]] #makes dataframe from user selection of data
    
    start <- input$date[1]
    
    end <- input$date[2]
    
    wells <- filter(welldata, Well == ID, date >= start, date <= end)
    
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
 