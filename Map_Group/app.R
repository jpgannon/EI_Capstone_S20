# A shiny web app to explore the water table, precipitation, and discharge record
# of watershed 3 at the Hubbard Brook Experimental Forest in North Woodstock, NH
# Macey O'Neill, Rachel Melton, Liza White, Ryan Whalen
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(sf)
library(shinythemes)
library(raster)
library(leaflet.extras)

#library(rgdal)



# Define UI for application
ui <- fluidPage(
  
  #Set theme
  theme = shinytheme("flatly"),
  
  #Application title
  titlePanel("Watershed 3 Well Visualization"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      
      #Creates text for general information
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
      
      
      # Creates download, app guide, and FAQ buttons 
      downloadButton("downloadData", "Download"),
      h6(""),
      actionButton("appGuide", "App User Guide"),
      actionButton("background", "App Background Information"),
      
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


#define server logic
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
  
  # Pop up box if user clicks "App User Guide"
  observeEvent(input$appGuide, {
    showModal(modalDialog(
      title = "How To Use the App:",
      HTML("<b> 1. Set date range </b> <br>
        Select range via calender, or type in yyyy-mm-dd format <br> 
          <b> 2. Select a Well </b> <br>
            You can select wells to plot via the interactive map by clicking on the well or just typing in the well name with spaces between. <br>
            If you select too many wells you can erase with the backspace. <br>
          <b> 4. View the plots </b> <br>
          <b> 5. Brushing to zoom in on plot </b> <br>
          Click and drag a box to set x axis range, then double click to zoom to range.  <br>
          Double click again to reset date to max  date range. </b> <br>
           <b> 6. Switch layers </b> <br>
           To change layers on the map, go to layers icon and click to view available layers. <br>
           Check or uncheck the boxes to turn the layers on or off. </b> <br>
           <b> 7. Reset Map View </b> <br> 
           To reset the map, click the button below the zoom buttons on the map. <br>
          <b> 8. Screenshot map </b> <br>
          To screenshot the plots/map, use your computer's default screenshotting tool (e.g. Snip & Sketch, Snipping Tool, etc.) <br>
          <b> 9. Download selected data </b> <br>
          To download data, follow the steps prior and then click the Download Data button. <br>
          Navigate to the desired folder on your computer, then name the file and click save."
      )
    ))
  })
  
  # Pop up box if user clicks "FAQ"
  observeEvent(input$background, {
    showModal(modalDialog(
      title = "Application Background",
      HTML("<b> 1. Background of App </b> <br> 

<b> The map shows an overview of watershed 3 and has interactive icons on the map representing the wells in Watershed 3. The user will be able to
see the map that shows all the wells and then the user can zoom in on a well to get access to information. When looking at data from a well, the user 
can choose to look at data from a range of days by selecting dates on an interactive calendar. In addition to groundwater measurements, the map 
will also allow the user to view soil composition in the watershed and to also query multiple parameters such as precipitation and water depth by clicking
on checkboxes. The plots will include precipitation (upside down car plot), discharge from watershed 3, and water depth for individual wells. Multiple wells 
can be selected to compare their plots. When looking at the map, the user will be able to turn on different layers showing landscape metrics such as Topographic Wetness Index,
stream networks, and Upslope Accumulation Area. The user can also select the wells to show this information for individual wells. There will be an export
button on the plot tab so the user can export the data that was subsetted. <br> <br>

<b> 2. Variables and Units </b> <br>

<b>
The first optional raster layer that the user can enable is a Topographic Wetness Index (TWI),which shows preferential flow paths of water during storm events. 
This is a useful raster layer for users because it allows the user to visualize how the water drains down the surrounding slopes into the basin. 
The second optional raster layer is slope which shows how steep the slopes within the watershed are and can also help the user visualize how the 
water drains out of the watershed. The third optional raster layer was Upslope Accumulated Area (UAA) which highlights the areas that could produce run off
to the area of interest which, in this case, is Watershed 3. The fourth optional raster is soil HPU. The final layer is the stream network in the watershed 
color coded by stream type including ephemeral, intermittent, and perennial streams.The first graph displays daily precipitation in millimeters, 
the second graph displays groundwater level in centimeters, and the third graph displays daily weir discharge in millimeters. <br> <br>

<b> 3. Attributions </b> <br>
         
<b> We would like to acknowledge our team members who worked effortlessly to create this app: Macey O'Neill, Rachel Melton, Liza White, Ryan Whalen, and our 
unofficial but honorary member Dr. JP Gannon. We also want to thank Dr. Robert Settlage with advanced research computing at VT who got our server up and running. 
Finally, we want to thank Hubbard Brooke Researchers for collecting and giving us the information we needed to create our app!"
           
      )
    ))
  })
  

  
}



# Runs the app
app <- shinyApp(ui, server)
runApp(app)

