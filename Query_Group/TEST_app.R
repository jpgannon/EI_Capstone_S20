#install.packages("shinyWidgets")
#install.packages("devtools")
#devtools::install_github("dreamRs/shinyWidgets")
#shinyWidgets::shinyWidgetsGallery()
library(shiny)
library(devtools)
library(tidyverse)
library(lubridate)

# Define UI ----
ui <- fluidPage(
  
  # Main title
  titlePanel("Well Subsetting"),
  
  # Select multiple wells
  # needs up to date well names
    selectInput("select", label = h3("Select Well(s)"), 
              choices = list("1" = 1, "2" = 2, "3" = 3), multiple = TRUE),
  
  # Date selection
      #Select whether user wants single date or range to choose from
      selectInput('range', "Single Date or Range", 
                      choices = c("Single Date", "Date Range"),
                      selected = "Date Range"),
          
      #if user chose single date, show a single date input box
      conditionalPanel(condition = "input.range == 'Single Date'",
          dateInput("date", "Date:", value = "2010-08-01", 
                    format = "mm/dd/yy")),
          
      #if user chose date range, show a date range input box
      conditionalPanel(condition = "input.range == 'Date Range'",
          dateRangeInput("dRange", "Date Range:", start = "2010-08-01", 
                         end = "2013-01-10", format = "mm/dd/yy")),
  
  # Stream Flow Button
  checkboxInput("checkbox", label = "View Stream Flow Data", value = FALSE),
  
  # Precipitation Slider
  sliderInput("slider", label = h3("Precipitation Range (units)"), min = 0, max = 100, value = c(20, 50)),
  
  # plot and ability to zoom into plot (DO NOT TOUCH plz - katie)
  fluidRow(column(width = 10, offset = 1, class = "well",
           h4("To zoom: Click and drag box, then double click. Double click plot to zoom back out."),
           plotOutput("wellplot", 
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush",
                        resetOnNew = TRUE
                      )))
  )
)

# Define server logic ----
server <- function(input, output) {
  
#################### *** CHANGE THIS *** ####################
  setwd("C:/Users/Katie Dunlap/Documents/EI Capstone/")
#############################################################
  
  # read in and clean data
  well_data <- read_csv("N3.txt")
  subset <- well_data %>%
    filter(date >= "2012-03-01" & date <= "2012-06-01") %>%
    select(date, level)
  subset$level[subset$level  > 162] <- NA # is.na would have worked 
  subset$level <- subset$level - 94.5
  
  # makes range data for slider tool
  ranges <- reactiveValues(x = as.POSIXct(c(start = "2010-08-01", end = "2013-01-10")))
  maxrange <- reactiveValues(x = as.POSIXct(c(start = "2010-08-01", end = "2013-01-10")))
  
  # plot (DO NOT TOUCH plz - katie)
  output$wellplot <- renderPlot({
    
    # ID = well number
    #ID <- strsplit(input$wells, " ")[[1]]
    # filters graph by wells selected
    #wells <- filter(well_data, Well == ID)
    
    start_d <- input$dRange[1]
    end_d <- input$dRange[2]
    xmin <- input$plot_brush$xmin
    xmax <- input$plot_brush$xmax
    
    
    ggplot(data = subset, mapping = aes(x = date, y = level))+
      geom_line()+
      scale_y_reverse()+
      ylab("Water Table Depth (cm)")+
      xlab("Date") + 
        # makes the auto zoom work, but manual input date doesnt work
      # coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+ 
        # makes the manual input date work, but the auto zoom doesnt work (also single date doesnt work)
      coord_cartesian(xlim = c(as.POSIXct(input$dRange[1]), as.POSIXct(input$dRange[2])))+
      theme_classic()
  })
  
  # ability to zoom into plot (DO NOT TOUCH plz - katie)
  observeEvent(input$plot1_dblclick,
               {
                 brush <- input$plot1_brush
                 if (!is.null(brush)) 
                 {
                   ranges$x <- c(brush$xmin, brush$xmax)
                   
                 } else {
                   ranges$x <- maxrange$x
                 }
               })
}

# Run the app ----
shinyApp(ui = ui, server = server)
