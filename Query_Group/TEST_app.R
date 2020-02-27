# Loads libraries
  #install.packages("shinyWidgets")
  #install.packages("devtools")
  #devtools::install_github("dreamRs/shinyWidgets")
  #shinyWidgets::shinyWidgetsGallery()
  library(shiny)
  library(devtools)
  library(tidyverse)
  library(lubridate)

# Makes wellNames variable
  wellNames = list('A5', 'A6', 'A7', 'D1', 'H4', 'I3', 'I6', 'I7', 'I8', 'I9', 'JD01', 'JD02', 'JD03', 
                 'JD04', 'JD05', 'JD06', 'JD07', 'JD08', 'JD10', 'JD11', 'JD12', 'JD13', 'JD14', 'JD15',
                 'JD16', 'JD17', 'JD18', 'JD19', 'JD20', 'JD21', 'JD22', 'JD23', 'JD24', 'JD25', 'JD26',
                 'JD27', 'JD28', 'JD29', 'JD30', 'JD31', 'K1', 'K10', 'K11', 'K12', 'K1D', 'K4D', 'K4M',
                 'K4S', 'K5', 'K6D', 'K6S', 'K7D', 'K7S', 'K8', 'K9', 'N1', 'N2', 'N3', 'N4', 'N5', 'O1',
                 'O2', 'P1', 'P2', 'Q1', 'Q2', 'T1')

# Define UI ----
ui <- fluidPage(
  
  # Main title
    titlePanel("Well Subsetting"),
  
  # Select multiple wells based on well names
    selectInput("select", label = h3("Select Well(s)"), 
              choices = wellNames, multiple = TRUE),
  
  # Date selection - Select whether user wants single date or range to choose from
    selectInput('range', "Single Date or Range", 
                    choices = c("Single Date", "Date Range"),
                    selected = "Date Range"),
          
    #if user chose single date, show a single date input box
      conditionalPanel(condition = "input.range == 'Single Date'",
          dateInput("date", "Date:", value = "2010-07-01", 
                    format = "mm/dd/yy")),
            
    #if user chose date range, show a date range input box
      conditionalPanel(condition = "input.range == 'Date Range'",
          dateRangeInput("dRange", "Date Range:", start = "2010-07-01", 
                         end = "2018-01-10", format = "mm/dd/yy")),
    
  # Stream Flow Button
    checkboxInput("checkbox", label = "View Stream Flow Data", value = FALSE),
  
  # Precipitation Slider
    sliderInput("slider", label = h3("Precipitation Range (units)"), min = 0, max = 100, value = c(20, 50)),
  
  # plot and ability to zoom into plot 
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
  
# Sets working directory  
  #################### *** CHANGE THIS *** ####################
    setwd("C:/Users/Katie Dunlap/Documents/EI Capstone/")
  #############################################################
  
  # This section reads in N3.txt for example purposes
    # read in and clean data
    #   well_data <- read_csv("N3.txt")
    # subset <- well_data %>%
    #   filter(date >= "2012-03-01" & date <= "2012-06-01") %>%
    #   select(date, level)
    # subset$level[subset$level  > 162] <- NA # is.na would have worked 
    # subset$level <- subset$level - 94.5
  
  # This reads in the cleanWellData.csv
    well_data <- read_csv("cleanWellData.csv")
  
  # makes range data for slider tool
    ranges <- reactiveValues(x = as.POSIXct(c(start = "2010-07-01", end = "2018-01-01")))
    maxrange <- reactiveValues(x = as.POSIXct(c(start = "2010-07-01", end = "2018-01-01")))
  
  # plot 
    output$wellplot <- renderPlot({
      
      # ID = well number
      #ID <- strsplit(input$wells, " ")[[1]]
      # filters graph by wells selected
      #wells <- filter(well_data, Well == ID)
      
      start_d <- input$dRange[1]
      end_d <- input$dRange[2]
      xmin <- input$plot_brush$xmin
      xmax <- input$plot_brush$xmax
      
      
      ggplot(data = well_data, mapping = aes(x = date, y = level))+
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
    
  # ability to zoom into plot 
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
