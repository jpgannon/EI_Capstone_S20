#install.packages("shinyWidgets")
#install.packages("devtools")
#devtools::install_github("dreamRs/shinyWidgets")
#shinyWidgets::shinyWidgetsGallery()
library(shiny)
library(devtools)

# Define UI ----
ui <- fluidPage(
  
  # Main title
  titlePanel("Well Subsetting"),
  
  # Select multiple wells
  selectInput("select", label = h3("Select Well(s)"), 
              choices = list("1" = 1, "2" = 2, "3" = 3), multiple = TRUE),
  # Date Range
  dateRangeInput("dates", label = h3("Date Range")),
  
  # Stream Flow Button
  checkboxInput("checkbox", label = "View Stream Flow Data", value = FALSE),
  
  # Precipitation Slider
  sliderInput("slider", label = h3("Precipitation Range (units)"), min = 0, max = 100, value = c(20, 50)),
  
  # plot 
  fluidRow(
    column(width = 10, offset = 1, class = "well",
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
  
  setwd("C:/Users/Katie Dunlap/Documents/EI Capstone/")
  
  # read in and clean data
  well_data <- read_csv("N3.txt")
  subset <- well_data %>%
    filter(date >= "2012-03-01" & date <= "2012-06-01") %>%
    select(date, level)
  subset$level[subset$level  > 162] <- NA # is.na would have worked 
  subset$level <- subset$level - 94.5
  
  # plot
  output$wellplot <- renderPlot({
    
    # ID = well number
    #ID <- strsplit(input$wells, " ")[[1]]
    # filters graph by wells selected
    #wells <- filter(well_data, Well == ID)
    
    ggplot(data = subset, mapping = aes(x = date, y = level))+
      geom_line()+
      scale_y_reverse()+
      ylab("Water Table Depth (cm)")+
      scale_y_reverse() +
      theme_classic()
  })
  
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
