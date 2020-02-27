
library(shiny)
library(tidyverse)
library(lubridate)

# well_data <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/allwelldata.csv")
# Welldata_Names <- as.list(unique(well_data$Well))

#Master_List <- ("C://Master_list_file.csv") #<- this is our local path to that file

Master_List <- data.frame("Well_1" = c("A1", "A1", "A1", "B9", "B9", "B9"), 
                          "Well_2" = c("B3", "B4", "C9", "A12", "C13", "C4"))



###This will be deleted when the app is functional

num_wells <- 5 #5 wells
s_rows <- sample(1:12820210, num_wells) #This will select 5 random rows from within the 12,820,210 rows
exploratory_sample <- well_data[s_rows,] #This pulls only the 5 randomly selected rows from well_data

sample <- c(exploratory_sample$Well) #This is just to view the 5 randomly selected wells and set to a variable

sample_subset <- well_data %>% #This subsets to only include well data from the 5 random wells
    filter(Well == sample[1] | Well == sample[2] | Well == sample[3] | Well == sample[4] | Well == sample[5])
####

dataset <- sample_subset #I would need to have a global file that creates this dataset, or reads it in
Well_Names <- as.list(unique(sample_subset$Well))

# Define UI for application that plots wells for a specific time period
ui <- fluidPage(
    
    # Application title
    titlePanel("Predictive Data Gap Filling"),
    
    # Show a plot of the water level data
        fluidRow(
            column(width = 10, class = "well",
                   h4("To zoom: Click and drag box, then double click. Double click plot to zoom back out."),
                   plotOutput("wellplot", 
                              dblclick = "plot1_dblclick",
                              brush = brushOpts(
                                  id = "plot1_brush",
                                  resetOnNew = TRUE
                              ))),
                column(3,
                       h4("Date Selection"),
                       dateRangeInput("dates", label = h5("Select the date range for the missing data"))),
                column(4, offset = 1,
                       h4("Well Selection"),
                       h5("Well 1 is the well containing missing data that will need to be predicted. Well 2 is the well selected or recommended to be used to predict and fill in Well 1's missing data."),
                       selectInput("Well_1", label = h5("Select Well 1."), 
                                   choices = Well_Names, 
                                   selected = 1),
                       selectInput('Well_2', label = h5("Select Well 2."),
                                   choices = Well_Names,
                                   selected = 1),
                ),
                column(4,
                       actionButton("Download_Data", label = "Download Data")
                )
            ))


# Define server logic required to draw a line plot 
server <- function(input, output) {
    
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    
    ranges <- reactiveValues(x = as.POSIXct(c(start = "2010-08-01", end = "2013-01-10")))
    maxrange <- reactiveValues(x = as.POSIXct(c(start = "2010-08-01", end = "2013-01-10")))
    
    output$wellplot <- renderPlot({
        ID <- strsplit(input$Well_1, " ")[[1]]
        
        #start <- input$date[1]
        
        #end <- input$date[2]
        
        wells <- filter(sample_subset, Well == ID) #, date >= start, date <= end)
        
        #xmin <- input$plot_brush$xmin
        
        #xmax <- input$plot_brush$xmax
        
        ggplot(data = wells, mapping = aes(x = date., y = level, color = Well)) +
            geom_line() +
            scale_y_reverse() +
            ylab("Water Table Depth (cm)") +
            xlab("Date") +
            coord_cartesian(xlim = as.POSIXct(ranges$x, origin = "1970-01-01"), expand = FALSE)+
            #xlim(ranges$x)+
            theme_classic()
    })
    
    # observeEvent(input$plot1_dblclick,
    observeEvent(input$plot1_dblclick,
                 {
                     brush <- input$plot1_brush
                     if (!is.null(brush)) 
                     {
                         ranges$x <- c(brush$xmin, brush$xmax)
                         
                     } else {
                         #ranges$x <- NULL
                         ranges$x <- maxrange$x
                         
                     }
                 })
}

# Run the application 
shinyApp(ui = ui, server = server)