library(DT)
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(shinycssloaders)

# import data
setwd("D:/Capstone/data")
wells <- read_csv("hourly.csv")
clusters <- read_csv("dtw_result.csv")
wellsList <- as.list(unique(wells$Well))

ui <- fluidPage(
  titlePanel("Predictive Well Data Gap Filling"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Well Selection"),
      h5("Well 1 is the well containing missing data that will need to be predicted."),
      selectInput("well1", label = h5("Select Well One:"),
                  choices = wellsList),
      uiOutput("Well_2_Req"),
      uiOutput("date_req"),
      checkboxInput("Well_2_Plot", "Check to view Well 2 plot data in grey", FALSE),
      downloadButton("downloadplot", "Download Plot!",
                     style = "background-color:#0dc5c1;
                      color:#FFFFFF;
                      border-color:#D1D1D1;
                      border-style:none;
                      border-width:1px;
                      border-radius:5%;
                      font-size:14px;"),
      downloadButton("downloaddata", "Download Data!",
                     style = "background-color:#0dc5c1;
                      color:#FFFFFF;
                      border-color:#D1D1D1;
                      border-style:none;
                      border-width:1px;
                      border-radius:5%;
                      font-size:14px;")
    ),
    
    mainPanel(
      plotOutput("Plot") %>% withSpinner(color = "#0dc5c1"),
      DT::dataTableOutput("mytable") %>% withSpinner(color = "#0dc5c1")
    )
  )
  
)

server <- function(input, output){
  
  Well_1_input <- reactive({
    wells %>% 
      filter(Well == input$well1) %>% 
      as.data.frame()
  })
  
  output$Well_2_Req <- renderUI({
    
    well_1_cluster <- clusters %>% 
      filter(Well == input$well1) %>% 
      select(Cluster) %>% 
      as.numeric()
    
    well_2_options <- clusters %>% 
      filter(Cluster == well_1_cluster) %>% 
      as.list()
    
    selectInput("Well_2_Selection", label = h5("Select Well 2:"),
                choices = well_2_options,
                selected = 1)
    
  })
  
  Well_2_input <- reactive({
    wells %>% 
      filter(Well == input$Well_2_Selection) %>% 
      as.data.frame()
  })
  
  output$date_req <- renderUI({
    
    Well_1_data <- Well_1_input()
    Well_2_data <- Well_2_input()
    
    min_date <- "2012-01-01"
    max_date <- "2012-06-01"
    
    h4("Date Selection")
    dateRangeInput("dates",
                   label = h5("Select the date range for the missing data:"),
                   min = min_date,
                   max = max_date,
                   start = min_date,
                   end = max_date)
    
  })
  
  Dataset <- reactive({
    
    Well_1_data <- Well_1_input()
    Well_2_data <- Well_2_input()
    
    Well_1_data <- Well_1_data %>% 
      filter(date. >= input$dates[1] & date. <= input$dates[2])
    Well_2_data <- Well_2_data %>% 
      filter(date. >= input$dates[1] & date. <= input$dates[2])
    
    Well_1_data <- Well_1_data %>% 
      select(date., wtdepth)
    Well_2_data <- Well_2_data %>% 
      select(date., wtdepth)
    
    combined <- left_join(Well_2_data, Well_1_data, by = "date.")
    colnames(combined) <- c("date.", "well_2", "well_1")
    
    # interpolation of well 1 based on well 2 data
    combined <- combined %>% 
      mutate(is_predicted = ifelse(is.na(well_1), TRUE, FALSE))
    combined$well_1 <- na.approx(combined$well_1, na.rm = FALSE) #interpolate NAs
    
    return(combined)
  })
  
  # Calculate and plot output
  output$Plot <- renderPlot({
    
    well_data <- Dataset()
      
    if(input$Well_2_Plot == FALSE){
      ggplot() +
        geom_point(data = well_data,
                   mapping = aes(x = date.,
                                 y = well_1,
                                 color = is_predicted)) +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = well_1)) +
        scale_y_reverse() +
        labs(x = "Date",
             y = "Water Table Depth (cm)")
    } else{  # plotting well 1 and well 2
      
      ggplot() +
        geom_point(data = well_data,  # well 1
                   mapping = aes(x = date.,
                                 y = well_1,
                                 color = is_predicted)) +
        geom_line(data = well_data,   # well 2
                  mapping = aes(x = date.,
                                y = well_2,
                  color = "grey70")) +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = well_1)) +
        scale_y_reverse() +
        labs(x = "Date",
             y = "Water Table Depth (cm)")
      
    }
  })
  
  # table with water depth and predictions results
  output$mytable = DT::renderDataTable({
    
    well_data <- Dataset()
    
    # print table
    well_data
  })
  
  output$downloadplot <- downloadHandler(
    filename <- function(){
      paste('plot', 'png', sep = ".")
    },
    content <- function(file){
      png(file)
      
      well_data <- Dataset()
      
      plot <- ggplot()
      
      if(input$Well_2_Plot == FALSE){  # only plotting well 1
        
        plot <- ggplot() +
          geom_point(data = well_data,
                     mapping = aes(x = date.,
                                   y = well_1,
                                   color = is_predicted)) +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = well_1)) +
          scale_y_reverse() +
          labs(x = "Date",
               y = "Water Table Depth (cm)")
      } else{  # plotting well 1 and well 2
        
        plot <- ggplot() +
          geom_point(data = well_data,  # well 1
                     mapping = aes(x = date.,
                                   y = well_1,
                                   color = is_predicted)) +
          geom_line(data = well_data,   # well 2
                    mapping = aes(x = date.,
                                  y = well_2,
                                  color = "grey70")) +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = well_1)) +
          scale_y_reverse() +
          labs(x = "Date",
               y = "Water Table Depth (cm)")
      }
        
        print(plot)
        dev.off()
      
    },
    
    contentType = "image/png"
  )
  
  output$downloaddata <- downloadHandler(
    filename = function() {"Well_1_Predicted_data.csv"}, content = function(file){
      write.csv(Dataset(), file, row.names = FALSE)
    }
  )
 
}

# Run the application 
shinyApp(ui = ui, server = server)
