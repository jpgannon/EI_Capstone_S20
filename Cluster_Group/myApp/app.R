library(DT)
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(shinycssloaders)

# import data
setwd("D:/Capstone/data")
wells <- read_csv("oneHourSummary.csv")
clusters <- read_csv("clusters_with_HPU.csv")
wellsList <- as.list(unique(wells$Well))
prediction_choices <- c("Interpolation", "Linear-Regression")

ui <- fluidPage(
  titlePanel("Predictive Well Data Gap Filling"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("filling_choice", label = h5("Select a gap filling method:"),
                  choices = prediction_choices),
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
    
    if(well_1_cluster == 5){
      well_2_options <- clusters %>%
        select(Well, Cluster) %>% 
        as.list()
    }else{
    well_2_options <- clusters %>% 
      filter(Cluster == well_1_cluster) %>%
      select(Well, Cluster) %>% 
      as.list()
    }
    
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
    
    min_date <- "2007-08-10"
    max_date <- "2018-10-08"
    
    h4("Date Selection")
    dateRangeInput("dates",
                   label = h5("Select the date range for the missing data:"),
                   min = min_date,
                   max = max_date,
                   start = min_date,
                   end = max_date)
    
  })
  
  Dataset <- reactive({
    
    combined <- data.frame()
    
    if(input$filling_choice == "Interpolation"){
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
      combined <- combined[,c(1, 3, 2, 4)]  # rearrange columns to make well 1 before well 2
      
    } else if (input$filling_choice == "Linear-Regression"){
      
      Well_1_data <- Well_1_input()
      Well_2_data <- Well_2_input()
      
      Well_1_data <- Well_1_data %>%
        filter(date. >= input$dates[1] & date. <= input$dates[2])
      Well_2_data <- Well_2_data %>%
        filter(date. >= input$dates[1] & date. <= input$dates[2])
      
      combined <- left_join(Well_2_data, Well_1_data, by = "date.")
      
      combined <- plyr::rename(combined, c(wtdepth.x = "Well_2_level", wtdepth.y = "Well_1_level"))
      
      regression <- lm(Well_1_level ~ Well_2_level, combined)
      
      #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
      
      slope <- regression$coefficients[2]
      y_int <- regression$coefficients[1]
      
      combined <- combined %>%
        mutate(predicted_values = NA) %>%
        mutate(is_predicted = NA)
      
      num_well1 <- nrow(combined)
      
      for (i in 1:num_well1) {
        if (is.na(combined$Well_1_level[i]) == TRUE) {
          combined$predicted_values[i] = (slope * combined$Well_2_level[i] + y_int)
        } else if (combined$Well_1_level[i] == -99) {
          combined$predicted_values[i] = (slope * combined$Well_2_level[i] + y_int)
        }
        else {
          combined$predicted_values[i] = combined$Well_1_level[i]
        }
      }
      
      combined$predicted_values <- as.numeric(combined$predicted_values)
      
      for (i in 1:num_well1) {
        if (is.na(combined$Well_1_level[i]) == TRUE) {
          combined$is_predicted[i] = TRUE
        } else if (combined$Well_1_level[i] == -99) {
          combined$is_predicted[i] = TRUE
        }
        else {
          combined$is_predicted[i] = FALSE
        }
      }
      
      combined <- plyr::rename(combined, c(Well_2_level = "well_2", Well_1_level = "well_1"))
      combined <- combined %>% 
        select(date., well_1, well_2, is_predicted, predicted_values)
    }
    return(combined)
  })
  
  # Calculate and plot output
  output$Plot <- renderPlot({
    
    well_data <- Dataset()
    
    w1 <- as.character(Well_1_input()[1, 1])
    w1_hpu <- clusters %>% 
      filter(Well == w1)
    w1_hpu <- as.character(w1_hpu[1, 3])
    w2 <- as.character(Well_2_input()[1, 1])
    w2_hpu <- clusters %>% 
      filter(Well == w2)
    w2_hpu <- as.character(w2_hpu[1, 3])
    
    # different ggplots for interpolation and linear regression situations
    if(input$Well_2_Plot == FALSE & input$filling_choice == "Interpolation"){
      
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
             y = "Water Table Depth (cm)",
             caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth for Wells", w1, "and", w2)) +
        theme_bw()
    } else if (input$Well_2_Plot == FALSE & input$filling_choice == "Linear-Regression"){
        
        fit <- lm(well_2 ~ date., data = well_data)
        
        ggplot() +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = predicted_values, 
                                  color = is_predicted, group = 1)) +
          geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1]) +
          scale_y_reverse() +
          ylab("Water Table Depth (cm)") +
          xlab("Date") +
          labs(caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
               title = paste("Water Depth for Wells", w1, "and", w2)) +
          theme_bw()
      }else if (input$Well_2_Plot == TRUE & input$filling_choice == "Interpolation"){  # plotting well 1 and well 2
      
      ggplot() +
        geom_point(data = well_data,  # well 1
                   mapping = aes(x = date.,
                                 y = well_1,
                                 color = is_predicted)) +
        geom_line(data = well_data,   # well 2
                  mapping = aes(x = date.,
                                y = well_2),
                  color = "grey70") +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = well_1)) +
        scale_y_reverse() +
        labs(x = "Date",
             y = "Water Table Depth (cm)",
             caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth for Wells", w1, "and", w2)) +
          theme_bw()
    } else{
        
        fit <- lm(well_2 ~ date., data = well_data)
        
        ggplot() +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = predicted_values, 
                                  color = is_predicted, group = 1)) +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = well_2),
                    color = "grey70") +
          geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1]) +
          scale_y_reverse() +
          ylab("Water Table Depth (cm)") +
          xlab("Date") +
          labs(caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
               title = paste("Water Depth for Wells", w1, "and", w2)) +
          theme_bw()
    }
  }
  )
  
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
      w1 <- as.character(Well_1_input()[1, 1])
      w1_hpu <- clusters %>% 
        filter(Well == w1)
      w1_hpu <- as.character(w1_hpu[1, 3])
      w2 <- as.character(Well_2_input()[1, 1])
      w2_hpu <- clusters %>% 
        filter(Well == w2)
      w2_hpu <- as.character(w2_hpu[1, 3])
      
      if(input$Well_2_Plot == FALSE & input$filling_choice == "Interpolation"){
        
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
               y = "Water Table Depth (cm)",
               caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
               title = paste("Water Depth for Wells", w1, "and", w2)) +
          theme_bw()
      } else if (input$Well_2_Plot == FALSE & input$filling_choice == "Linear-Regression"){
        
        fit <- lm(well_2 ~ date., data = well_data)
        
        plot <- ggplot() +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = predicted_values, 
                                  color = is_predicted, group = 1)) +
          geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1]) +
          scale_y_reverse() +
          ylab("Water Table Depth (cm)") +
          xlab("Date") +
          labs(caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
               title = paste("Water Depth for Wells", w1, "and", w2)) +
          theme_bw()
      }else if (input$Well_2_Plot == TRUE & input$filling_choice == "Interpolation"){  # plotting well 1 and well 2
        
        plot <- ggplot() +
          geom_point(data = well_data,  # well 1
                     mapping = aes(x = date.,
                                   y = well_1,
                                   color = is_predicted)) +
          geom_line(data = well_data,   # well 2
                    mapping = aes(x = date.,
                                  y = well_2),
                    color = "grey70") +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = well_1)) +
          scale_y_reverse() +
          labs(x = "Date",
               y = "Water Table Depth (cm)",
               caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
               title = paste("Water Depth for Wells", w1, "and", w2)) +
          theme_bw()
      } else{
        
        fit <- lm(well_2 ~ date., data = well_data)
        
        plot <- ggplot() +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = predicted_values, 
                                  color = is_predicted, group = 1)) +
          geom_line(data = well_data,
                    mapping = aes(x = date.,
                                  y = well_2),
                    color = "grey70") +
          geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1]) +
          scale_y_reverse() +
          ylab("Water Table Depth (cm)") +
          xlab("Date") +
          labs(caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
               title = paste("Water Depth for Wells ", w1, "and ", w2)) +
          theme_bw()
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
