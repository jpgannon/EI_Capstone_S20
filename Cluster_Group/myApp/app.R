library(DT)
library(shiny)
library(tidyverse)
library(lubridate)

# import data
setwd("D:/Capstone/data")
wells <- read_csv("oneHourSummary.csv")
clusters <- read_csv("clusters_with_HPU.csv")
wellsList <- as.list(unique(wells$Well))
prediction_choices <- c("Interpolation", "Linear-Regression")

# User Interface
ui <- fluidPage(
  titlePanel("Predictive Well Data Gap Filling"),
  sidebarLayout(
    sidebarPanel(
      actionButton("show", "Show App User Guide"),
      selectInput("filling_choice", label = h5("Select a gap filling method:"),
                  choices = prediction_choices),
      h3("Well Selection"),
      selectInput("well1", label = h5("Select Well One:"),
                  choices = wellsList),
      uiOutput("Well_2_Req"),
      dateRangeInput("date", "Select a Date Range:",
                     start = "2007-08-10",
                     end = "2018-10-08",
                     format = "yyyy-mm-dd"),
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
      plotOutput("Plot",
                 dblclick = "dblclick",
                 brush = brushOpts(id = "date_brush")),
      DT::dataTableOutput("mytable")
    )
  ),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }")
)

# Back end
server <- function(input, output){
  
  # Pop up box if user clicks "Show App User Guide"
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "How To Use the App:",
      HTML("1. Select which gap filling method you would like to use <br> 
      Tip: Is the gap a smaller time period? <br>
             If yes, then it is safe to use either Interpolation or the Linear Regression method. <br>
             If no, then it is safer to use the Linear Regression method and find a Well 2 that has a high R2 Value (seen at the bottom right corner of the Linear Regression plot, above the top of the Dataset) <br>
             <br>
             2. Select Well 1 <br>
             What well has missing data that you are trying to fill with synthetic data? <br>
             <br>
             3. Select Well 2 <br>
             Tip: Are you using the Interpolation method? <br>
             If yes, the Well 2 that you select will not matter or factor into the gap filling process, so it does not matter which Well 2 is selected. <br>
             If no, it is important that you select a Well 2. You can determine which is the best Well 2 to select based on which has a higher R2 value (seen at the bottom right, below the plot and above the datatable) <br>
             <br>
             4. Select the Date range for the missing data <br>
             You can use this data range to narrow down to a single day that is missing data, or you can view a broader time period for the data, which might include multiple gaps, or no gaps at all. <br>
             <br>
             5. View the plot <br>
             If you selected Linear Regression, it might be beneficial to check the box to view Well 2 data (in grey) on the plot alongside the Well 1 data. This will help indicate to you the behavior of the well you are using to synthesize data (Well 2) <br>
             <br>
             6. Brushing to zoom in on plot <br>
             Click and drag a box over the time frame. Then, double click inside the box to replot the graph. <br>
             <br>
             7. View the datatable (below the plot) <br>
             This allows the user to view the raw data being plotted <br>
             <br>
             8. If desired, click the button to Download Plot <br>
             <br>
             9. If desired, click the button to Download Data"
      )
    ))
  })
  
  # Filter out Well 1 out data
  Well_1_input <- reactive({
    req(wells) %>% 
      filter(Well == input$well1) %>% 
      as.data.frame()
  })
  
  # Provide options for Well 2 based on Well 1 choice
  output$Well_2_Req <- renderUI({
    
    well_1_cluster <- req(clusters) %>% 
      filter(Well == req(input$well1)) %>% 
      select(Cluster) %>% 
      as.numeric()
    
    if(well_1_cluster == 5){
      well_2_options <- req(clusters) %>% 
        filter(Well != req(input$well1)) %>% 
        select(Well, Cluster) %>% 
        as.list()
    }else{
      well_2_options <- req(clusters) %>% 
        filter(Cluster == well_1_cluster,
               Well != req(input$well1)) %>%
        select(Well, Cluster) %>% 
        as.list()
    }
    
    selectInput("Well_2_Selection", label = h5("Select Well 2:"),
                choices = well_2_options)
    
  })
  
  # Filter out Well 2 data
  Well_2_input <- reactive({
    wells %>% 
      filter(Well == req(input$Well_2_Selection)) %>% 
      as.data.frame()
  })
  
  # Reactive value to store min and max dates
  date_range <- reactiveValues(x = as.POSIXct(c(start = "2008-08-10",
                                                end = "2018-10-08")))
  
  # Changes reactive date_range based on input from date UI
  observeEvent(input$date,{
    date_range$x <- as.POSIXct(c(input$date[1], input$date[2]))
  })
  
  # Performs calculations based on chosen gap filling method
  # Returns: data frame of combined Well 1 and Well 2 data
  Dataset <- reactive({
    
    combined <- data.frame()
    
    # Interpolation gap filling
    if(input$filling_choice == "Interpolation"){
      Well_1_data <- Well_1_input()
      Well_2_data <- Well_2_input()
      
      # filtering out data by date range
      Well_1_data <- Well_1_data %>% 
        filter(date. >= date_range$x[1] & date. <= date_range$x[2])
      Well_2_data <- Well_2_data %>% 
        filter(date. >= date_range$x[1] & date. <= date_range$x[2])
      
      # selecting relevant columns
      Well_1_data <- Well_1_data %>% 
        select(date., wtdepth)
      Well_2_data <- Well_2_data %>% 
        select(date., wtdepth)
      
      # joining Well 1 and Well 2 data
      combined <- left_join(Well_2_data, Well_1_data, by = "date.")
      colnames(combined) <- c("date.", "well_2", "well_1")
      
      # interpolation of well 1 NA values
      combined <- combined %>%  # creates new column to separate original and interpolated values 
        mutate(is_predicted = ifelse(is.na(well_1), TRUE, FALSE)) 
      combined$well_1 <- na.approx(combined$well_1, na.rm = FALSE)  # interpolate NA values
      combined <- combined[,c(1, 3, 2, 4)]   # rearrange columns to make well 1 before well 2
      
      # Linear regression gap filling    
    } else if (input$filling_choice == "Linear-Regression"){
      
      Well_1_data <- Well_1_input()
      Well_2_data <- Well_2_input()
      
      Well_1_data <- Well_1_data %>%
        filter(date. >= date_range$x[1] & date. <= date_range$x[2])
      Well_2_data <- Well_2_data %>%
        filter(date. >= date_range$x[1] & date. <= date_range$x[2])
      
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
  
  # Reactive function to create plots each time well selection changes
  # Returns: ggplot object for plotting or export
  create_plot <- reactive({
    
    # Getting data from reactive function
    well_data <- Dataset()
    result <- ggplot()  # Initializing empty ggplot
    
    # Getting start and end dates to put on plot axis
    start_date <- well_data %>% 
      arrange(date.)
    start_date <- start_date[1, 1]
    end_date <- well_data %>% 
      arrange(date.)
    end_date <- end_date[1, 1]
    
    # Getting well 1 and well 2 HPUs to put in plot caption
    w1 <- as.character(Well_1_input()[1, 1])
    w1_hpu <- clusters %>% 
      filter(Well == w1)
    w1_hpu <- as.character(w1_hpu[1, 3])
    w2 <- as.character(Well_2_input()[1, 1])
    w2_hpu <- clusters %>% 
      filter(Well == w2)
    w2_hpu <- as.character(w2_hpu[1, 3])
    
    # Plotting interpolation with just well 1 plot
    if(input$Well_2_Plot == FALSE & input$filling_choice == "Interpolation"){
      
      result <- ggplot() +
        geom_point(data = well_data,
                   mapping = aes(x = date.,
                                 y = well_1,
                                 color = is_predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Original", "Predicted")) +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = well_1)) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_y_reverse() +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth for Wells", w1, "and", w2),
             subtitle = paste(start_date, "to", end_date)) +
        theme_bw()
      
      # Plotting linear regression with just well 1 plot  
    } else if (input$Well_2_Plot == FALSE & input$filling_choice == "Linear-Regression"){
      
      fit <- lm(well_2 ~ date., data = well_data)
      
      result <- ggplot() +
        geom_point(data = well_data,
                   mapping = aes(x = date.,
                                 y = predicted_values, 
                                 color = is_predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Original", "Predicted")) +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = predicted_values)) +
        scale_y_reverse() +
        geom_hline(yintercept = 0, color = "brown") +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", 
                             w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth for Wells", w1, "and", w2),
             subtitle = paste(start_date, "to", end_date)) +
        theme_bw()
      
      # Plotting well 1 with interpolated data and original well 2 data
    }else if (input$Well_2_Plot == TRUE & input$filling_choice == "Interpolation"){  
      
      result <- ggplot() +
        geom_point(data = well_data,  # well 1
                   mapping = aes(x = date.,
                                 y = well_1,
                                 color = is_predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Original", "Predicted")) +
        geom_line(data = well_data,   # well 2
                  mapping = aes(x = date.,
                                y = well_2),
                  color = "grey70") +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = well_1)) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_y_reverse() +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth for Wells", w1, "and", w2),
             subtitle =paste(start_date, "to", end_date)) +
        theme_bw()
      
      # Plotting well 1 with data filled using linear regression and original well 2 data  
    } else{
      
      fit <- lm(well_2 ~ date., data = well_data)
      
      result <- ggplot() +
        geom_point(data = well_data,
                   mapping = aes(x = date.,
                                 y = predicted_values, 
                                 color = is_predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Original", "Predicted")) +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = predicted_values)) +
        geom_line(data = well_data,
                  mapping = aes(x = date.,
                                y = well_2),
                  color = "grey70") +
        geom_hline(yintercept = 0, color = "brown") +
        scale_y_reverse() +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth for Wells", w1, "and", w2),
             subtitle = paste(start_date, "to", end_date)) +
        theme_bw()
      
    }
    return(result)  # return ggplot object and graphing settings
  })
  
  # Observes for brushing to change date_range
  observeEvent(input$dblclick,{
    brush <- input$date_brush
    if (!is.null(brush)){
      date_range$x <- c(brush$xmin, brush$xmax)
    } else{
      date_range$x <- as.POSIXct(c(input$date[1], input$date[2]))
    }
    
  })
  
  # Plot output
  output$Plot <- renderPlot({
    create_plot()      
  }
  )
  
  # Table with water depth and predictions results
  output$mytable = DT::renderDataTable({
    
    well_data <- Dataset()  # get data table from reactive function
    
    well_data  # print data table
  })
  
  # Download plot as PNG
  output$downloadplot <- downloadHandler(
    filename = function(){
      paste(input$well1, ".png", sep = "")  # format for filename
    },
    content = function(file){
      ggsave(file, 
             plot = create_plot(),  # settings for size and dpi of exported plot
             device = "png",
             scale = 1,
             width = 20,
             height = 10,
             dpi = 300)
      dev.set(dev.next())
      dev.off()
    },
    contentType = "image/png"
  )
  
  # Download data table as a csv
  output$downloaddata <- downloadHandler(
    filename = function() {"Well_1_Predicted_data.csv"}, content = function(file){
      write.csv(Dataset(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)