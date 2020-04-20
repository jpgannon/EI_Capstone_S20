# Load required packages
library(DT)
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(png)
library(zoo)

# Import data
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
      actionButton("appGuide", "Show App User Guide"),
      actionButton("FAQ", "Show App FAQ"),
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
      checkboxInput("Well_2_Plot", "View Well 2 plot data in grey", FALSE),
      checkboxInput("data_table_viewer", "View Data Table", FALSE),
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
      tabsetPanel(
        tabPanel("Well Plot", plotOutput("PlotWells",
                                    dblclick = "dblclick",
                                    brush = brushOpts(id = "date_brush"),
                                    height = "600px")),
        tabPanel("Scatter Plot", plotOutput("PlotScatter")),
        tabPanel("Data Availability", imageOutput("PlotDataAvailability"))
      ),
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
  observeEvent(input$appGuide, {
    showModal(modalDialog(
      title = "How To Use the App:",
      HTML("<b> 1. Select which gap filling method you would like to use </b> <br> 
             Tip: Is the gap a smaller time period? (Less than 1 Week) <br>
             If yes, then it is safe to use either Interpolation or the Linear Regression method. <br>
             If no, then it is safer to use the Linear Regression method and find a Well 2 that has a high R2 Value (seen at the bottom right corner of the Linear Regression plot, above the top of the Dataset) <br>
             <br>
             <b> 2. Select Well 1 </b> <br>
             What well has missing data that you are trying to fill with synthetic data? <br>
             <br>
             <b> 3. Select Well 2 </b> <br>
             Tip: Are you using the Interpolation method? <br>
             If yes, the Well 2 that you select will not matter or factor into the gap filling process, so it does not matter which Well 2 is selected. <br>
             If no, it is important that you select a Well 2. You can determine which is the best Well 2 to select based on which has a higher R2 value (seen at the bottom right, below the plot and above the datatable) <br>
             <br>
             <b> 4. Select the Date range for the missing data </b> <br>
             You can use this data range to narrow down to a single day that is missing data, or you can view a broader time period for the data, which might include multiple gaps, or no gaps at all. <br>
             <br>
             <b> 5. View the plot </b> <br>
             If you selected Linear Regression, it might be beneficial to check the box to view Well 2 data (in grey) on the plot alongside the Well 1 data. This will help indicate to you the behavior of the well you are using to synthesize data (Well 2) <br>
             <br>
             <b> 6. Brushing to zoom in on plot </b> <br>
             Click and drag a box over the time frame. Then, double click inside the box to replot the graph. <br>
             <br>
             <b> 7. If desired, check the \"View Data Table \" box to display a table of the data below the plot. </b> <br>
             <br>
             <b> 8. Different Main Panel Tabs </b> <br>
             Well Plot: Line plot of well 1 and well 2 water depths over time. <br>
             Scatter Plot: Scatter plot of well 1 and well 2 water depth relationship. <br>
             Data Availability: Image of when each well has measured data. <br>
             <br>
             <b> 9. If desired, right-click on the image of the plot and select 'Save Image As' to download the plot </b> <br>
             <br>
             <b> 10. If desired, click the button to Download Data </b>"
      )
    ))
  })
  
  # Pop up box if user clicks "Show App FAQ"
  observeEvent(input$FAQ, {
    showModal(modalDialog(
      title = "Application FAQ & Background",
      HTML("<b> 1. Terms Definition </b> <br> 
              <b> Well 1: </b> A well containing missing data for a certain time period. <br>
              
              <b> Well 2: </b> Any well that the user selects in order to perform a linear regression. <br>
              This well is not needed or used during the interpolation gap filling process, and is dynamically limited 
              by the app so the user can only select a well that is from within the same cluster as Well 1.<br>
              
              <b> Interpolation: </b> A method of predictive gap filling that relies only on Well 1 data. 
              It uses two data points, the one directly preceding and the one directly following a gap in the 
              data, and populates the gap with what the algorithm expects the data to look like between the two 
              data points. <br>
              
              <b> Linear Regression: </b> A method of predictive gap filling that relies on two inputs: a Well 1 and a
              Well 2. This method then graphs all of the original Well 1 data that it has, and when it encounters a gap
              in the data, it performs a linear regression between Well 1 and Well 2 data, then uses that linear regression 
              formula to determine, based on the behavior of the Well 2 data, what the Well 1 data would look like in that gap. <br>
              The premise of this is as follows: when there is a gap, look to a well, within the same cluster, that doesn’t have a gap, 
              see what that second well is doing, and predict what Well 1 (which behaves similarly due to it falling in the same cluster) 
              is doing during that gap. <br>
              <br>
            <b> 2. Hubbard Brook Experimental Forest Background </b> <br>
             This Well data is sourced from Watershed 3, located within the Hubbard Brook Experimental Forest. <br>
             'The Hubbard Brook Ecosystem Study, founded in 1963 by G.E. Likens, F.H. Bormann, R.S. Pierce, and N.M. Johnson, is
             among the longest running and most comprehensive ecosystem studies in the world. Since the establishment of the 3,160-hectare
             (~7,800-acre) Hubbard Brook Experimental Forest in the White Mountains of New Hampshire by the USDA Forest Service in 1955, 
             researchers have used the site to study the hydrology, ecology, and management of northern forests. Hubbard Brook scientists 
             pioneered the small watershed approach to understanding forest ecosystems and advanced the use of whole-ecosystem manipulations 
             to quantify the response of forests to disturbance. Hubbard Brook research has figured strongly in the national debates on air 
             pollution, carbon emissions, and forest management.' (https://hubbardbrook.org/about) <br>
             <br>
             <b> For more information about the Hubbard Brook Experimental Forest, visit: </b> <br>
             https://hubbardbrook.org/ <br>
             <b> To find additional data from the Hubbard Brook Experimental Forest, visit their Data Catalog: </b> <br>
             https://hubbardbrook.org/d/hubbard-brook-data-catalog <br>
             <br>
            <b> 3. Which method should I use to predict the Synthetic Data?</b> <br>
             Our app is designed to use the results of a cluster anaylsis in order to determine which wells behave
             similarly enough to be used to predict missing data within each other's datasets. There are two methods
              that we use to make these predictions: <b> Interpolation </b> and <b>Linear Regression </b>. <br>
             <br>
             As stated in the Terms Definitions section, Interpolation only uses Well 1 to predict missing data within
              Well 1 data. It estimates the data in gaps based on the last known data point and the next available known
               data point. Because of this, it is recommended that this method is used for shorter gaps in data (less than one week). <br>
             Linear Regression relies on Well 2 to predict missing data in Well 1. Because of that, it can be used to fill both
              short (less than one week) and longer gaps (greater than one week). <br>
             <br>
             <b> 4. Which method is better to use to predict data? </b> <br>
             Both methods are viable, but to determine which method should be used first consider the how long the gap in the data 
             is. If it is longer than one week, interpolation will be less reliable, and it may be safer to use Linear Regression. 
             <br> In order to ensure the best fit, explore several different wells to determine which well has the highest R^2 Value. This
              can demonstrate which wells behave more similarly. <br> In order to achieve the highest R^2 Value, it is recommended to trim the
               selected date range down as small as possible, while still achieving your goal. <br>
             <br>
             <b> 5. Explain the Cluster Analysis </b> <br>
             In order to determine the optimal clustering method for clustering the Water Table well data, we tested two contrasting clustering methods, 
             and then compared the results. <br>
             The first clustering method was <b> Dynamic Time Warping (DTW) Clustering </b> <br>
             This method is a Hierarchical clustering method, and it calculates the distance from relative peaks to relative peaks and relative lows to 
             relative lows in time series. This allows for comparing time series with irregular time axis, such as the Water Table, however it is more 
             computationally intense than other methods <br>
             <br>
             The second method was <b> K-Shape Clustering </b> <br>
             This method is a Partitional Clustering Method and it is a derivative of K-Means clustering, optimized for time series clustering.<br>
             It iteratively calculates centroids based on Shape Based Distance calculation between time series, and clusters objects based on distance to centroids. 
             It also automatically z-normalizes data for calculation, but it z-normalizes based on entire data set, instead of individual objects. It requires
              a predefined cluster amount, and similar to DTW, it allows for clustering of time series with irregular time axis. In addition, it is less computationally intense. <br>
              <br>
              
              Using R, we implemented K-Shape and DTW algorithms over four time frames to cluster 40 selected wells that had sufficient data, based on the raw hourly summary and 
              the normalized hourly summary data. <br> 
              <br> We performed the analysis over two longer time frames and two short time frames that had high precipitation. We then generated results for both algorithms based 
              on all four time frames, for both data sets, with k = 3 to k = 10 to determine what number of clusters would be ideal, in addition to what algorithm, data set, and 
              time frame should be used to obtain optimal results.
              <br> 
              <br> To determine what combination of algorithm, data set, time frame, and cluster amount would produce the ideal clusters, Internal Cluster Validity Index (CVI) 
              Analysis was used. In particular, the Silhouette Index was used as a measure of how similar wells in a particular cluster were to each other, and how different 
              they were to wells from other clusters. <br>
              <br> To determine what clustering algorithm was optimal for creating clusters that best represented the different soil classes that the wells belong to, External 
              Cluster Validity Analysis was performed for each algorithm, time frame, and data set, but with the number of clusters kept to 6, because there were 6 different 
              soil HPU classes represented in the 40 wells chosen for the analysis. Rand Index was chosen as the CVI metric for success to determine which combination of variables
              best represented the soil classes. <br>
              <br>  <b> Results: </b> <br>
              Dynamic Time Warping generally outperformed K-Shape when based on the raw data set, and the results were mixed when based on the normalized data set. The highest 
              performing algorithm run resulted from Dynamic Time Warping with 4 clusters over 1-1-2012 to 6-1-2012 based on the raw data set. This test produced a Silhouette 
              Index of 0.62. <i> The results from this cluster analysis will be the clusters used in the gap filling application because it produced the most distinct clusters, 
              which will be ideal in generating synthetic data based on similar wells. </i> <br>
              <br> The most favorable Rand Index outcome resulted from the K-Shape algorithm performed over 8-9-12 to 8-11-12 based on the raw data set. This run generated a Rand Index value
              of 0.76. Notably, this algorithm was able to cluster the wells that belong to the “Typical” soil HPU class into the same generated cluster.  The most favorable 
              outcome based on the normalized data set also occurred under the same set of conditions, and resulted in a Rand Index of 0.73.  <br>
              <br> DTW generally underperformed at creating clusters that matched the properties of the soil HPU classes K-shape was better at creating clusters that matched the
              soil HPU classes when the algorithm was performed over shorter precipitation events, indicating that the algorithm was able to detect and classify based on the water 
              retention properties of the soil classes.<br>

            <br>
             <b> 6. Who made the app and why was it made? </b> <br>
             We're glad you asked! This app was made in conjunction with two other apps for Virginia Tech's Spring 2020 Senior Capstone project for students in
              the Environmental Informatics major. The professor overseeing the project was Dr. J.P. Gannon. For our Capstone project, our class was approached
               and requested to make three apps that allowed for better data processing, subsetting, and visualization of water table data from Watershed 3 in
                the Hubbard Brook Experimental Forest. <br>
                This app was made by: <br>
                <b> Tri Le, Robert Coulter, Lily Chen, and Lauren Boesch </b>"
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
    req(clusters, input$well1)
    
    well_1_cluster <- clusters %>% 
      filter(Well == input$well1) %>% 
      select(Cluster) %>% 
      as.numeric()
    
    if(well_1_cluster == 5){
      well_2_options <- clusters %>% 
        filter(Well != input$well1) %>% 
        select(Well, Cluster) %>% 
        as.list()
    }else{
      well_2_options <- clusters %>% 
        filter(Cluster == well_1_cluster,
               Well != input$well1) %>%
        select(Well, Cluster) %>% 
        as.list()
    }
    
    selectInput("Well_2_Selection", label = h5("Select Well 2:"),
                choices = well_2_options)
  })
  
  # Filter out Well 2 data
  Well_2_input <- reactive({
    req(input$Well_2_Selection)
    wells %>% 
      filter(Well == input$Well_2_Selection) %>% 
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
    
    # Initialize data frame object
    combined <- data.frame()
    
    # Setting inputs to wells to be used
    Well_1_data <- Well_1_input()
    Well_2_data <- Well_2_input()
    
    # Filtering wells to date range
    Well_1_data <- Well_1_data %>%
      filter(date. >= date_range$x[1] & date. <= date_range$x[2])
    Well_2_data <- Well_2_data %>%
      filter(date. >= date_range$x[1] & date. <= date_range$x[2])
    
    # Adding NAs to fill in blanks/gaps for well 1
    start_date <- Well_1_data %>% 
      arrange(date.) %>% 
      select(date.)
    start_date <- start_date[1, 1]
    end_date <- Well_1_data %>% 
      arrange(desc(date.)) %>% 
      select(date.)
    end_date <- end_date[1, 1]
    
    # Converting tibble to POSIXct class
    start_date <- as.vector(t(start_date))
    end_date <- as.vector(t(end_date))
    start_date <- as.POSIXct(start_date, origin = "1970-01-01:00:00:00")
    end_date <- as.POSIXct(end_date, origin = "1970-01-01:00:00:00")
    
    # Making full sequence of hourly dates from start to end of well 1 data
    all_times <- seq(start_date, end_date, by = "hour")
    all_times_frame <- data.frame(all_times)  # Converting seq vector to data frame class
    colnames(all_times_frame) <- "date."  # Renaming column to "date." to match well date column name
    
    # Joining data to have all dates from start to end of well 1 date
    Well_1_data <- left_join(all_times_frame, Well_1_data, by = "date.")
    
    
    # Interpolation gap filling
    if(input$filling_choice == "Interpolation"){
      
      # Selecting relevant columns
      Well_1_data <- Well_1_data %>% 
        select(date., wtdepth)
      Well_2_data <- Well_2_data %>% 
        select(date., wtdepth)
      
      # Joining Well 1 and Well 2 data
      combined <- right_join(Well_2_data, Well_1_data, by = "date.")
      colnames(combined) <- c("date.", "well_2", "well_1")
      
      # Interpolation of well 1 NA values
      combined <- combined %>%  # creates new column to separate original and interpolated values 
        mutate(is_predicted = ifelse(is.na(well_1), TRUE, FALSE)) 
      combined$well_1 <- na.approx(combined$well_1, na.rm = FALSE)  # interpolate NA values
      combined <- combined[,c(1, 3, 2, 4)]   # rearrange columns to make well 1 before well 2
      combined <- combined %>% 
        mutate(is_predicted = ifelse(is_predicted == TRUE, "Predicted", "Measured"))
      colnames(combined) <- c("Date_Time", "Well_1_Water_Depth", "Well_2_Water_Depth", "Measured_or_Predicted")
      
      # Linear regression gap filling    
    } else if (input$filling_choice == "Linear-Regression"){
      
      combined <- full_join(Well_2_data, Well_1_data, by = "date.")
      
      combined <- plyr::rename(combined, c(wtdepth.x = "Well_2_level", wtdepth.y = "Well_1_level"))
      
      # Linear model for relationship between well 1 and well 2 water levels
      regression <- lm(Well_1_level ~ Well_2_level, combined)
      
      # The formula is y = regression$coefficients[2]x + regression$coefficients[1]
      slope <- regression$coefficients[2]
      y_int <- regression$coefficients[1]
      
      combined <- combined %>%
        mutate(predicted_values = NA) %>%
        mutate(is_predicted = NA)
      
      num_well1 <- nrow(combined)
      
      # Replacing NAs and -99s with calculated value from formula
      for (i in 1:num_well1) {
        if (is.na(combined$Well_1_level[i]) == TRUE) {
          combined$predicted_values[i] = (slope * combined$Well_2_level[i] + y_int)
        } else if (combined$Well_1_level[i] == -99) {
          combined$predicted_values[i] = (slope * combined$Well_2_level[i] + y_int)
        }
        else {
          combined$predicted_values[i] = combined$Well_1_level[i] # Non-NAs stay the same
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
      
      # Renaming columns
      combined <- plyr::rename(combined, c(Well_2_level = "Well_2_Water_Depth", 
                                           date. = "Date_Time",
                                           is_predicted = "Measured_or_Predicted",
                                           predicted_values = "Well_1_Water_Depth"))
      
      # Selecting relevant columns
      combined <- combined %>%
        mutate(Measured_or_Predicted = ifelse(Measured_or_Predicted == FALSE, "Measured", "Predicted")) %>% 
        select(Date_Time, Well_1_Water_Depth, Well_2_Water_Depth, Measured_or_Predicted)
    }
    return(combined)  # Return data frame object
  })
  
  # Reactive function to create plots each time well selection changes
  # Returns: ggplot object for plotting or export
  create_plot <- reactive({
    
    # Getting data from reactive function
    well_data <- Dataset()
    result <- ggplot()  # Initializing empty ggplot
    
    # Getting start and end dates to put on plot axis
    start_date <- well_data %>% 
      arrange(Date_Time)
    start_date <- start_date[1, 1]
    end_date <- well_data %>% 
      arrange(desc(Date_Time))
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
                   mapping = aes(x = Date_Time,
                                 y = Well_1_Water_Depth,
                                 color = Measured_or_Predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Measured", "Predicted")) +
        geom_line(data = well_data,
                  mapping = aes(x = Date_Time,
                                y = Well_1_Water_Depth)) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_y_reverse() +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth of Wells", w1, "and", w2),
             subtitle = paste(start_date, "to", end_date)) +
        theme_bw(base_size = 20)
      
      # Plotting linear regression with just well 1 plot  
    } else if (input$Well_2_Plot == FALSE & input$filling_choice == "Linear-Regression"){
      
      fit <- lm(Well_1_Water_Depth ~ Well_2_Water_Depth, data = well_data)
      
      result <- ggplot() +
        geom_point(data = well_data,
                   mapping = aes(x = Date_Time,
                                 y = Well_1_Water_Depth, 
                                 color = Measured_or_Predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Measured", "Predicted")) +
        geom_line(data = well_data,
                  mapping = aes(x = Date_Time,
                                y = Well_1_Water_Depth)) +
        scale_y_reverse() +
        geom_hline(yintercept = 0, color = "brown") +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", 
                             w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth of Wells", w1, "and", w2),
             subtitle = paste(start_date, "to", end_date)) +
        theme_bw(base_size = 20)
      
      # Plotting well 1 with interpolated data and original well 2 data
    }else if (input$Well_2_Plot == TRUE & input$filling_choice == "Interpolation"){  
      
      result <- ggplot() +
        geom_point(data = well_data,  # well 1
                   mapping = aes(x = Date_Time,
                                 y = Well_1_Water_Depth,
                                 color = Measured_or_Predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Measured", "Predicted")) +
        geom_line(data = well_data,   # well 2
                  mapping = aes(x = Date_Time,
                                y = Well_2_Water_Depth),
                  color = "grey70") +
        geom_line(data = well_data,
                  mapping = aes(x = Date_Time,
                                y = Well_1_Water_Depth)) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_y_reverse() +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("Well 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth of Wells", w1, "and", w2),
             subtitle =paste(start_date, "to", end_date)) +
        theme_bw(base_size = 20)
      
      # Plotting well 1 with data filled using linear regression and original well 2 data  
    } else{
      
      fit <- lm(Well_1_Water_Depth ~ Well_2_Water_Depth, data = well_data)
      
      result <- ggplot() +
        geom_point(data = well_data,
                   mapping = aes(x = Date_Time,
                                 y = Well_1_Water_Depth, 
                                 color = Measured_or_Predicted)) +
        scale_color_discrete(name = "Values",
                             labels = c("Measured", "Predicted")) +
        geom_line(data = well_data,
                  mapping = aes(x = Date_Time,
                                y = Well_1_Water_Depth)) +
        geom_line(data = well_data,
                  mapping = aes(x = Date_Time,
                                y = Well_2_Water_Depth),
                  color = "grey70") +
        geom_hline(yintercept = 0, color = "brown") +
        scale_y_reverse() +
        labs(x = NULL,
             y = "Water Table Depth (cm)",
             caption = paste("R-squared = ", summary(fit)$r.squared,"\nWell 1 HPU: ", w1_hpu, "\nWell 2 HPU: ", w2_hpu),
             title = paste("Water Depth of Wells", w1, "and", w2),
             subtitle = paste(start_date, "to", end_date)) +
        theme_bw(base_size = 20)
      
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
  output$PlotWells <- renderPlot({
    create_plot()      
  }
  )
  
  # Plots data availability chart in sidebar when "Show Data Availability" is pressed
  output$PlotDataAvailability <- renderImage({
    list(src = "data_availability_chart.png")
  }, deleteFile = FALSE)
  
  # Plots a scatterplot of well 1 and well 2 water depth relationship when "Show Scatterplot" is pressed
  output$PlotScatter <- renderPlot({
    Well_1_data <- Well_1_input()
    Well_2_data <- Well_2_input()
    
    # Names of wells
    w1 <- Well_1_data[1, 1] %>% 
      as.character()
    w2 <- Well_2_data[1, 1] %>% 
      as.character()
    
    # Filtering out date range
    Well_1_data <- Well_1_data %>%
      filter(date. >= date_range$x[1] & date. <= date_range$x[2])
    Well_2_data <- Well_2_data %>%
      filter(date. >= date_range$x[1] & date. <= date_range$x[2])
    
    # Adding NAs to fill in blanks/gaps for well_1
    start_date <- Well_1_data %>% 
      arrange(date.) %>% 
      select(date.)
    start_date <- start_date[1, 1]
    end_date <- Well_1_data %>% 
      arrange(desc(date.)) %>% 
      select(date.)
    end_date <- end_date[1, 1]
    
    # Converting tibble to POSIXct class
    start_date <- as.vector(t(start_date))
    end_date <- as.vector(t(end_date))
    start_date <- as.POSIXct(start_date, origin = "1970-01-01:00:00:00")
    end_date <- as.POSIXct(end_date, origin = "1970-01-01:00:00:00")
    
    # Making full sequence of hourly dates from start to end of well 1 data
    all_times <- seq(start_date, end_date, by = "hour")
    all_times_frame <- data.frame(all_times)  # Converting seq vector to data frame class
    colnames(all_times_frame) <- "date."  # Renaming column to "date." to match well date column name
    
    # Joining data to have all dates from start to end of well 1 date
    Well_1_data <- left_join(all_times_frame, Well_1_data, by = "date.")
    combined <- full_join(Well_1_data, Well_2_data, by = "date.")
    combined <- combined %>% 
      select(date., wtdepth.x, wtdepth.y)
    colnames(combined) <- c("date.", "Well_1_Water_Depth", "Well_2_Water_Depth")
    
    # ggplot for scatterplot
    ggplot(data = combined,
           mapping = aes(x = Well_1_Water_Depth,
                         y = Well_2_Water_Depth)) +
      geom_point(size = 0.5) +
      labs(x = paste(w1, "Water Depth (cm)"),
           y = paste(w2, "Water Depth (cm)"),
           title = paste("Water Depth of Wells", w1, "and", w2),
           subtitle = paste(start_date, "to", end_date)) +
      theme_bw()
  })
  
  # Table with water depth and predictions results
  output$mytable = DT::renderDataTable({
    req(input$data_table_viewer == TRUE)
    well_data <- Dataset()  # get data table from reactive function
    
    well_data  # print data table
  })
  
  # Download data table as a csv
  output$downloaddata <- downloadHandler(
    filename = function() {"Well_1_Predicted_data.csv"}, content = function(file){
      write.csv(Dataset(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)