library(shiny)
library(tidyverse)
library(lubridate)

Master_List <- read_csv("C:/Users/Lauren/Documents/Capstone/Test_Data/Master_List.csv") #<- this is our local path to that file

Well_Names <- as.list(unique(Master_List$Well_1)) #All Well Names

source("C:/Users/Lauren/Documents/Capstone/R_Files/global.R")

A1 <- read_csv("C:/Users/Lauren/Documents/Capstone/Test_Data/A1_Well.csv")
B3 <- read_csv("C:/Users/Lauren/Documents/Capstone/Test_Data/B3_Well.csv")
B4 <- read_csv("C:/Users/Lauren/Documents/Capstone/Test_Data/B4_Well.csv")
C9 <- read_csv("C:/Users/Lauren/Documents/Capstone/Test_Data/C9_Well.csv")

# Define UI for application

ui <- fluidPage(

    fluidPage(
        
        # Application title
        titlePanel("Predictive Data Gap Filling"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                h3("Well Selection"),
                h5("Well 1 is the well containing missing data that will need to be predicted. Well 2 is the well selected or recommended to be used to predict and fill in Well 1's missing data."),
                selectInput("Well_1_Selection", label = h5("Select Well 1:"), 
                            choices = Well_Names, 
                            selected = 1),
                uiOutput("Well_2_Req"),
                uiOutput("date_req"),
                actionButton("Initiate", "Go!",
                             style = "background-color:#0077FF;
                      color:#FFFFFF;
                      border-color:#D1D1D1;
                      border-style:none;
                      border-width:1px;
                      border-radius:5%;
                      font-size:18px;")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("Plot")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    Well_1_Input <- reactive({
        switch(input$Well_1_Selection,
               "A1" = A1,
               "B3" = B3,
               "B4" = B4,
               "C9" = C9)
    })
    
    output$Well_2_Req <- renderUI({
        
        Well_2_Options <- Master_List %>%
            filter(Well_1 == input$Well_1_Selection) %>%
            select(Well_2)
        
        selectInput('Well_2_Selection', label = h5("Select Well 2:"),
                    choices = Well_2_Options,
                    selected = 1)
    })
    
    Well_2_Input <- reactive({
        switch(input$Well_2_Selection,
               "A1" = A1,
               "B3" = B3,
               "B4" = B4,
               "C9" = C9)
    })
    
    
    output$date_req <- renderUI({
        #Read in the data files for both Well 1 and the chosen Well 2
        
        Well_1_data <- Well_1_Input()
        Well_2_data <- Well_2_Input()
        
        max_well_1_date <- max(Well_1_data$date)
        min_Well_1_date <- min(Well_1_data$date)
        
        h4("Date Selection")
        dateRangeInput("dates", 
                       label = h5("Select the date range for the missing data"),
                       max = max_well_1_date,
                       min = min_Well_1_date,
                       start = min_Well_1_date,
                       end = max_well_1_date)
        
    })
    
    getDateRange <- reactive({
        input$go
        isolate({
            dateRange <- list(input$dates[1], input$dates[2])
        })
        return(dateRange)
    })
    
    observe({
        req(input$dates)
        dateRange <- getDateRange()
        print(dateRange)
        start_date <- dateRange[1]
        end_date <- dateRange[2]
    })   
    
    output$Plot <- renderPlot({
        
        # start_date <- dateRange[1]
        # end_date <- dateRange[2]
        
        Well_1_data <- Well_1_Input()
        Well_2_data <- Well_2_Input()
        
        Well_1_data <- Well_1_data %>%
            filter(date >= input$dates[1] & date <= input$dates[2])
         
         Well_2_data <- Well_2_data %>%
             filter(date >= input$dates[1] & date <= input$dates[2])
        
        regression <- lm(Well_1_data$level ~ Well_2_data$level)
        
        #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
        
        slope <- regression$coefficients[2]
        y_int <- regression$coefficients[1]
        
        
        num_well1 <- nrow(Well_1_data)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$level[i]) == TRUE) {
                Well_1_data$predicted_values[i] = ((slope*Well_2_data$level[i]) + y_int)
            } else {
                Well_1_data$predicted_values[i] = Well_1_data$level[i]
            }
        }
        
        Well_1_data$predicted_values <- as.numeric(Well_1_data$predicted_values)
        
        
        Well_1_data <- Well_1_data %>%
            mutate(is_predicted = NA)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$level[i]) == TRUE) {
                Well_1_data$is_predicted[i] = TRUE
            } else {
                Well_1_data$is_predicted[i] = FALSE
            }
        }
        
        ggplot() +
            geom_line(aes(Well_1_data$date, Well_1_data$predicted_values, color = is_predicted, group = 1), data = Well_1_data) +
            scale_y_reverse() +
            ylab("Water Table Depth (cm)") +
            xlab("Date") +
            theme_classic()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


