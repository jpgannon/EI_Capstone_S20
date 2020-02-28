library(shiny)
library(tidyverse)
library(lubridate)

Master_List <- read_csv("C:/Users/Lauren/Documents/Capstone/Test_Data/Master_List.csv") #<- this is our local path to that file

Well_Names <- as.list(unique(Master_List$Well_1)) #All Well Names


# Define UI for application that draws a histogram
ui <- fluidPage(

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
            actionButton("Initiate", label = "Go!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

server <- function(input, output, session) {
    
    output$Well_2_Req <- renderUI({
        Well_2_Options <- Master_List %>%
            filter(Well_1 == input$Well_1_Selection) %>%
            select(Well_2)
        
        selectInput('Well_2_Selection', label = h5("Select Well 2:"),
                    choices = Well_2_Options,
                    selected = 1)
    })

    output$date_req <- renderUI({
        #Read in the data files for both Well 1 and the chosen Well 2
        
        Well_1 <- input$Well_1_Selection
        Well_1_read_file <- paste("C:/Users/Lauren/Documents/Capstone/Test_Data/", Well_1, "_Well.csv", sep = "")
        Well_1_Data <- read_csv(Well_1_read_file)
        
        Well_2 <- input$Well_2_Selection
        Well_2_read_file <- paste("C:/Users/Lauren/Documents/Capstone/Test_Data/", Well_2, "_Well.csv", sep = "")
        Well_2_Data <- read_csv(Well_2_read_file)
        
        max_Well1_date <- max(Well_1_Data$date)
        min_Well1_date <- min(Well_1_Data$date)
        
        h4("Date Selection")
        dateRangeInput("dates", 
                       label = h5("Select the date range for the missing data"),
                       max = max_Well1_date,
                       min = min_Well1_date,
                       start = min_Well1_date,
                       end = max_Well1_date)
        
    })
    
    
    
     Well_1_Data <- eventReactive(input$Initiate, {
         regression <- lm(Well_1_Data$level ~ Well_2_Data$level)
         
         #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
         
         slope <- regression$coefficients[2]
         y_int <- regression$coefficients[1]
         
         
         num_well1 <- nrow(Well_1_Data)
         
         for (i in 1:num_well1) {
             if (is.na(Well_1_Data$level[i]) == TRUE) {
                 Well_1_Data$predicted_values[i] = ((slope*Well_2_Data$level[i]) + y_int)
             } else {
                 Well_1_Data$predicted_values[i] = Well_1_Data$level[i]
             }
         }
         
         Well_1_Data$predicted_values <- as.numeric(Well_1_Data$predicted_values)
         
         
         Well_1_Data <- Well_1_Data %>%
             mutate(is_predicted = NA)
         
         for (i in 1:num_well1) {
             if (is.na(Well_1_Data$level[i]) == TRUE) {
                 Well_1_Data$is_predicted[i] = TRUE
             } else {
                 Well_1_Data$is_predicted[i] = FALSE
             }
         }
         
         
     })

     output$Plot <- renderPlot({
         ggplot() +
             geom_line(aes(Well_1_Data$date, Well_1_Data$level), data = Well_1_Data) +
             scale_y_reverse() +
             ylab("Water Table Depth (cm)") +
             xlab("Date") +
             theme_classic()
     })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


