
library(shiny)
library(tidyverse)
library(lubridate)

Master_List <- read_csv("C:/Users/Lauren/Documents/Capstone/Test_Data/Master_List.csv") #<- this is our local path to that file

Well_Names <- as.list(unique(Master_List$Well_1)) #All Well Names



# Define UI for application that plots wells for a specific time period
ui <- fluidPage(
    
    # Application title
    titlePanel("Predictive Data Gap Filling"),
    
    # Show a plot of the water level data
        fluidRow(
            column(width = 10, class = "well",
                 plotOutput("wellplot")),
            
            column(3,
                   h4("Date Selection"),
                       dateRangeInput("dates", label = h5("Select the date range for the missing data"))),
            
            column(4, offset = 1,
                       h4("Well Selection"),
                       h5("Well 1 is the well containing missing data that will need to be predicted. Well 2 is the well selected or recommended to be used to predict and fill in Well 1's missing data."),
                       selectInput("Well_1_Selection", label = h5("Select Well 1."), 
                                   choices = Well_Names, 
                                   selected = 1),
                   
                       selectInput('Well_2_Selection', label = h5("Select Well 2."),
                                   choices = Well_2_Options,
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
    
    Well_1 <- input$Well_1_Selection #This is what the user selects for Well 1
    
    Well_1_read_file <- paste("C:/Users/Lauren/Documents/Capstone/Test_Data/", Well_1, "_Well.csv", sep = "")
    Well_1_Data <- read_csv(Well_1_read_file)
    
    Well_2_Options <- Master_List %>% #This block of text finds the Well_1 in the Master list and populates the drop down for Well 2 with corresponding Wells
        filter(Well_1 == input$Well_1_Selection) %>%
        select(Well_2)
    
    Well_2 <- input$Well_2_Selection
    Well_2_read_file <- paste("C:/Users/Lauren/Documents/Capstone/Test_Data/", Well_2, "_Well.csv", sep = "")
    Well_2_Data <- read_csv(Well_2_read_file)
    
    #for the sake of this test I'm just going to run this as a straight linear regression 
    #this will probably need to be changed when we do the actual analysis
    regression <- lm(Well_1_Data$level ~ Well_2_Data$level)
    
    #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
    
    slope <- regression$coefficients[2]
    y_int <- regression$coefficients[1]
    
    #Create a new column (predicted values) in Well_1 dataset
    
    Well_1_Data <- Well_1_Data %>%
        mutate(predicted_values = NA)
    
    #Loop over all the Well_1 original data. If there is an NA, then apply the formula.
    #Otherwise, keep the original values.
    num_well1 <- nrow(Well_1_Data)
    
    for (i in 1:num_well1) {
        if (is.na(Well_1_Data$level[i]) == TRUE) {
            Well_1_Data$predicted_values[i] = ((slope*Well_2_Data$level[i]) + y_int)
        } else {
            Well_1_Data$predicted_values[i] = Well_1_Data$level[i]
        }
    }
    
    Well_1_Data$predicted_values <- as.numeric(Well_1_Data$predicted_values)
    
    #Create an additional column "is_predicted" to clearly state whether each value 
    #in the predicted_values column is either from the original data or predicted and artificial
    
    Well_1_Data <- Well_1_Data %>%
        mutate(is_predicted = NA)
    
    for (i in 1:num_well1) {
        if (is.na(Well_1_Data$level[i]) == TRUE) {
            Well_1_Data$is_predicted[i] = TRUE
        } else {
            Well_1_Data$is_predicted[i] = FALSE
        }
    }

    
    output$wellplot <- renderPlot({
        
        wells <- filter(sample_subset, Well == ID) #, date >= start, date <= end)
        
        ggplot() +
            geom_line(aes(Well_1_Data$date, Well_1_Data$predicted_values, color = is_predicted, group = 1), data = Well_1_Data) +
            scale_y_reverse() +
            ylab("Water Table Depth (cm)") +
            xlab("Date") +
            theme_classic()
    })
}    

# Run the application 
shinyApp(ui = ui, server = server)