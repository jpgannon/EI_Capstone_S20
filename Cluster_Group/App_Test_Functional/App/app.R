library(DT)
library(shiny)
library(tidyverse)
library(lubridate)
library(shinycssloaders)

Master_List <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/dtw_result.csv") #<- this is our local path to that file

Well_Names <- as.list(unique(Master_List$Well_1)) #All Well Names

#All_Well_Data <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/allwelldata.csv")
A6 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/A6.csv")
I7 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/I7.csv")
I8 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/I8.csv")
I9 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/I9.csv")
JD02 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/JD02.csv")
JD23 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/JD23.csv")
JD24 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/JD24.csv")
JD25 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/JD25.csv")
K1 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K1.csv")
K1D <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K1D.csv")
K4D <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K4D.csv")
K4M <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K4M.csv")
K4S <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K4S.csv")
N1 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/N1.csv")
N3 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/N3.csv")
O1 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/O1.csv")
O2 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/O2.csv")
P2 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/P2.csv")
Q1 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/Q1.csv")
Q2 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/Q2.csv")
T1 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/T1.csv")
D1 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/D1.csv")
K6D <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K6D.csv")
K6S <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K6S.csv")
K7D <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K7D.csv")
K7S <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K7S.csv")
H4 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/H4.csv")
I3 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/I3.csv")
I6 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/I6.csv")
JD03 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/JD03.csv")
JD21 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/JD21.csv")
JD22 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/JD22.csv")
K10 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K10.csv")
K11 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K11.csv")
K8 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K8.csv")
K9 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/K9.csv")
N2 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/N2.csv")
N4 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/N4.csv")
N5 <- read_csv("C:/Users/Lauren/Documents/Capstone/Data/WS3_waterlevel_data/N5.csv")

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
                checkboxInput("OtherPlot", "Check to view Well 2 Data on the plot (in grey)", FALSE),
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
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("Plot") %>% withSpinner(color="#0dc5c1"),
                DT::dataTableOutput("mytable") %>% withSpinner(color="#0dc5c1")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    Well_1_Input <- reactive({
        switch(input$Well_1_Selection,
               "A6" = A6,
               "I7" = I7,
               "I8" = I8,
               "I9" = I9,
               "JD02" = JD02,
               "JD23" = JD23,
               "JD24" = JD24,
               "JD25" = JD25,
               "K1" = K1,
               "K1D" = K1D,
               "K4D" = K4D,
               "K4M" = K4M,
               "K4S" = K4S,
               "N1" = N1,
               "N3" = N3,
               "O1" = O1,
               "O2" = O2,
               "P2" = P2,
               "Q1" = Q1,
               "Q2" = Q2,
               "T1" = T1,
               "D1" = D1,
               "K6D" = K6D,
               "K6S" = K6S,
               "K7D" = K7D,
               "K7S" = K7S,
               "H4" = H4,
               "I3" = I3,
               "I6" = I6,
               "JD03" = JD03,
               "JD21" = JD21,
               "JD22" = JD22,
               "K10" = K10,
               "K11" = K11,
               "K8" = K8,
               "K9" = K9,
               "N2" = N2,
               "N4" = N4,
               "N5" = N5)
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
               "A6" = A6,
               "I7" = I7,
               "I8" = I8,
               "I9" = I9,
               "JD02" = JD02,
               "JD23" = JD23,
               "JD24" = JD24,
               "JD25" = JD25,
               "K1" = K1,
               "K1D" = K1D,
               "K4D" = K4D,
               "K4M" = K4M,
               "K4S" = K4S,
               "N1" = N1,
               "N3" = N3,
               "O1" = O1,
               "O2" = O2,
               "P2" = P2,
               "Q1" = Q1,
               "Q2" = Q2,
               "T1" = T1,
               "D1" = D1,
               "K6D" = K6D,
               "K6S" = K6S,
               "K7D" = K7D,
               "K7S" = K7S,
               "H4" = H4,
               "I3" = I3,
               "I6" = I6,
               "JD03" = JD03,
               "JD21" = JD21,
               "JD22" = JD22,
               "K10" = K10,
               "K11" = K11,
               "K8" = K8,
               "K9" = K9,
               "N2" = N2,
               "N4" = N4,
               "N5" = N5)
    })
    
    
    output$date_req <- renderUI({
        #Read in the data files for both Well 1 and the chosen Well 2
        
        Well_1_data <- Well_1_Input()
        Well_2_data <- Well_2_Input()
        
        # max_well_1_date <- max(Well_1_data$date.)
        # min_Well_1_date <- min(Well_1_data$date.)
        
        max_well_1_date <- "2012-06-01"
        min_Well_1_date <- "2012-01-01"
        
        
        h4("Date Selection")
        dateRangeInput("dates", 
                       label = h5("Select the date range for the missing data"),
                       max = max_well_1_date,
                       min = min_Well_1_date,
                       start = min_Well_1_date,
                       end = max_well_1_date)
        
    })
    
    
    
    output$Plot <- renderPlot({
        
        if(input$OtherPlot == FALSE) {
            
        Well_1_data <- Well_1_Input()
        Well_2_data <- Well_2_Input()
        
        Well_1_data <- Well_1_data %>%
            filter(date. >= input$dates[1] & date. <= input$dates[2])
        
        Well_2_data <- Well_2_data %>%
            filter(date. >= input$dates[1] & date. <= input$dates[2])
        
        Well_1_data <- left_join(Well_2_data, Well_1_data, by = "date.")
        
        Well_1_data <- plyr::rename(Well_1_data, c(level.x = "Well_2_level", level.y = "Well_1_level"))
        
        regression <- lm(Well_1_data$Well_1_level ~ Well_1_data$Well_2_level)
        
        #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
        
        slope <- regression$coefficients[2]
        y_int <- regression$coefficients[1]
        
        Well_1_data <- Well_1_data %>%
            mutate(predicted_values = NA) %>%
            mutate(is_predicted = NA)
        
        num_well1 <- nrow(Well_1_data)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
            } else if (Well_1_data$Well_1_level[i] == -99) {
                Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
            }
            else {
                Well_1_data$predicted_values[i] = Well_1_data$Well_1_level[i]
            }
        }
        
        Well_1_data$predicted_values <- as.numeric(Well_1_data$predicted_values)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                Well_1_data$is_predicted[i] = TRUE
            } else if (Well_1_data$Well_1_level[i] == -99) {
                Well_1_data$is_predicted[i] = TRUE
            }
            else {
                Well_1_data$is_predicted[i] = FALSE
            }
        }
        
        #head(Well_1_data)
        
        ggplot() +
            geom_line(aes(Well_1_data$date., Well_1_data$predicted_values, color = is_predicted, group = 1), data = Well_1_data) +
            scale_y_reverse() +
            ylab("Water Table Depth (cm)") +
            xlab("Date") +
            theme_classic()
        } else {
            
            library(ggplot2)
            
            Well_1_data <- Well_1_Input()
            Well_2_data <- Well_2_Input()
            
            Well_1_data <- Well_1_data %>%
                filter(date. >= input$dates[1] & date. <= input$dates[2])
            
            Well_2_data <- Well_2_data %>%
                filter(date. >= input$dates[1] & date. <= input$dates[2])
            
            Well_1_data <- left_join(Well_2_data, Well_1_data, by = "date.")
            
            Well_1_data <- plyr::rename(Well_1_data, c(level.x = "Well_2_level", level.y = "Well_1_level"))
            
            regression <- lm(Well_1_data$Well_1_level ~ Well_1_data$Well_2_level)
            
            #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
            
            slope <- regression$coefficients[2]
            y_int <- regression$coefficients[1]
            
            Well_1_data <- Well_1_data %>%
                mutate(predicted_values = NA) %>%
                mutate(is_predicted = NA)
            
            num_well1 <- nrow(Well_1_data)
            
            for (i in 1:num_well1) {
                if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                    Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
                } else if (Well_1_data$Well_1_level[i] == -99) {
                    Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
                }
                else {
                    Well_1_data$predicted_values[i] = Well_1_data$Well_1_level[i]
                }
            }
            
            Well_1_data$predicted_values <- as.numeric(Well_1_data$predicted_values)
            
            for (i in 1:num_well1) {
                if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                    Well_1_data$is_predicted[i] = TRUE
                } else if (Well_1_data$Well_1_level[i] == -99) {
                    Well_1_data$is_predicted[i] = TRUE
                }
                else {
                    Well_1_data$is_predicted[i] = FALSE
                }
            }
            
            for (i in 1:num_well1) {
                if (Well_1_data$Well_2_level[i] == -99) {
                    Well_1_data$Well_2_level[i] = NA
                }
                else {
                    Well_1_data$Well_2_level[i] = Well_1_data$Well_2_level[i]
                }
            }
            
            #head(Well_2_data)
            
            ggplot(data = Well_1_data, aes(x = Well_1_data$date.)) + 
                geom_line(aes(y = Well_1_data$predicted_values, color = is_predicted, group = 1)) + 
                geom_line(aes(y = Well_1_data$Well_2_level), color="grey70") +
                scale_y_reverse() +
                ylab("Water Table Depth (cm)") +
                xlab("Date") +
                theme_classic()
            
    }
    })
    
    
    
    output$mytable = DT::renderDataTable({
        Well_1_data <- Well_1_Input()
        Well_2_data <- Well_2_Input()
        
        Well_1_data <- Well_1_data %>%
            filter(date. >= input$dates[1] & date. <= input$dates[2])
        
        Well_2_data <- Well_2_data %>%
            filter(date. >= input$dates[1] & date. <= input$dates[2])
        
        Well_1_data <- left_join(Well_2_data, Well_1_data, by = "date.")
        
        Well_1_data <- plyr::rename(Well_1_data, c(level.x = "Well_2_level", level.y = "Well_1_level"))
        
        regression <- lm(Well_1_data$Well_1_level ~ Well_1_data$Well_2_level)
        
        #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
        
        slope <- regression$coefficients[2]
        y_int <- regression$coefficients[1]
        
        Well_1_data <- Well_1_data %>%
            mutate(predicted_values = NA) %>%
            mutate(is_predicted = NA)
        
        num_well1 <- nrow(Well_1_data)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
            } else if (Well_1_data$Well_1_level[i] == -99) {
                Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
            }
            else {
                Well_1_data$predicted_values[i] = Well_1_data$Well_1_level[i]
            }
        }
        
        Well_1_data$predicted_values <- as.numeric(Well_1_data$predicted_values)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                Well_1_data$is_predicted[i] = TRUE
            } else if (Well_1_data$Well_1_level[i] == -99) {
                Well_1_data$is_predicted[i] = TRUE
            }
            else {
                Well_1_data$is_predicted[i] = FALSE
            }
        }
        
        Well_1_data
    })  
    
    #download button code will go
    
    output$downloadplot <- downloadHandler(
        filename <- function() {
            paste('plot', 'png', sep = ".")
        },
        content <- function(file) {
            png(file)
            
            Well_1_data <- Well_1_Input()
            Well_2_data <- Well_2_Input()
            
            Well_1_data <- Well_1_data %>%
                filter(date. >= input$dates[1] & date. <= input$dates[2])
            
            Well_2_data <- Well_2_data %>%
                filter(date. >= input$dates[1] & date. <= input$dates[2])
            
            Well_1_data <- left_join(Well_2_data, Well_1_data, by = "date.")
            
            Well_1_data <- plyr::rename(Well_1_data, c(level.x = "Well_2_level", level.y = "Well_1_level"))
            
            regression <- lm(Well_1_data$Well_1_level ~ Well_1_data$Well_2_level)
            
            #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
            
            slope <- regression$coefficients[2]
            y_int <- regression$coefficients[1]
            
            Well_1_data <- Well_1_data %>%
                mutate(predicted_values = NA) %>%
                mutate(is_predicted = NA)
            
            num_well1 <- nrow(Well_1_data)
            
            for (i in 1:num_well1) {
                if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                    Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
                } else if (Well_1_data$Well_1_level[i] == -99) {
                    Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
                }
                else {
                    Well_1_data$predicted_values[i] = Well_1_data$Well_1_level[i]
                }
            }
            
            Well_1_data$predicted_values <- as.numeric(Well_1_data$predicted_values)
            
            for (i in 1:num_well1) {
                if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                    Well_1_data$is_predicted[i] = TRUE
                } else if (Well_1_data$Well_1_level[i] == -99) {
                    Well_1_data$is_predicted[i] = TRUE
                }
                else {
                    Well_1_data$is_predicted[i] = FALSE
                }
            }
            
            plot <- ggplot() +
                geom_line(aes(Well_1_data$date., Well_1_data$predicted_values, color = is_predicted, group = 1), data = Well_1_data) +
                scale_y_reverse() +
                ylab("Water Table Depth (cm)") +
                xlab("Date") +
                theme_classic() 
            
            print(plot)
            
            dev.off()
        },
        contentType = "image/png"
    )
    
    Dataset <- reactive({
        
        Well_1_data <- Well_1_Input()
        Well_2_data <- Well_2_Input()
        
        Well_1_data <- Well_1_data %>%
            filter(date. >= input$dates[1] & date. <= input$dates[2])
        
        Well_2_data <- Well_2_data %>%
            filter(date. >= input$dates[1] & date. <= input$dates[2])
        
        Well_1_data <- left_join(Well_2_data, Well_1_data, by = "date.")
        
        Well_1_data <- plyr::rename(Well_1_data, c(level.x = "Well_2_level", level.y = "Well_1_level"))
        
        regression <- lm(Well_1_data$Well_1_level ~ Well_1_data$Well_2_level)
        
        #the formula is y = regression$coefficients[2]x + regression$coefficients[1]
        
        slope <- regression$coefficients[2]
        y_int <- regression$coefficients[1]
        
        Well_1_data <- Well_1_data %>%
            mutate(predicted_values = NA) %>%
            mutate(is_predicted = NA)
        
        num_well1 <- nrow(Well_1_data)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
            } else if (Well_1_data$Well_1_level[i] == -99) {
                Well_1_data$predicted_values[i] = ((slope*Well_1_data$Well_2_level[i]) + y_int)
            }
            else {
                Well_1_data$predicted_values[i] = Well_1_data$Well_1_level[i]
            }
        }
        
        Well_1_data$predicted_values <- as.numeric(Well_1_data$predicted_values)
        
        for (i in 1:num_well1) {
            if (is.na(Well_1_data$Well_1_level[i]) == TRUE) {
                Well_1_data$is_predicted[i] = TRUE
            } else if (Well_1_data$Well_1_level[i] == -99) {
                Well_1_data$is_predicted[i] = TRUE
            }
            else {
                Well_1_data$is_predicted[i] = FALSE
            }
        }
        
        return(Well_1_data)
    })
    
    output$downloaddata <- downloadHandler(
        filename = function() { "Well_1_Predicted_data.csv" }, content = function(file) {
            write.csv(Dataset(), file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)


