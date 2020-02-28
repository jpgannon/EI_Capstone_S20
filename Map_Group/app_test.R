setwd("C:/Users/lizaw/Desktop/Capstone/")

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)


# Define UI for application that plots wells for a specific time period
ui <- fluidPage(
  
  #Application title
  titlePanel("Watershed 3 well visualization"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      dateRangeInput("date", "Select date range:", start = NULL, end = NULL,
                     separator = "to", startview = "year"),
      textInput("wells", "Enter well names with spaces in between",
                value = "A5")
      
      
      
    ),
    mainPanel( plotOutput("wellplot"), plotOutput("precplot"))
    
  )
)




#define server logic to draw line plot
server <- function(input, output) {
  
  #setwd("C:/Users/maone/OneDrive/Documents/SPRING2020/FREC4444/Map_Code/EI_Capstone_S20/Map_Group/")
  
  welldata <- read_csv("well_data.csv")
  precip <- read_csv("dailyWatershedPrecip1956-2019.csv")
  
  head(welldata)
  head(precip)
  
  precipWS3 <- precip %>%
    filter(precip$watershed == 3) %>% #Filter for watershed 3
    mutate(Precip = Precip * .1) #Convert mm to cm to match water level units
  
  
  #Filter for dates selected
  #precip_select <- filter(precipWS3, Precip == Precip, DATE >= start, DATE <= end)
  
  
  
  output$wellplot <- renderPlot({
    ID <- strsplit(input$wells, " ")[[1]]
    
    start <- input$date[1]
    
    end <- input$date[2]
    
    wells <- filter(welldata, Well == ID, date >= start, date <= end)
    
    ggplot(data = wells, mapping = aes(x = date, y = wtdepth, color = Well))+
      geom_line()+
      scale_y_reverse()+
      ylab("Water Table Depth (cm)")+
      xlab("Date") +
      theme_classic()
    
    
  })
  output$precplot <- renderPlot({
    ggplot(data = precip_select, mapping = aes(x = DATE, y = Precip))+
      geom_line()+
      ylab("Precipitation (cm)")+
      xlab("Date") +
      scale_y_reverse()+
      theme_classic()
  })
  
}


app <- shinyApp(ui, server)
runApp(app)



