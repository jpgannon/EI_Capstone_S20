library(shiny)
library(tidyverse)
well_names <- read_csv("C:/Users/user/Desktop/Spring 2019/FREC4444/EI_Capstone_S20/Cluster_Group/Data/w3well_locations.txt")


# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("Watershed 3 Well Gap-Filling"),

    # Main panel for displaying outputs ----
    mainPanel(
      h4("Well 1 is the well with missing data that will need to be corrected. Well 2 will be used to predict or fill in missing data values from Well 1."),
      # # Output: Histogram ----
      # plotOutput(outputId = "distPlot"),    
      
    fluidRow(
      column(4, offset = 2,
             selectInput('Well 1', label = h3("Select Well 1:"), choices = well_names$Well)

      ),
      column(4,
             selectInput('Well 2', label = h3("Select Well 2:"), choices = well_names$Well),
             checkboxInput('reccomendation', 'Check here to view only reccomendations for Well 2')
      ),
      column(12, offset = 2,
             h3("Date Selection"),
             dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 2, end = Sys.Date() + 2
             ),
             h3("Plot of Well Data"),
             plotOutput("plot1", click = "plot_click"),
             actionButton(inputId = 'export', label = "Export Data")
             

      )
    )
  )
)

# Define server logic required to draw plot of well data
server <- function(input, output) {
  
# filler plot for now
  output$plot1 <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x))
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Minutes",
         main = "Values")
    
  })
  
}

shinyApp(ui = ui, server = server)




