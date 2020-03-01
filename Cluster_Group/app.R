library(shiny)
library(tidyverse)
well_names <- read_csv("C:/Users/user/Desktop/Spring 2019/FREC4444/EI_Capstone_S20/Cluster_Group/Data/w3well_locations.txt")
well_matches <- read_csv("C:/Users/user/Desktop/Spring 2019/FREC4444/Data/well_matches.csv")


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
             selectInput('Well1', label = h3("Select Well 1:"), choices = unique(well_matches$`Well 1`))

      ),
      column(4,
             selectInput('Well2', label = h3("Select Well 2:"), choices = NULL),
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
             downloadButton("downloadData", "Download")

      )
    )
  )
)

# Define server logic required to draw plot of well data
server <- function(input, output, session) {
  
  observeEvent(input$Well1, {
    updateSelectInput(session, 'Well2', 
                      choices = unique(well_matches$`Well 2`[well_matches$`Well 1`==input$Well1]))
  })
  
  
# filler plot for now
  output$plot1 <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x))
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Minutes",
         main = "Values")
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("well_names-", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(well_names, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)




