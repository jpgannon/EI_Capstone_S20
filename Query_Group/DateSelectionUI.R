#---
#title: "Date Selection"
#author: "Ian Bogucki"
#date: "2/14/20"
#ouput: "shiny app"
#---

library(shiny)

# UI for choosing a date or date range

ui <- fluidPage(
titlePanel("Choose Dates"),
sidebarLayout(
  sidebarPanel(
    
    #Select whether user wants single date or range to choose from
    selectInput('range', "Single Date or Range", 
    choices = c("Single Date", "Date Range"),
    selected = "Single Date"),

    #if user chose single date, show a single date input box
    conditionalPanel(
    condition = "input.range == 'Single Date'",
    dateInput("date", "Date:", value = Sys.Date(), 
    format = "mm/dd/yy")),
    
    #if user chose date range, show a date range input box
    conditionalPanel(
    condition = "input.range == 'Date Range'",
    dateRangeInput("dRange", "Date Range:", 
    start = Sys.Date() - 2, end = Sys.Date() + 2, 
    format = "mm/dd/yy")
    
)),
mainPanel())
)

server <- function(input, output) {
  
}


shinyApp(ui = ui, server = server)






        
    
