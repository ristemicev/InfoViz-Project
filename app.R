# Load required libraries
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("InfoViz Project"),
  
  # Main content area
  mainPanel(
    h3("Welcome to the InfoViz Project"),
    p("MovieLens Data Visualization Tool"),
    br(),
    p("Coming soon...")
  )
)

# Define Server
server <- function(input, output, session) {
  # Server logic will go here
}

# Run the application
shinyApp(ui = ui, server = server)