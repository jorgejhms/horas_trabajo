library(shiny)
library(shiny.semantic)

# Define UI for application that draws a histogram
ui <- semanticPage(
  title = "Test",
  h1("Prueba")
)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
