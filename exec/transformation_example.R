
library(shiny)
source("../R/transformation.R")

ui <- fluidPage(
  transformationUI(id = "id", transposeOptions = TRUE)
)

server <- function(input, output) {
  data_matrix <- matrix( 0:10, ncol = 10)

  data <- reactive({
    data_matrix
  })

  mod <- callModule(transformation, "id", data, transpose = F, pseudocount = 0, replaceInf = T, replaceNA = T)

  observe({
    print(mod$method())
    print(mod$data())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
