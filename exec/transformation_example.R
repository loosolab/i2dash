
library(shiny)
source("../R/transformation.R")

ui <- fluidPage(
  h3("Module UI"),
  transformationUI(id = "id", transposeOptions = TRUE),
  hr(),
  h3("Input parameter"),
  numericInput(inputId = "pseudocount", label = "Pseudocount", value = 1),
  checkboxInput(inputId = "transpose", label = "Transpose"),
  checkboxInput(inputId = "replaceInf", label = "replace Inf with NA", value = TRUE),
  checkboxInput(inputId = "replaceNA", label = "replace NA with 0", value = TRUE),
  hr(),
  h3("Module output"),
  verbatimTextOutput(outputId = "module_out")
)

server <- function(input, output) {
  data_matrix <- matrix(c(0:9, 0:9), ncol = 10)

  data <- reactive({
    data_matrix
  })

  mod <- callModule(transformation, "id", data, transpose = reactive(input$transpose), pseudocount = reactive(input$pseudocount), replaceInf = reactive(input$replaceInf), replaceNA = reactive(input$replaceNA))

  output$module_out <- renderPrint({
    print(mod$method())
    print(mod$data())
    print(mod$transpose())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
