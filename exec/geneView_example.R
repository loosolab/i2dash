
library(shiny)
library(shinydashboard)
source("../R/function.R")
source("../R/colorPicker2.R")
source("../R/transformation.R")
source("../R/geneView.R")
source("../R/columnSelector.R")
source("../R/label.R")
source("../R/limit.R")
source("../R/global.R")

####Test Data
data <- data.table::data.table(id = rownames(mtcars), names = rownames(mtcars), mtcars)
# create metadata
metadata <- data.table::data.table(names(data), factor1 = rep("", length.out = length(names(data))), level = c(rep("annotation", 2), rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"
####

ui <- dashboardPage(header = dashboardHeader(), sidebar = dashboardSidebar(
  numericInput(inputId = "width", label = "width in cm", value = 0, min = 0),
  numericInput(inputId = "height", label = "height in cm", value = 0, min = 0),
  sliderInput(inputId = "scale", label = "scale plot", value = 1, min = 1, max = 10)
), dashboardBody(fluidPage(
  geneViewUI("id")
)))

server <- function(input, output) {
  table.r <- reactive({
    data
  })
  metadata.r <- reactive({
    metadata
  })
  level.r <- reactive({
    metadata[level != "annotation"][["level"]]
  })

  gene <- callModule(geneView, "id", data = table.r, metadata.r, level.r, custom.label = table.r, plot.method = "static", width = reactive(input$width), height = reactive(input$height), scale = reactive(input$scale))

  observe({
    print(gene())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
