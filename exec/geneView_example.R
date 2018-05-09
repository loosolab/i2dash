
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
source("../R/clarion.R")

# test data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), level = c("feature", rep("sample", 7), rep("condition", 4)), factor1 = c(rep("a", 5), rep("b", 7)), factor2 = c(rep("1", 7), rep("2", 5)), factor3 = c(rep("", 4), rep("test", 5), rep("test2", 3)))
names(metadata)[1] <- "key"
clarion <- Clarion$new(data = data, metadata = metadata)
####

ui <- dashboardPage(header = dashboardHeader(), sidebar = dashboardSidebar(
  numericInput(inputId = "width", label = "width in cm", value = 0, min = 0),
  numericInput(inputId = "height", label = "height in cm", value = 0, min = 0),
  sliderInput(inputId = "scale", label = "scale plot", value = 1, min = 1, max = 10)
), dashboardBody(fluidPage(
  geneViewUI("id")
)))

server <- function(input, output) {
  gene <- callModule(geneView, "id", clarion = clarion, plot.method = "static", width = reactive(input$width), height = reactive(input$height), scale = reactive(input$scale))

  observe({
    print(gene())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
