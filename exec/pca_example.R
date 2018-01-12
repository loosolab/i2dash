
library(shiny)
library(shinydashboard)
source("../R/columnSelector.R")
source("../R/function.R")
source("../R/pca.R")

#### Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), level = c("annotation", rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"
####

ui <- dashboardPage(header = dashboardHeader(), sidebar = dashboardSidebar(
  numericInput(inputId = "width", label = "width in cm", value = 0, min = 0),
  numericInput(inputId = "height", label = "height in cm", value = 0, min = 0),
  sliderInput(inputId = "scale", label = "scale plot", min = 1, max = 10, value = 1)
), dashboardBody(fluidPage(
  pcaUI("id")
)))

server <- function(input, output) {
  callModule(pca, "id", data = data, types = metadata, levels = metadata[level != "annotation"][["level"]], width = reactive(input$width), height = reactive(input$height), scale = reactive(input$scale))
}

# Run the application
shinyApp(ui = ui, server = server)
