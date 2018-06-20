
library(shiny)
library(shinydashboard)
source("../R/columnSelector.R")
source("../R/function.R")
source("../R/pca.R")
source("../R/global.R")
source("../R/clarion.R")
source("../R/label.R")
source("../R/colorPicker.R")
source("../R/limit.R")

####Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), level = c("feature", rep("sample", 7), rep("condition", 4)), factor1 = c(rep("group_a", 6), rep("group_b", 6)), factor2 = c(rep("group_1", 3), rep("group_2", 3), rep("group_3", 6)))
names(metadata)[1] <- "key"
clarion <- Clarion$new(data = data, metadata = metadata)
####

ui <- dashboardPage(header = dashboardHeader(), sidebar = dashboardSidebar(
  numericInput(inputId = "width", label = "width in cm", value = 0, min = 0),
  numericInput(inputId = "height", label = "height in cm", value = 0, min = 0),
  sliderInput(inputId = "scale", label = "scale plot", min = 1, max = 10, value = 1)
), dashboardBody(fluidPage(
  pcaUI("id")
)))

server <- function(input, output) {
  callModule(pca, "id", clarion = clarion, width = reactive(input$width), height = reactive(input$height), scale = reactive(input$scale))
}

# Run the application
shinyApp(ui = ui, server = server)
