
library(shiny)
library(shinydashboard)
source("../R/function.R")
source("../R/colorPicker.R")
source("../R/columnSelector.R")
source("../R/transformation.R")
source("../R/scatterPlot.R")
source("../R/marker.R")
source("../R/limit.R")
source("../R/label.R")
source("../R/global.R")
source("../R/clarion.R")

####Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), level = c("feature", rep("sample", 7), rep("condition", 4)))
names(metadata)[1] <- "key"
clarion <- Clarion$new(data = data, metadata = metadata)
####

ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = dashboardSidebar(
                      markerUI("marker"),
                      numericInput(inputId = "width", label = "width in cm", value = 0, min = 0),
                      numericInput(inputId = "height", label = "height in cm", value = 0, min = 0),
                      sliderInput(inputId = "scale", label = "scale plot", min = 1, max = 10, value = 1)
                    ),
                    body = dashboardBody(
                      fluidPage(
                        scatterPlotUI("id")
                      )
                    )
)

server <- function(input, output) {
  # highlight first 10 cars
  marked <- Clarion$new(metadata = metadata, data = data[1:10])
  marker <- callModule(marker, "marker", clarion = marked)

  plot <- callModule(scatterPlot, "id", clarion = clarion, marker.output = marker, plot.method = "interactive", width = reactive(input$width), height = reactive(input$height), scale = reactive(input$scale))

  observe({
    print(plot())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
