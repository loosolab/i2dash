
library(shiny)
library(shinydashboard)
source("../R/function.R")
source("../R/colorPicker2.R")
source("../R/columnSelector.R")
source("../R/transformation.R")
source("../R/scatterPlot.R")
source("../R/marker.R")
source("../R/limit.R")

####Test Data
data <- data.table::data.table(id = rownames(mtcars), mtcars)
# create metadata
metadata <- data.table::data.table(names(data), level = c("annotation", rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"
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
  marker <- callModule(marker, "marker", data)
  # highlight all manual cars
  plot <- callModule(scatterPlot, "id", data = data, types = metadata[level != "annotation"], features = data[am == 1], markerReac = marker, plot.method = "interactive", width = reactive(input$width), height = reactive(input$height), scale = reactive(input$scale))

  observe({
    print(plot())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
