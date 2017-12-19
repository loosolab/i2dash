
library(shiny)
library(shinydashboard)
source("../R/function.R")
source("../R/colorPicker2.R")
source("../R/columnSelector.R")
source("../R/transformation.R")
source("../R/heatmap.R")
source("../R/label.R")

####Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), type = c("annotation", rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"
####


ui <- dashboardPage(header = dashboardHeader(), sidebar = dashboardSidebar(
  numericInput(inputId = "width", label = "width in cm", value = 0, min = 0),
  numericInput(inputId = "height", label = "height in cm", value = 0, min = 0)
), dashboardBody(fluidPage(
  heatmapUI("id")
)))

server <- function(input, output) {
  table <- reactive({
    data
  })
  typ <- reactive({
    # without annotation
    metadata[ type != "annotation"]
  })

  heat <- callModule(heatmap, "id", data = table, types = typ, plot.method = "static", custom.row.label = table, width = reactive(input$width), height = reactive(input$height))

  observe({
    print(heat())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
