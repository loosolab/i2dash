
library(shiny)
source("../R/marker.R")
source("../R/colorPicker2.R")
source("../R/label.R")

#### Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), type = c("annotation", rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"
####

ui <- fluidPage(
  markerUI(id = "mark"),
  verbatimTextOutput(outputId = "output")
)

server <- function(input, output) {

  marker <-callModule(marker, "mark", highlight.labels = data)

  output$output <- renderPrint({
    marker()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
