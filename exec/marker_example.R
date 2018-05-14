
library(shiny)
source("../R/marker.R")
source("../R/colorPicker2.R")
source("../R/label.R")
source("../R/clarion.R")

####Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), level = c("feature", rep("sample", 7), rep("condition", 4)))
names(metadata)[1] <- "key"
clarion <- Clarion$new(data = data, metadata = metadata)
####

ui <- fluidPage(
  markerUI(id = "mark"),
  verbatimTextOutput(outputId = "output")
)

server <- function(input, output) {

  marker <-callModule(marker, "mark", clarion = clarion)

  output$output <- renderPrint({
    marker()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
