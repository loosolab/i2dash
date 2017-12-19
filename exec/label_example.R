
library(shiny)
source("../R/label.R")

####Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), type = c("annotation", rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"
####

ui <- fluidPage(
  labelUI("labeller"),
  "Vector of resulting labels:",
  verbatimTextOutput("result")
)

server <- function(input, output) {

  label <- callModule(label, "labeller", data = data[1:3], unique = T, multiple = T)

  output$result <- renderPrint({
    print(label())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
