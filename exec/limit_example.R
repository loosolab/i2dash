
library(shiny)
library(shinyjs)
source("../R/limit.R")

####Test Data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), type = c("annotation", rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"
####

ui <- fluidPage(
  limitUI(id = "limiter", label = "Upper/ Lower Limit"),
  verbatimTextOutput("result")

)

server <- function(input, output) {

  limit <- callModule(limit, "limiter", lower = 5, upper = 10)

  output$result <- renderPrint({
    print(limit())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
