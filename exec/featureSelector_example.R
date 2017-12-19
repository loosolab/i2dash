library(shiny)
library(shinydashboard)
source("../R/and.R")
source("../R/orNumeric.R")
source("../R/orTextual.R")
source("../R/featureSelector.R")
source("../R/function.R")

# test data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), type = c("annotation", rep("performance", 7), rep("design", 4)))
names(metadata)[1] <- "key"

ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = dashboardSidebar(
                      selectInput("columns", label = "Features to select from", choices = names(data), multiple = TRUE),
                      verbatimTextOutput("filter")
), dashboardBody(fluidPage(
  featureSelectorUI(id = "id")
)))


server <- function(input, output) {

  mod <-callModule(featureSelector, "id", data = data, delimiter = ",", features = reactive(input$columns), feature.grouping = metadata)

  output$filter <- renderText({
    paste(mod()$filter, collapse = "\n")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
