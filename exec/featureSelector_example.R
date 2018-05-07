library(shiny)
library(shinydashboard)
source("../R/and.R")
source("../R/orNumeric.R")
source("../R/orTextual.R")
source("../R/featureSelector.R")
source("../R/function.R")
source("../R/global.R")
source("../R/clarion.R")

# test data
data <- data.table::as.data.table(mtcars, keep.rowname = "id")
# create metadata
metadata <- data.table::data.table(names(data), level = c("feature", rep("sample", 7), rep("condition", 4)))
names(metadata)[1] <- "key"
clarion <- Clarion$new(data = data, metadata = metadata)

ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = dashboardSidebar(
                      verbatimTextOutput("filter")
), dashboardBody(fluidPage(
  featureSelectorUI(id = "id")
)))


server <- function(input, output) {

  mod <- callModule(featureSelector, "id", clarion = clarion)

  output$filter <- renderText({
    paste(mod()$filter, collapse = "\n")
  })

  observe({
    print(mod()$object$header)
    print(mod()$object$metadata)
    print(mod()$object$data)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
