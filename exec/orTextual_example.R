
library(shiny)
source("../R/orTextual.R")

ui <- fluidPage(
  h4('choices <- c("test,1,a,v,asd", "a,b,c", "a,b,c", "asd|lkj", "2")'),
  column(width = 4,
           orTextualUI(id = "contains"),
         verbatimTextOutput("co_out")
           ),
  column(width = 4,
          orTextualUI(id = "delimit"),
         verbatimTextOutput("del_out")
         ),
  column(width = 4,
         orTextualUI(id = "nodelimit"),
         verbatimTextOutput("nodel_out")
         ),
  actionButton("button", "reset")
)

server <- function(input, output) {
  choices <- c("test,1,a,v,asd", "a,b,c", "a,b,c", "asd|lkj", "2")

  contains <- callModule(orTextual, "contains", choices = choices, label = "contains = TRUE", contains = TRUE, reset = reactive(input$button))
  delimit <- callModule(orTextual, "delimit", choices = choices, label = "delimiter = ','", delimiter = ",", reset = reactive(input$button))
  no_delimit <- callModule(orTextual, "nodelimit", choices = choices, label = "delimiter = NULL, selected = 2, multiple = FALSE", selected = "2", multiple = FALSE, reset = reactive(input$button))

  output$co_out <- renderPrint(contains())
  output$del_out <- renderPrint(delimit())
  output$nodel_out <- renderPrint(no_delimit())
}

# Run the application
shinyApp(ui = ui, server = server)
