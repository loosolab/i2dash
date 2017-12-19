
library(shiny)
source("../R/orTextual.R")

ui <- fluidPage(
  h4('choices <- c("test,1,a,v,asd", "a,b,c", "a,b,c", "asd|lkj", "2")'),
  column(width = 4,
           orTextualUI(id = "contains"),
         verbatimTextOutput("co.out")
           ),
  column(width = 4,
          orTextualUI(id = "delimit"),
         verbatimTextOutput("del.out")
         ),
  column(width = 4,
         orTextualUI(id = "nodelimit"),
         verbatimTextOutput("nodel.out")
         ),
  actionButton("button", "reset")
)

server <- function(input, output) {
  choices <- c("test,1,a,v,asd", "a,b,c", "a,b,c", "asd|lkj", "2")

  contains <- callModule(orTextual, "contains", choices = choices, label = "contains = TRUE", contains = TRUE, reset = reactive(input$button))
  delimit <- callModule(orTextual, "delimit", choices = choices, label = "delimiter = ','", delimiter = ",", reset = reactive(input$button))
  no.delimit <- callModule(orTextual, "nodelimit", choices = choices, label = "delimiter = NULL, selected = 2, multiple = FALSE", selected = "2", multiple = FALSE, reset = reactive(input$button))

  output$co.out <- renderPrint(contains())
  output$del.out <- renderPrint(delimit())
  output$nodel.out <- renderPrint(no.delimit())
}

# Run the application
shinyApp(ui = ui, server = server)
