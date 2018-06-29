
library(shiny)
source("../R/orNumeric.R")
source("../R/function.R")

ui <- fluidPage(
  selectInput("test", "Select choices vector", choices = c(1, 2)),
  verbatimTextOutput("choices"),
  actionButton("reset", "reset"),
  tags$br(),
  column(width = 6,
         orNumericUI(id = "ranged"),
         verbatimTextOutput("ran_out")
         ),
  column(width = 6,
         orNumericUI(id = "single"),
         verbatimTextOutput("sin_out")
         )
)

server <- function(input, output) {
  choices1 <- c(0:10)
  choices2 <- c(11:20)

  choices <- reactive({
    if (input$test == 1) {
      choices1
    } else {
      choices2
    }
  })

  output$choices <- renderPrint(choices())

  value <- reactive({
    c(min(choices()), max(choices()))
  })

  single <- callModule(orNumeric, "single", choices = choices, value = 3, label = "Title single", step = 11, zoomable = FALSE, reset = reactive(input$reset))
  ranged <- callModule(orNumeric, "ranged", choices = choices, value = value, label = "Title ranged", stepsize = 1, reset = reactive(input$reset))

  output$ran_out <- renderPrint(ranged())
  output$sin_out <- renderPrint(single())
}

# Run the application
shinyApp(ui = ui, server = server)
