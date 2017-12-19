library(shiny)
source("../R/and.R")
source("../R/orNumeric.R")
source("../R/orTextual.R")
source("../R/function.R")

###Test Data
table <- data.table::data.table(w = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11),
                                x = c("a,b", "b,c", "c,d", "d,e", "e,f", "f,g", "g,h", "h,i", "j,k", "k,l"),
                                y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                z = c("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9", "j10")
                                )
delimiter <- c(NULL, ",", NULL, NULL)
multiple <- c(TRUE)
contains <- c(FALSE)
ranged <- c(TRUE, FALSE, FALSE)
step <- c(NULL)

ui <- fluidPage(
  fluidRow(
    selectInput(inputId = "column", label = "columns to select from", choices = names(table), multiple = T),
    actionButton(inputId = "reset", label = "Reset")
  ),
  fluidRow(
    andUI(id = "id")
  ),
  fluidRow(
    verbatimTextOutput("id.out")
  )
)

server <- function(input, output, session) {
  data <- reactive({
    table
  })

  mod <-callModule(and, "id", data = data, show.elements = reactive(input$column), delimiter = delimiter, multiple = multiple, contains = contains, ranged = ranged, step = step, selection.default = "all", reset = reactive(input$reset))

  output$id.out <- renderPrint({
    print(mod())
    print("Filter Data:")
    print(data())
    })
}

# Run the application
shinyApp(ui = ui, server = server)
