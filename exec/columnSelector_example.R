library(shiny)
source("../R/columnSelector.R")

###Test Data
table <- data.table::data.table(id = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), level = c("sample", "condition", "contrast"), sub_label = "sub")
names(table)[1] <- "key"

ui <- fluidPage(
  fluidRow(
    column(width = 6,
      columnSelectorUI(id = "id", label = F, title = "first selector"),
      verbatimTextOutput("first")
    ),
    column(width = 6,
      columnSelectorUI(id = "2", label = T, title = "second selector"),
      verbatimTextOutput("second")
    )
  )
)

server <- function(input, output) {
data <- reactive({
  table
})

type <- reactive({
  unique(table[[2]])[-1]
})

  mod <-callModule(columnSelector, "id", type.columns = table, multiple = FALSE, none = TRUE)
  mod2 <-callModule(columnSelector, "2", type.columns = data, type = type)

  output$first <- renderPrint({
    print(mod$type())
    print(mod$selectedColumn())
    print(mod$label())
  })

  output$second <- renderPrint({
    print(mod2$type())
    print(mod2$selectedColumn())
    print(mod2$label())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

