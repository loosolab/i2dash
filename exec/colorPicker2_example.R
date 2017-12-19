library(shiny)
source("../R/colorPicker2.R")


ui <- fluidPage(
  column(width = 4,
         colorPicker2UI("custom.single", custom = TRUE, label = "Single color select"),
         verbatimTextOutput("cs.t")
         ),
  column(width = 4,
         colorPicker2UI("custom.multiple", custom = TRUE, multiple = TRUE, label = "Multiple color select/ custom colorpalette"),
         verbatimTextOutput("cm.t")
         ),
  column(width = 4,
         colorPicker2UI("defined", label = "predefined palettes", show.scaleoptions = T),
         verbatimTextOutput("d.t")
         )
  )


server <- function(input, output) {
  def <- callModule(colorPicker2, "defined", distribution = "all", num.color = 3)
  cs <- callModule(colorPicker2, "custom.single", num.colors = 3)
  cm <- callModule(colorPicker2, "custom.multiple", num.colors = 3)

  output$d.t <- renderPrint(def())
  output$cs.t <- renderPrint(cs())
  output$cm.t <- renderPrint(cm())

}

# Run the application
shinyApp(ui = ui, server = server)
