library(shiny)
source("../R/colorPicker.R")


ui <- fluidPage(
  column(width = 4,
         colorPickerUI("custom.single", custom = TRUE, label = "Single color select"),
         verbatimTextOutput("cs.t")
         ),
  column(width = 4,
         colorPickerUI("custom.multiple", custom = TRUE, multiple = TRUE, label = "Multiple color select/ custom colorpalette"),
         verbatimTextOutput("cm.t")
         ),
  column(width = 4,
         colorPickerUI("defined", label = "predefined palettes", show.scaleoptions = T),
         verbatimTextOutput("d.t")
         )
  )


server <- function(input, output) {
  def <- callModule(colorPicker, "defined", distribution = "all", num.color = 3)
  cs <- callModule(colorPicker, "custom.single", num.colors = 3)
  cm <- callModule(colorPicker, "custom.multiple", num.colors = 3)

  output$d.t <- renderPrint(def())
  output$cs.t <- renderPrint(cs())
  output$cm.t <- renderPrint(cm())

}

# Run the application
shinyApp(ui = ui, server = server)
