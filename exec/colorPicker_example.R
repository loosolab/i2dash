library(shiny)
source("../R/colorPicker.R")


ui <- fluidPage(colorPickerUI("id"))

server <- function(input, output) {
  mod <- callModule(colorPicker, "id")

  observe(print(mod$scheme))

}

# Run the application
shinyApp(ui = ui, server = server)
