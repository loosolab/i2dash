#' Renders a textbox with arbitrary content
#'
#' @param title The title of the textbox
#' @param content The content of the textbox
#'
#' @return A string containing markdown code for the rendered textbox
render_textbox <- function(title, content) {
  knitr::knit_expand(file = system.file("templates", "textbox.Rmd", package = "i2dash"), title = title, content = content)
}