#' transformation module UI representation
#'
#' This function provides an input to select a transformation method.
#'
#' @param id The ID of the modules namespace.
#' @param label A character vector of length one with the label for the \code{\link[shiny]{selectInput}}.
#' @param selected The initially selected value. See \code{\link[shiny]{selectInput}}.
#' @param choices Named list of available transformations. Possible transformations are list(`None` = "raw", `log2` = "log2", `-log2` = "-log2", `log10` = "log10", `-log10` = "-log10", `Z score` = "zscore", `regularized log` = "rlog") which is also the default.
#' @param transposeOptions Boolean value if transpose radioButtons are shown (Default = FALSE).
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
transformationUI <- function(id, label = "Transformation", selected = "raw", choices = list(`None` = "raw", `log2` = "log2", `-log2` = "-log2", `log10` = "log10", `-log10` = "-log10", `Z score` = "zscore", `regularized log` = "rlog"), transposeOptions = FALSE) {
  ns <- shiny::NS(id)

  ret <- list(
    shiny::tags$b(label),
    #shiny::actionLink(ns("help"), label = NULL, icon = shiny::icon("question-circle")), # removed for now
    shiny::selectInput(ns("transform"),
                            label = NULL,
                            choices = choices,
                            selected = selected,
                            multiple = F))
  if(transposeOptions){
    ret <- list(ret, shinyjs::useShinyjs(), shiny::radioButtons(ns("transpose"), label = NULL, choices = c(`row-wise` = "row", `column-wise` = "column")))
  }

  shiny::tagList(ret)
}

#' transformation module server logic
#'
#' The module provides several transformations on a numeric data matrix for the user.
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param data Numeric matrix on which transformation is performed (column-wise). (Supports reactive)
#' @param transpose Whether the matrix should be transposed to enable row-wise transformation.
#' @param pseudocount Numeric Variable to add a pseudocount to log-based transformations.
#' @param replaceInf Change Infinite to NA, applied after transformation.
#' @param replaceNA Change NA to 0, applied after transformation.
#'
#' @return Namedlist of two containing data and name of the used method.
#'         data: Reactive containing the transformed matrix. Infinite values are replaced by NA and NA values are replaced by 0.
#'         method: Reactive containing String.
#'         transpose: Reactive containing String.
#'
#' @export
transformation <- function(input, output, session, data, transpose = FALSE, pseudocount = 1, replaceInf = TRUE, replaceNA = TRUE) {
  #handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)){
      data()
    }else{
      data
    }
  })

  #reset
  shinyjs::reset("transform")
  shinyjs::reset("transpose")

  #helptext
  # shiny::observeEvent(input$help, {
  #   title <- "Data transformation"
  #   content <- shiny::HTML("Choose a method with which the given data is transformed:<br/>")
  #
  #   none <- shiny::HTML("'None' = No transformation will be performed<br/>")
  #   log2 <- shiny::HTML(paste0("'log2' = A pseudocount of ", pseudocount, " will be added to all values afterwards a logarithm based two is performed.<br/>"))
  #   `-log2` <- shiny::HTML(paste0("'-log2' = Similar to log2 a pseudocount of ", pseudocount, " will be added to all values afterwards a <b>negated</b> logarithm based two is performed.<br/>"))
  #   log10 <- shiny::HTML(paste0("'log10' = Adds a pseudocount of ", pseudocount, " and performs a logarithm based ten.<br/>"))
  #   `-log10` <- shiny::HTML(paste0("'log10' = Similar to log10 adds a pseudocount of ", pseudocount, " and performs a <b>negated</b> logarithm based ten.<br/>"))
  #   zscore <- shiny::HTML(paste0("'zscore' = Applies a zscore transformation to the data.<br/>"))
  #   rlog <- shiny::HTML(paste0("'regularized log (rlog)' = Log2 transformation which minimizes differences between samples for rows with small counts, and which normalizes with respect to library size."))
  #
  #   content <- list(content, none, log2, `-log2`, log10, `-log10`, zscore, rlog, shiny::HTML("<hr>"))
  #   if(!is.null(input$transpose)){
  #     transposeOpt <- shiny::HTML("Use the radioButtons to select whether the transformation should be applied row- or column-wise. Will only be enabled when needed (e.g. zscore).<br/>")
  #     content <- list(content, transposeOpt)
  #   }
  #   if(replaceInf){
  #     inf <- shiny::HTML("Every positive or negative Infinite will be replaced with NA after transformation.<br/>")
  #     content <- list(content, inf)
  #   }
  #   if(replaceNA){
  #     na <- shiny::HTML("All NA in the dataset will be set to 0 after the transformation is applied.")
  #     content <- list(content, na)
  #   }
  #
  #   shiny::showModal(
  #     shiny::modalDialog(
  #       title = title,
  #       footer = shiny::modalButton("close"),
  #       easyClose = TRUE,
  #       content
  #     )
  #   )
  # })

  # try rlog transformation else do log2
  try_rlog <- function(x) {
    tryCatch(DESeq2::rlogTransformation(x, blind = TRUE),
             error = function(err) {
               message("Rlog failed using log2 instead.")
               log2(x)
             })
  }

  transformed_data <- shiny::reactive({
    data <- data.r()

    if(transpose | ifelse(!is.null(input$transpose), input$transpose == "row", FALSE) & input$transform == "zscore"){
      data <- t(data)
    }

    #transform data
    output <- switch(input$transform,
      log2 = log2(data + pseudocount),
      `-log2` = -log2(data + pseudocount),
      log10 = log10(data + pseudocount),
      `-log10` = -log10(data + pseudocount),
      zscore = scale(data, center = TRUE, scale = TRUE),
      rlog = try_rlog(round(data) + pseudocount),
      raw = data
    )

    #replace infinite with NA & NA with 0
    if(replaceInf){
      is.na(output) <- sapply(output, is.infinite)
    }
    if(replaceNA){
      output[is.na(output)] <- 0
    }

    if(transpose | ifelse(!is.null(input$transpose), input$transpose == "row", FALSE) & input$transform == "zscore"){
      output <- t(output)
    }

    return(output)
  })

  #enable transposeOptions only if relevant
  shiny::observe({
    if(input$transform == "zscore"){
      shinyjs::enable("transpose")
    }else{
      shinyjs::disable("transpose")
    }
  })

  method <- shiny::reactive({
    if(input$transform == "zscore") {
      paste(input$transform, input$transpose)
    } else {
      input$transform
    }
  })

  return(list(data = transformed_data, method = method, transpose = shiny::reactive(input$transpose)))
}
