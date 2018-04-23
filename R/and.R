#' AND module UI representation
#'
#' The AND module connects filtering and selection across multiple columns of a data.frame. Columns of class boolean, character or factor will be represented as textual ORs, numeric columns as numerical OR.
#'
#' @param id The ID of the modules namespace.
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
andUI <- function(id) {
  ns <- shiny::NS(id)

  ret <- shiny::uiOutput(ns("and"))

  shiny::tagList(ret)
}

#' AND module server logic
#'
#' This function evaluates output from multiple OR modules by combining with a logical and.
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param data The input data.frame for which selection should be provided. Evaluates an OR module for each column (Supports reactive).
#' @param show.elements A Vector of column names determining which OR modules are shown. Defaults to names(data). (Supports reactive)
#' @param element.grouping Group features in boxes. (Data.table: first column = columnnames, second column = groupnames) (Supports reactive)
#' @param column.labels Additional labels for the columns, defaults to \code{names(data)}.
#' @param delimiter A single character, or a vector indicating how column values are delimited. (Fills vector sequentially if needed)(Supports reactive)
#' @param multiple Whether or not textual ORs should allow multiple selections. (Fills vector sequentially if needed)(Supports reactive)
#' @param contains Whether or not textual ORs are initialized as textInput checking entries for given string. (Fills vector sequentially if needed)(Supports reactive)
#' @param ranged Whether or not numeric ORs are ranged. (Fills vector sequentially if needed)(Supports reactive)
#' @param step Set numeric ORs slider steps. (Fills vector sequentially if needed)(Supports reactive)
#' @param reset Reactive which will cause a UI reset on change.
#'
#' @return A reactive containing named list with a boolean vector of length \code{nrow(data)} (bool), indicating whether an observation is selected or not and a vector of Strings showing the used filter (text).
#'
#' @export
and <- function(input, output, session, data, show.elements = NULL, element.grouping = NULL, column.labels = NULL, delimiter = NULL, multiple = TRUE, contains = FALSE, ranged = FALSE, step = 100, reset = NULL) {
  # handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)){
      data()
    }else{
      data
    }
  })

  # handle reactive show.elements
  show.elements.r <- shiny::reactive({
    if(shiny::is.reactive(show.elements)) {
      show.elements <- show.elements()
    } else {
      show.elements <- show.elements
    }
    if(is.null(show.elements)) {
      show.elements <- names(data.r())
    }

    return(show.elements)
  })

  # handle reactive grouping
  element.grouping.r <- shiny::reactive({
    if(shiny::is.reactive(element.grouping)){
      element.grouping()
    }else{
      element.grouping
    }
  })

  parameter <- shiny::reactive({
    # get column labels
    if(is.null(column.labels)){
      column.labels <- names(data.r())
    }else{
      column.labels <- column.labels
    }
    # fill multiple if vector is too small
    if(shiny::is.reactive(multiple)) {
      multiple <- multiple()
    }
    if (length(multiple) < ncol(data.r())) {
      multiple <- rep(multiple, length.out = ncol(data.r()))
    }
    # fill contains if vector is too small
    if(shiny::is.reactive(contains)) {
      contains <- contains()
    }
    if (length(contains) < ncol(data.r())) {
      contains <- rep(contains, length.out = ncol(data.r()))
    }
    # fill ranged if vector is too small
    if(shiny::is.reactive(ranged)) {
      ranged <- ranged()
    }
    if (length(ranged) < ncol(data.r())) {
      ranged <- rep(ranged, length.out = ncol(data.r()))
    }
    # fill delimiter if vector is too small
    if(shiny::is.reactive(delimiter)) {
      delimiter <- delimiter()
    }
    if (length(delimiter) < ncol(data.r()) & !is.null(delimiter)) {
      delimiter <- rep(delimiter, length.out = ncol(data.r()))
    }
    # fill step if vector is too small
    if(shiny::is.reactive(step)) {
      step <- step()
    }
    if (length(step) < ncol(data.r()) & !is.null(step)) {
      step <- rep(step, length.out = ncol(data.r()))
    }

    return(list(column.labels = column.labels, multiple = multiple, contains = contains, ranged = ranged, delimiter = delimiter, step = step))
  })

  output$and <- shiny::renderUI({
    # new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Render orModules:")

    # select data based on show.elements
    data <- data.r()[, show.elements.r(), with = FALSE]

    step <- ncol(data)

    if(!is.null(element.grouping.r())){
      # only group shown data
      element.grouping <- element.grouping.r()[element.grouping.r()[[1]] %in% show.elements.r()]

      grouping <- tapply(element.grouping[[1]], element.grouping[[2]], function(x){x})
      # keep grouping order
      grouping <- grouping[unique(element.grouping[[2]])]

      return <- lapply(1:length(grouping), function(i){
        group <- lapply(unlist(grouping[i]), function(x){
          progress$inc(step, detail = x)

          if(is.numeric(data[[x]])){
            ui <- orNumericUI(id = session$ns(openssl::sha1(x)))
          }else{
            ui <- orTextualUI(id = session$ns(openssl::sha1(x)))
          }

          if(length(ui) < 4){ # orTextual
            shiny::tagList(shiny::fluidRow(
              shiny::column(width = 4, ui[1]),
              shiny::column(width = 3, ui[2]),
              shiny::column(width = 1, offset = 4, ui[3])
            ))
          }else{ # orNumeric
            shiny::tagList(shiny::fluidRow(
              shiny::column(width = 4, ui[1]),
              shiny::column(width = 1, ui[2]),
              shiny::column(width = 6, ui[3]),
              shiny::column(width = 1, ui[4])
            ))
          }

        })

        shiny::tagList(shinydashboard::box(width = 12, collapsible = TRUE, collapsed = TRUE, title = names(grouping[i]), shiny::tagList(group)))
      })
    }else{
      return <- lapply(1:ncol(data), function(x) {
        progress$inc(step, detail = names(data)[x])
        if (is.numeric(data[[x]])) {
          ui <- orNumericUI(id = session$ns(openssl::sha1(names(data)[x])))

        } else{
          ui <- orTextualUI(id = session$ns(openssl::sha1(names(data)[x])))
        }

        if(length(ui) < 4){ # orTextual
          shiny::tagList(shiny::fluidRow(
            shiny::column(width = 4, ui[1]),
            shiny::column(width = 3, ui[2]),
            shiny::column(width = 1, offset = 4, ui[3])
          ))
        }else{ # orNumeric
          shiny::tagList(shiny::fluidRow(
            shiny::column(width = 4, ui[1]),
            shiny::column(width = 1, ui[2]),
            shiny::column(width = 6, ui[3]),
            shiny::column(width = 1, ui[4])
          ))
        }
      })
    }

    # initialize new modules
    modules()

    shiny::tagList(return)
  })

  # initialize or modules
  # returns a vector containing modules
  modules <- shiny::reactive({
    # new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Filtering Module:")
    step <- ncol(data.r())

    lapply(1:ncol(data.r()), function(x) {
      progress$inc(step, detail = names(data.r())[x])
      if (is.numeric(data.r()[[x]])) {
        if (parameter()$ranged[x]) {
          shiny::callModule(
            module = orNumeric,
            id = openssl::sha1(names(data.r())[x]),
            choices = data.r()[[x]],
            value = c(floor(min(data.r()[[x]], na.rm = TRUE)), ceiling(max(data.r()[[x]], na.rm = TRUE))),
            label = parameter()$column.labels[x],
            step = parameter()$step[x],
            min. = floor(min(data.r()[[x]], na.rm = TRUE)),
            max. = ceiling(max(data.r()[[x]], na.rm = TRUE)),
            reset = reset
          )
        } else{
          shiny::callModule(
            module = orNumeric,
            id = openssl::sha1(names(data.r())[x]),
            choices = data.r()[[x]],
            value = mean(data.r()[[x]], na.rm = TRUE),
            label = parameter()$column.labels[x],
            step = parameter()$step[x],
            min. = floor(min(data.r()[[x]], na.rm = TRUE)),
            max. = ceiling(max(data.r()[[x]], na.rm = TRUE)),
            reset = reset
          )
        }
      } else{
        shiny::callModule(
          module = orTextual,
          id = openssl::sha1(names(data.r())[x]),
          choices = as.character(data.r()[[x]]),
          label = parameter()$column.labels[x],
          delimiter = parameter()$delimiter[x],
          multiple = parameter()$multiple[x],
          contains = parameter()$contains[x],
          reset = reset
        )
      }
    })
  })

  selection <- shiny::reactive({
    # new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Apply Filter")

    log_message(message = "Applying filter...", level = "INFO", token = session$token)

    or.modules <- modules()

    step <- 0.9 / length(or.modules)
    # OR modules selection
    or.selection.bool <- sapply(or.modules, function(x) {
      progress$inc(step, detail = x()$label)
      x()$bool
    })
    or.selection.text <- sapply(or.modules, function(x) {
      if(shiny::isTruthy(x()$text)){
        return(paste0(x()$label, ": ", paste(x()$text, collapse = ","), collapse = ""))
      }
    })

    # cast to matrix if sapply returns vector
    if(is.vector(or.selection.bool)){
      or.selection.bool <- t(as.matrix(or.selection.bool))
    }

    # selected rows (and selection)
    and.selection.bool <- apply(or.selection.bool, 1, all)

    or.selection.text <- unlist(or.selection.text)

    progress$set(1)

    log_message(message = "Done.", level = "INFO", token = session$token)

    return(list(bool = and.selection.bool, text = unlist(or.selection.text)))
  })

  return(selection)
}
