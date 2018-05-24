#' global correlation heatmap module UI representation
#'
#' @param id The ID of the modules namespace.
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}
#'
#' @export
global_cor_heatmapUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidPage(
      # load additional functionality (e.g. guide, disable ui)
      rintrojs::introjsUI(),
      shinyjs::useShinyjs(),
      # plot space
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          shiny::div(
            style = "overflow-y: scroll; overflow-x: scroll; height: 800px; text-align: center",
            shiny::uiOutput(ns("cor_heatmap"))
          )
        )
      ),
      # module options
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          collapsible = TRUE,
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::div(
                id = ns("guide_selection"),
                columnSelectorUI(id = ns("select"))
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                id = ns("guide_calculate"),
                shiny::selectInput(
                  inputId = ns("calc"),
                  label = "Calculate",
                  choices = c("distance", "correlation")
                ),
                shiny::selectInput(
                  inputId = ns("calc_method"),
                  label = "Calculation method",
                  choices = c("euclidean", "maximum", "manhattan", "canberra", "minkowski")
                )
              ),
              shiny::div(
                id = ns("guide_cluster"),
                shiny::selectInput(
                  inputId = ns("distance"),
                  label = "Cluster distance",
                  choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall"),
                  multiple = FALSE
                ),
                shiny::selectInput(
                  inputId = ns("method"),
                  label = "Cluster method",
                  choices = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty"),
                  multiple = FALSE
                )
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                id = ns("guide_transformation"),
                transformationUI(id = ns("transform"), choices = list(`None` = "raw", `regularized log` = "rlog"))
              ),
              shiny::div(
                id = ns("guide_coloring"),
                shiny::selectInput(
                  inputId = ns("distribution"),
                  label = "Data distribution",
                  choices = c("Sequential", "Diverging"),
                  multiple = FALSE
                ),
                colorPicker2UI(
                  id = ns("color"),
                  show.transparency = FALSE
                )
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                id = ns("guide_options"),
                shiny::textInput(
                  inputId = ns("label"),
                  label = "Unit label", placeholder = "Enter unit..."
                ),
                shiny::checkboxInput(
                  inputId = ns("row_label"),
                  label = "Row label",
                  value = TRUE
                ),
                shiny::checkboxInput(
                  inputId = ns("column_label"),
                  label = "Column label",
                  value = TRUE
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                id = ns("guide_buttons"),
                shiny::actionButton(
                  inputId = ns("plot"),
                  label = "Plot",
                  style = "color: #fff;  background-color: #3c8dbc"
                ),
                shiny::actionButton(
                  inputId = ns("reset"),
                  label = "Reset",
                  style = "color: #fff; background-color: #3c8dbc"
                ),
                shiny::actionButton(
                  inputId = ns("guide"),
                  label = "Launch guide",
                  style = "color: #fff; background-color: #3c8dbc",
                  icon = shiny::icon("question-circle")
                ),
                shiny::downloadButton(
                  outputId = ns("download"),
                  label = "Download"
                )
              )
            )
          )
        )
      )
    )
  )
}

#' global correlation heatmap module server logic
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#' @param clarion A clarion object. See \code{\link[wilson]{Clarion}}. (Supports reactive)
#' @param plot.method Choose which method is used for plotting. Either "static" or "interactive" (Default = "static").
#' @param width Width of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param height Height of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param ppi Pixel per inch. Defaults to 72 and supports reactive.
#' @param scale Scale plot size. Defaults to 1, supports reactive.
#'
#' @return Reactive containing data used for plotting.
#'
#' @export
global_cor_heatmap <- function(input, output, session, clarion, plot.method = "static", width = "auto", height = "auto", ppi = 72, scale = 1) {
  # globals -----------------------------------------------------------------
  # clear plot
  clearPlot <- shiny::reactiveVal(FALSE)
  # disable downloadButton on init
  shinyjs::disable("download")

  # load module -------------------------------------------------------------
  object <- shiny::reactive({
    # support reactive
    if (shiny::is.reactive(clarion)) {
      if (!methods::is(clarion(), "Clarion")) shiny::stopApp("Object of class 'Clarion' needed!")

      obj <- clarion()$clone(deep = TRUE)
    } else {
      if (!methods::is(clarion, "Clarion")) shiny::stopApp("Object of class 'Clarion' needed!")

      obj <- clarion$clone(deep = TRUE)
    }
  })

  # handle reactive sizes
  size <- shiny::reactive({
    width <- ifelse(shiny::is.reactive(width), width(), width)
    height <- ifelse(shiny::is.reactive(height), height(), height)
    ppi <- ifelse(shiny::is.reactive(ppi), ppi(), ppi)
    scale <- ifelse(shiny::is.reactive(scale), scale(), scale)

    if (!is.numeric(width) || width <= 0) {
      width <- "auto"
    }
    if (!is.numeric(height) || height <= 0) {
      if (plot.method == "interactive") {
        height <- 28
      } else {
        height <- "auto"
      }
    }
    if (!is.numeric(ppi) || ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })

  # load internal modules
  columns <- shiny::callModule(columnSelector, "select", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), columnTypeLabel = "Column types to choose from")
  transform <- shiny::callModule(transformation, "transform", data = shiny::reactive(as.matrix(object()$data[, columns$selectedColumns(), with = FALSE])))
  colorPicker <- shiny::callModule(colorPicker2, "color", distribution = shiny::reactive(tolower(input$distribution)), winsorize = shiny::reactive(equalize(result_data()[, -1])))

  # load dynamic ui
  if (plot.method == "static") {
    output$cor_heatmap <- shiny::renderUI({
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = session$ns("static")), proxy.height = "800px")
    })
  } else if (plot.method == "interactive") {
    output$cor_heatmap <- shiny::renderUI({
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = session$ns("interactive")), proxy.height = "800px")
    })
  }

  # functionality -----------------------------------------------------------
  # reset ui
  shiny::observeEvent(input$reset, {
    log_message("Global correlation heatmap: reset", "INFO", token = session$token)

    shinyjs::reset("calc")
    shinyjs::reset("calc_method")
    shinyjs::reset("distance")
    shinyjs::reset("method")
    shinyjs::reset("distribution")
    shinyjs::reset("label")
    shinyjs::reset("row_label")
    shinyjs::reset("column_label")
    columns <<- shiny::callModule(columnSelector, "select", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), columnTypeLabel = "Column types to choose from")
    transform <<- shiny::callModule(transformation, "transform", data = shiny::reactive(as.matrix(object()$data[, columns$selectedColumns(), with = FALSE])))
    colorPicker <<- shiny::callModule(colorPicker2, "color", distribution = shiny::reactive(tolower(input$distribution)), winsorize = shiny::reactive(equalize(result_data()[, -1])))
    clearPlot(TRUE)
  })

  # warning if plot size exceeds limits
  shiny::observe({
    if (plot()$exceed_size) {
      shiny::showNotification(
        ui = "Width and/ or height exceed limit. Using 500 cm instead.",
        id = session$ns("limit"),
        type = "warning"
      )
    } else {
      shiny::removeNotification(session$ns("limit"))
    }
  })

  # fetch the reactive guide for this module
  guide <- global_cor_heatmap_guide(session)
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  # show warning if not enough columns selected
  shiny::observe({
    shiny::req(columns$selectedColumns())

    if (length(columns$selectedColumns()) < 2) {
      shiny::showNotification(
        ui = "Warning! At least two columns needed. Please select more.",
        id = session$ns("less_data_warning"),
        type = "warning"
      )
    } else {
      shiny::removeNotification(session$ns("less_data_warning"))
    }
  })

  # enable/ disable plot button
  shiny::observe({
    if (!shiny::isTruthy(columns$selectedColumns()) || length(columns$selectedColumns()) < 2) {
      shinyjs::disable("plot")
    }else {
      shinyjs::enable("plot")
    }
  })

  # automatic unitlabel
  shiny::observe({
    shiny::updateTextInput(session = session, inputId = "label", value = transform$method())
  })

  # show right methods
  shiny::observe({
    if (input$calc == "distance") {
      shiny::updateSelectInput(session = session, inputId = "calc_method",
                               choices = c("euclidean", "maximum", "manhattan", "canberra", "minkowski"))
    } else if (input$calc == "correlation") {
      shiny::updateSelectInput(session = session, inputId = "calc_method", choices = c("spearman", "pearson", "kendall"))
    }
  })

  # plotting ----------------------------------------------------------------
  # preprocess data
  result_data <- shiny::eventReactive(input$plot, {
    # progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Compute data")

    # corellate data
    if (input$calc == "distance") {
      processed_data <- data.table::as.data.table(as.matrix(stats::dist(t(transform$data()), method = input$calc_method)), keep.rownames = "Names")
    } else if (input$calc == "correlation") {
      processed_data <- data.table::as.data.table(stats::cor(transform$data(), method = input$calc_method), keep.rownames = "Names")
    }

    # update progress indicator
    progress$set(1)

    return(processed_data)
  })

  # build plot object
  plot <- shiny::eventReactive(input$plot, {
    log_message("Global correlation heatmap: computing plot...", "INFO", token = session$token)

    # enable downloadButton
    shinyjs::enable("download")
    # show plot
    clearPlot(FALSE)

    # progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Building plot")

    # check if rlog failed
    if (is.null(attr(transform$data(), "betaPriorVar")) && is.null(attr(transform$data(), "intercept")) && transform$method() == "rlog") {
      shiny::showNotification("Regularized log failed (dispersion within 2 orders of magnitude)! Performing log2 instead.",
                              duration = 5,
                              type = "warning")

      if (input$label == "rlog") {
        unitlabel <- "log2"
        shiny::updateTextInput(session = session, inputId = "label", value = unitlabel)
      } else {
        unitlabel <- input$label
      }
    } else {
      unitlabel <- input$label
    }

    # call create/ building function
    plot <- create_heatmap(
      data = result_data(),
      unitlabel = unitlabel,
      row.label = input$row_label,
      row.custom.label = make.unique(columns$label()),
      column.label = input$column_label,
      column.custom.label = make.unique(columns$label()),
      clustering = "both",
      clustdist = input$distance,
      clustmethod = input$method,
      colors = colorPicker()$palette,
      width = size()$width,
      height = size()$height,
      ppi = size()$ppi,
      plot.method = plot.method,
      winsorize.colors = colorPicker()$winsorize,
      scale = size()$scale
    )

    # update progress indicator
    progress$set(1)

    log_message("Global correlation heatmap: done.", "INFO", token = session$token)
    return(plot)
  })

  # render plot
  if (plot.method == "static") {
    output$static <- shiny::renderPlot(
      width = shiny::reactive(plot()$width * (plot()$ppi / 2.54)),
      height = shiny::reactive(plot()$height * (plot()$ppi / 2.54)),
      {
        if (clearPlot()) {
          return()
        } else {
          log_message("Global correlation heatmap: render plot static", "INFO", token = session$token)

          # progress indicator
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(0.2, message = "Rendering plot")

          # get plot
          plot <- plot()$plot

          # update progress indicator
          progress$set(1)

          # draw plot
          return(ComplexHeatmap::draw(plot, heatmap_legend_side = "bottom"))
        }
      }
    )
  } else if (plot.method == "interactive") {
    output$interactive <- plotly::renderPlotly({
      if (clearPlot()) {
        return()
      } else {
        log_message("Global correlation heatmap: render plot interactive", "INFO", token = session$token)

        # progress indicator
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(0.2, message = "Rendering plot")

        plot <- plot()$plot

        # update progress indicator
        progress$set(1)

        return(plot)
      }
    })
  }

  output$download <- shiny::downloadHandler(filename = "global_correlation_heatmap.zip",
                                            content = function(file) {
                                              log_message("Global correlation heatmap: download", "INFO", token = session$token)

                                              download(file = file, filename = "global_correlation_heatmap.zip", plot = plot()$plot, width = plot()$width, height = plot()$height, ppi = plot()$ppi, ui = user_input())
                                            })

  user_input <- shiny::reactive({
    # format selection
    selection <- list(type = columns$type(), selectedColumns = columns$selectedColumns())

    # format calculation
    calculation <- list(
      calculate = input$calc,
      method = input$calc_method
    )

    # format clustering
    clustering <- list(
      distance = input$distance,
      method = input$method
    )

    # format options
    options <- list(
      transformation = transform$method(),
      color = list(distribution = input$distribution, scheme = colorPicker()$name, reverse = colorPicker()$reverse, winsorize = colorPicker()$winsorize),
      unit_label = input$label,
      row_label = input$row_label,
      column_label = input$column_label
    )

    # merge all
    all <- list(selection = selection, calculation = calculation, clustering = clustering, options = options)
  })

  # return plotting data
  return(result_data)
}

#' global correlation heatmap module guide
#'
#' @param session The shiny session
#'
#' @return A shiny reactive that contains the texts for the Guide steps.
#'
global_cor_heatmap_guide <- function(session) {
  steps <- list(
    "guide_selection" = "<h4>Data selection</h4>
    Select a column type for visualisation, then select individual columns from all columns of the chosen type.",
    "guide_calculate" = "<h4>Apply calculation</h4>
      Either apply a distance function or correlate the data. Also choose which method should be used in order to do so.",
    "guide_cluster" = "<h4>Row & Column clustering</h4>
    Select a clustering distance and method.",
    "guide_transformation" = "<h4>Data transformation</h4>
    Pick a transformation that you want to apply to your data or leave it as 'None' if no transformation is needed.",
    "guide_coloring" = "<h4>Color palettes</h4>
    Based on the selected data distribution, available color palettes are either sequential or diverging.<br/>
    The selected palette can additionally be reversed.<br/>
    Set the limits of the color palette with 'Winsorize to upper/lower'. Out of bounds values will be mapped to the nearest color.",
    "guide_options" = "<h4>Additional options</h4>
    You can set a label for the color legend that describes the underlying data unit. Furthermore, you can enable/disable row and column labels.",
    "guide_buttons" = "<h4>Create the plot</h4>
    As a final step click, a click on the Plot button will render the plot, while a click on the reset button will reset the parameters to default."
  )

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
