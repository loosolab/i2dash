#' scatterPlot module UI representation
#'
#' @param id The ID of the modules namespace.
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
scatterPlotUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(shiny::fluidPage(
    shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "wilson_www/styles.css"))),
    shiny::fluidRow(shinydashboard::box(
      width = 12,
      shiny::div(style = "overflow-y: scroll; overflow-x: scroll; height: 800px; text-align: center",
                 shiny::uiOutput(outputId = ns("scatter"))
      )
    )),
    shiny::fluidRow(
      rintrojs::introjsUI(),
      shinyjs::useShinyjs(),
      shinydashboard::box(
        width = 12,
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_xaxis"),
                       columnSelectorUI(
                         id = ns("xaxis"),
                         title = "Data on x-axis",
                         label = TRUE
                       )
            ),
            shiny::div(id = ns("guide_xaxis_transformation"),
                       transformationUI(id = ns("transform_x"), label = "Transformation", choices = list(`None` = "raw", `log2` = "log2", `-log2` = "-log2", `log10` = "log10", `-log10` = "-log10", `Z score` = "zscore"))
            ),
            shiny::div(id = ns("guide_xaxis_limit"),
                       limitUI(id = ns("xaxis_limit"), label = "Limit axis"))
          ),
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_yaxis"),
                       columnSelectorUI(
                         id = ns("yaxis"),
                         title = "Data on y-axis",
                         label = TRUE
                       )
            ),
            shiny::div(id = ns("guide_yaxis_transformation"),
                       transformationUI(id = ns("transform_y"), label = "Transformation", choices = list(`None` = "raw", `log2` = "log2", `-log2` = "-log2", `log10` = "log10", `-log10` = "-log10", `Z score` = "zscore"))
            ),
            shiny::div(id = ns("guide_yaxis_limit"),
                       limitUI(id = ns("yaxis_limit"), label = "Limit axis"))

          ),
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_zaxis"),
                       columnSelectorUI(
                         id = ns("zaxis"),
                         title = "Data on z-axis",
                         label = TRUE
                       ),
                       shiny::checkboxInput(
                         inputId = ns("force_cat"),
                         label = "Force categories",
                         value = FALSE
                       )
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_color"),
                       colorPickerUI(id = ns("color"))
            ),
            shiny::div(id = ns("guide_pointsize"),
                       shiny::sliderInput(
                         ns("pointsize"),
                         "Point Size",
                         min = 0.1,
                         max = 10,
                         value = 0.4
                       ),
                       shiny::sliderInput(
                         ns("labelsize"),
                         "Label Size",
                         min = 1,
                         max = 20,
                         value = 5,
                         round = TRUE
                       )
            ),
            shiny::div(id = ns("guide_options"),
                       shiny::HTML("<strong>Additional options</strong>"),
                       shiny::checkboxInput(ns("density"), "Add 2D kernel density estimate", value = FALSE),
                       shiny::checkboxInput(ns("line"), "Add reference line", value = TRUE)
            )
          )
        ),
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::div(id = ns("guide_buttons"),
                     shiny::actionButton(ns("plot"), "Plot", style = "color: #fff; background-color: #3c8dbc"),
                     shiny::actionButton(ns("reset"), "Reset", style = "color: #fff; background-color: #3c8dbc"),
                     shiny::actionButton(ns("guide"), "Launch guide", style = "color: #fff; background-color: #3c8dbc", icon = shiny::icon("question-circle")),
                     shiny::downloadButton(outputId = ns("download"), label = "Download")
          )
        ))
      )
    )
  ))
}

#' scatterPlot module server logic
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#' @param clarion A clarion object. See \code{\link[wilson]{Clarion}}. (Supports reactive)
#' @param marker.output Marker module output. See \code{\link[wilson]{marker}}.
#' @param plot.method Choose to rather render a 'interactive' or 'static' plot. Defaults to 'static'.
#' @param width Width of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param height Height of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param ppi Pixel per inch. Defaults to 72 and supports reactive.
#' @param scale Scale plot size. Defaults to 1, supports reactive.
#'
#' @return Returns reactive containing data used for plot.
#'
#' @details As markerOutput provides a second dataset used for highlighting it is crucial for it to have the same columnnames as the dataset provided by clarion.
#' @details Intersections between marker and clarion will be removed from clarion in favor of highlighting them.
#'
#' @export
scatterPlot <- function(input, output, session, clarion, marker.output = NULL, plot.method = "static", width = "auto", height = "auto", ppi = 72, scale = 1) {
  # globals/ initialization #####
  # clear plot
  clear_plot <- shiny::reactiveVal(FALSE)
  # disable downloadbutton on init
  shinyjs::disable("download")
  # set labelsize default for interactive
  if (plot.method == "interactive") shiny::updateSliderInput(session = session, inputId = "labelsize", value = 10)

  # input preparation #####
  object <- shiny::reactive({
    # support reactive
    if (shiny::is.reactive(clarion)) {
      if (!methods::is(clarion(), "Clarion")) shiny::stopApp("Object of class 'Clarion' needed!")

      clarion()$clone(deep = TRUE)
    } else {
      if (!methods::is(clarion, "Clarion")) shiny::stopApp("Object of class 'Clarion' needed!")

      clarion$clone(deep = TRUE)
    }
  })
  # create deep copy of marker data if existing
  if (!is.null(marker.output)) {
    marker_object <- shiny::reactive({
      marker.output$clarion()$clone(deep = TRUE)
    })
  }

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
      height <- "auto"
    }
    if (!is.numeric(ppi) || ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })

  # modules/ ui #####
  xaxis <- shiny::callModule(columnSelector, "xaxis", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column type to choose from", label.label = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_x$method()))
  yaxis <- shiny::callModule(columnSelector, "yaxis", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column type to choose from", label.label = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_y$method()))
  zaxis <- shiny::callModule(columnSelector, "zaxis", type.columns = shiny::reactive(object()$metadata[, intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column type to choose from", label.label = "Color label", multiple = FALSE, none = TRUE)
  color <- shiny::callModule(colorPicker, "color", distribution = "all", winsorize = shiny::reactive(switch(shiny::isTruthy(zaxis$selected_column()) + 1, NULL, equalize(object()$data[, zaxis$selected_column(), with = FALSE]))))
  transform_x <- shiny::callModule(transformation, "transform_x", data = shiny::reactive(as.matrix(object()$data[, xaxis$selected_column(), with = FALSE])))
  transform_y <- shiny::callModule(transformation, "transform_y", data = shiny::reactive(as.matrix(object()$data[, yaxis$selected_column(), with = FALSE])))
  # transform highlight data
  if (!is.null(marker.output)) {
    # note: same id as transform_x/y so it depends on same ui
    highlight_transform_x <- shiny::callModule(transformation, "transform_x", data = shiny::reactive(as.matrix(marker_object()$data[, xaxis$selected_column(), with = FALSE])))
    highlight_transform_y <- shiny::callModule(transformation, "transform_y", data = shiny::reactive(as.matrix(marker_object()$data[, yaxis$selected_column(), with = FALSE])))
  }
  limit_x <- shiny::callModule(limit, "xaxis_limit", lower = shiny::reactive(result_data()$xlim[1]), upper = shiny::reactive(result_data()$xlim[2]))
  limit_y <- shiny::callModule(limit, "yaxis_limit", lower = shiny::reactive(result_data()$ylim[1]), upper = shiny::reactive(result_data()$ylim[2]))

  # select container dependend on plot.method
  if (plot.method == "static") {
    output$scatter <- shiny::renderUI({
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = session$ns("static")), proxy.height = "800px")
    })
  } else if (plot.method == "interactive") {
    output$scatter <- shiny::renderUI({
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = session$ns("interactive")), proxy.height = "800px")
    })
  }

  # functionality/ plotting #####
  # reset ui
  shiny::observeEvent(input$reset, {
    log_message("Scatterplot: reset", "INFO", token = session$token)

    shinyjs::reset("density")
    shinyjs::reset("line")
    shinyjs::reset("pointsize")
    shinyjs::reset("labelsize")
    shinyjs::reset("force_cat")
    xaxis <<- shiny::callModule(columnSelector, "xaxis", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column type to choose from", label.label = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_x$method()))
    yaxis <<- shiny::callModule(columnSelector, "yaxis", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column type to choose from", label.label = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_y$method()))
    zaxis <<- shiny::callModule(columnSelector, "zaxis", type.columns = shiny::reactive(object()$metadata[, intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column type to choose from", label.label = "Color label", multiple = FALSE, none = TRUE)
    color <<- shiny::callModule(colorPicker, "color", distribution = "all", winsorize = shiny::reactive(switch(shiny::isTruthy(zaxis$selected_column()) + 1, NULL, equalize(object()$data[, zaxis$selected_column(), with = FALSE]))))
    transform_x <<- shiny::callModule(transformation, "transform_x", data = shiny::reactive(as.matrix(object()$data[, xaxis$selected_column(), with = FALSE])))
    transform_y <<- shiny::callModule(transformation, "transform_y", data = shiny::reactive(as.matrix(object()$data[, yaxis$selected_column(), with = FALSE])))
    # transform highlight data
    if (!is.null(marker.output)) {
      # note: same id as transform_x/y so it depends on same ui
      highlight_transform_x <<- shiny::callModule(transformation, "transform_x", data = shiny::reactive(as.matrix(marker_object()$data[, xaxis$selected_column(), with = FALSE])))
      highlight_transform_y <<- shiny::callModule(transformation, "transform_y", data = shiny::reactive(as.matrix(marker_object()$data[, yaxis$selected_column(), with = FALSE])))
    }
    limit_x <<- shiny::callModule(limit, "xaxis_limit", lower = shiny::reactive(result_data()$xlim[1]), upper = shiny::reactive(result_data()$xlim[2]))
    limit_y <<- shiny::callModule(limit, "yaxis_limit", lower = shiny::reactive(result_data()$ylim[1]), upper = shiny::reactive(result_data()$ylim[2]))
    clear_plot(TRUE)
  })

  # disable plot if mandatory x- or y-axis missing
  shiny::observe({
    if (!shiny::isTruthy(xaxis$selected_column()) || !shiny::isTruthy(yaxis$selected_column())) {
      shinyjs::disable("plot")
    } else {
      shinyjs::enable("plot")
    }
  })

  transformed_data <- shiny::reactive({
    # reassemble after transformation
    # columns: unique_id, x, y(, z)
    if (shiny::isTruthy(zaxis$selected_column())) {
      z <- object()$data[, zaxis$selected_column(), with = FALSE]
      data.table::data.table(object()$data[, object()$get_id(), with = FALSE], transform_x$data(), transform_y$data(), z)
    } else {
      data.table::data.table(object()$data[, object()$get_id(), with = FALSE], transform_x$data(), transform_y$data())
    }
  })

  if (!is.null(marker.output)) {
    highlight_data <- shiny::reactive({
      # return null on empty table
      if (nrow(marker.output$clarion()$data) == 0) return()
      # reassemble after transformation
      # columns: unique_id, x, y(, z)
      if (shiny::isTruthy(zaxis$selected_column())) {
        z <- marker_object()$data[, zaxis$selected_column(), with = FALSE]
        data.table::data.table(marker_object()$data[, marker_object()$get_id(), with = FALSE], highlight_transform_x$data(), highlight_transform_y$data(), z)
      } else {
        data.table::data.table(marker_object()$data[, marker_object()$get_id(), with = FALSE], highlight_transform_x$data(), highlight_transform_y$data())
      }
    })
  }

  result_data <- shiny::eventReactive(input$plot, {
    # new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Computing data")

    result <- list(
      processed_data = NULL,
      data_label = NULL,
      data_hovertext = NULL,
      highlight_color = NULL,
      highlight_label = NULL,
      highlight_hovertext = NULL,
      highlight_data = NULL,
      xlim = NULL,
      ylim = NULL
    )

    # get selected data
    progress$set(0.3, detail = "transforming")
    processed_data <- transformed_data()
    progress$set(0.5, detail = "selecting")

    # no highlighting either disabled or N/A
    if (is.null(marker.output) || is.null(highlight_data()) || marker.output$highlight() == "Disabled") {
      # get axis limits
      result$xlim <- c(min(processed_data[, 2], na.rm = TRUE), max(processed_data[, 2], na.rm = TRUE))
      result$ylim <- c(min(processed_data[, 3], na.rm = TRUE), max(processed_data[, 3], na.rm = TRUE))

      # add name to hovertext
      if (plot.method == "interactive" && object()$get_name() != object()$get_id()) {
        result$data_hovertext <- object()$data[[object()$get_name()]]
      }

      result$processed_data <- processed_data
    } else {
      # get highlight data
      highlight_data <- highlight_data()

      # get axis limit including both datasets
      result$xlim <- c(min(processed_data[, 2], highlight_data[, 2], na.rm = TRUE), max(processed_data[, 2], highlight_data[, 2], na.rm = TRUE))
      result$ylim <- c(min(processed_data[, 3], highlight_data[, 3], na.rm = TRUE), max(processed_data[, 3], highlight_data[, 3], na.rm = TRUE))

      # get colors
      result$highlight_color <- marker.output$color()

      if (marker.output$highlight() == "Highlight") {
        # omit duplicates from processed.data
        processed_data <- data.table::fsetdiff(x = processed_data, y = highlight_data)

        # for everything duplicated = empty processed.data
        if (nrow(processed_data) == 0) {
          # notification that highlight color will be ignored
          shiny::showNotification(
            id = session$ns("full_highlight"),
            ui = "Ignoring highlight color as complete dataset is selected. Otherwise z-axis coloring would be lost.",
            duration = NULL,
            type = "warning"
          )
          # show notification in center
          shinyjs::runjs(paste0("$(document.getElementById('", paste0("shiny-notification-", session$ns("full_highlight")), "')).addClass('notification-position-center');"))

          # add name to hovertext
          if (plot.method == "interactive" && marker_object()$get_name() != marker_object()$get_id()) {
            result$data_hovertext <- marker_object()$data[[marker_object()$get_name()]]
          }

          result$processed_data <- highlight_data
        } else {
          # add name to hovertext
          if (plot.method == "interactive" && object()$get_name() != object()$get_id()) {
            # only keep selected rows
            result$data_hovertext <- object()$data[processed_data, on = object()$get_id()][[object()$get_name()]]
          }
          # add name to hovertext
          if (plot.method == "interactive" && marker_object()$get_name() != marker_object()$get_id()) {
            result$highlight_hovertext <- marker_object()$data[[marker_object()$get_name()]]
          }

          result$processed_data <- processed_data
          result$highlight_data <- highlight_data
        }

        # set label; ignore if more than 100
        if (length(marker.output$label()) <= 100) {
          if (nrow(processed_data) == 0) {
            result$data_label <- marker.output$label()
          } else {
            result$highlight_label <- marker.output$label()
          }
        }
      } else if (marker.output$highlight() == "Exclusive") {
        # add name to hovertext
        if (plot.method == "interactive" && marker_object()$get_name() != marker_object()$get_id()) {
          result$data.hovertext <- marker_object()$data[[marker_object()$get_name()]]
        }

        result$processed_data <- highlight_data

        # set label; ignore if more than 100
        if (length(marker.output$label()) <= 100) {
          result$data_label <- marker.output$label()
        }
      }
    }

    progress$set(1)
    return(result)
  })

  plot <- shiny::eventReactive(input$plot, {
    log_message("Scatterplot: computing plot...", "INFO", token = session$token)

    # enable downloadbutton
    shinyjs::enable("download")
    clear_plot(FALSE)

    # new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Computing plot")

    if (!is.null(limit_x())) {
      xlimit <- unlist(limit_x())
    } else {
      xlimit <- result_data()$xlim
    }
    if (!is.null(limit_y())) {
      ylimit <- unlist(limit_y())
    } else {
      ylimit <- result_data()$ylim
    }

    plot <- create_scatterplot(
      data = result_data()$processed_data,
      data.labels = result_data()$data_label,
      data.hovertext <- result_data()$data_hovertext,
      color = color()$palette,
      x_label = xaxis$label(),
      y_label = yaxis$label(),
      z_label = zaxis$label(),
      transparency = color()$transparency,
      pointsize = input$pointsize,
      labelsize = input$labelsize,
      density = input$density,
      line = input$line,
      highlight.data = result_data()$highlight_data,
      highlight.color = result_data()$highlight_color,
      highlight.labels = result_data()$highlight_label,
      highlight.hovertext = result_data()$highlight_hovertext,
      xlim = xlimit,
      ylim = ylimit,
      colorbar.limits = color()$winsorize,
      plot.method = plot.method,
      width = size()$width,
      height = size()$height,
      ppi = size()$ppi,
      scale = size()$scale,
      categorized = if (input$force_cat || ncol(result_data()$processed_data) >= 4 && !is.numeric(result_data()$processed_data[[4]])) TRUE else FALSE
    )

    progress$set(1)
    log_message("Scatterplot: done.", "INFO", token = session$token)
    return(plot)
  })

  # render plot #####
  if (plot.method == "static") {
    output$static <- shiny::renderPlot(
      width = shiny::reactive(plot()$width * (plot()$ppi / 2.54)),
      height = shiny::reactive(plot()$height * (plot()$ppi / 2.54)),
      {
        if (clear_plot()) {
          return()
        } else {
          log_message("Scatterplot: render plot static", "INFO", token = session$token)

          plot()$plot
        }
      }
    )
  } else if (plot.method == "interactive") {
    output$interactive <- plotly::renderPlotly({
      if (clear_plot()) {
        return()
      } else {
        log_message("Scatterplot: render plot interactive", "INFO", token = session$token)

        # new progress indicator
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(0.2, message = "Render plot")

        plot <- plot()$plot

        progress$set(1)
        return(plot)
      }
    })
  }

  # download #####
  output$download <- shiny::downloadHandler(filename = "scatterPlot.zip",
    content = function(file) {
      log_message("Scatterplot: download", "INFO", token = session$token)
      download(file = file, filename = "scatterPlot.zip", plot = plot()$plot, width = plot()$width, height = plot()$height, ppi = plot()$ppi, ui = user_input())
    }
  )

  user_input <- shiny::reactive({
    # format axis
    xaxis <- lapply(xaxis, function(x) { x() })
    yaxis <- lapply(yaxis, function(x) { x() })
    zaxis <- lapply(zaxis, function(x) { x() })
    ## transformation
    xaxis <- append(xaxis, list(transformation = transform_x$method(), limit = limit_x()), after = length(xaxis))
    yaxis <- append(yaxis, list(transformation = transform_y$method(), limit = limit_y()), after = length(yaxis))

    axis <- list(xaxis = xaxis, yaxis = yaxis, zaxis = zaxis)

    # format appearance
    appearance <- list(
      scheme = color()$name,
      reverse = color()$reverse,
      winsorize = color()$winsorize,
      transparency = color()$transparency,
      pointsize = input$pointsize,
      labelsize = input$labelsize)

    # format options
    options <- list(density = input$density, line = input$line)

    # format marker
    marker <- NULL
    if (!is.null(marker.output)) {
      marker <- list(
        highlight = marker.output$highlight(),
        color = marker.output$color(),
        label_column = marker.output$label_column(),
        label = marker.output$label()
      )
    }

    # merge all
    list(axis = axis, appearance = appearance, options = options, marker = marker)
  })

  # notifications #####
  # show warning if there would be more than 10 categories
  shiny::observe({
    # something selected?
    if (shiny::isTruthy(zaxis$selected_column())) {
      # categories used?
      if (input$force_cat || !is.numeric(object()$data[[zaxis$selected_column()]])) {
        cat_num <- length(unique(object()$data[[zaxis$selected_column()]]))

        if (cat_num > 10) {
          shiny::showNotification(
            id = session$ns("cat-limit"),
            paste("Warning! There are", cat_num, "different categories selected. This can result in unexpected behavior. Recommended are 10 or less categories."),
            duration = NULL,
            type = "warning"
          )

          shinyjs::runjs(paste0("$(document.getElementById('", paste0("shiny-notification-", session$ns("cat-limit")), "')).addClass('notification-position-center');"))
        } else {
          shiny::removeNotification(session$ns("cat-limit"))
        }
      } else {
        shiny::removeNotification(session$ns("cat-limit"))
      }
    } else {
      shiny::removeNotification(session$ns("cat-limit"))
    }
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

  # label restriction warning
  if (!is.null(marker.output)) {
    shiny::observe({
      if (marker.output$highlight() != "Disabled" && length(marker.output$label()) > 100) {
        shiny::showNotification(
          id = session$ns("label-limit"),
          paste("Warning! Label restricted to 100 or less labels. Currently selected:", length(marker.output$label()), "Please select fewer genes to label or else they will be ignored."),
          duration = NULL,
          type = "warning"
        )
        shinyjs::runjs(paste0("$(document.getElementById('", paste0("shiny-notification-", session$ns("label-limit")), "')).addClass('notification-position-center');"))
      } else {
        shiny::removeNotification(session$ns("label-limit"))
      }
    })
  }

  # Fetch the reactive guide for this module
  guide <- scatterPlotGuide(session, !is.null(marker.output))
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  return(shiny::reactive({unique(data.table::rbindlist(list(result_data()$processed_data, result_data()$highlight_data)))}))
}

#' scatterPlot module guide
#'
#' @param session The shiny session
#' @param marker Logical if marker step should be enabled (Default = FALSE).
#'
#' @return A shiny reactive that contains the texts for the Guide steps.
#'
scatterPlotGuide <- function(session, marker = FALSE) {
  steps <- list(
    "guide_xaxis" = "<h4>Data selection: x-axis</h4>
      Select a column type for visualization, then select an individual column of the chosen type.<br />
      You can also set a customized label for the axis. If left empty, the column name will be used as default.",
    "guide_xaxis_transformation" = "<h4>X-axis transformation</h4>
      Pick a transformation that you want to apply to your data or leave it as 'None' if no transformation is needed.",
    "guide_xaxis_limit" = "<h4>X-axis limit</h4>
      Use upper/ lower limit to customize the axis limits.",
    "guide_yaxis" = "<h4>Data selection: y-axis</h4>
      Select a column type for visualization, then select an individual column of the chosen type.<br />
      You can also set a customized label for the axis. If left empty, the column name will be used as default.",
    "guide_yaxis_transformation" = "<h4>Y-axis transformation</h4>
      Pick a transformation that you want to apply to your data or leave it as 'None' if no transformation is needed.",
    "guide_yaxis_limit" = "<h4>Y-axis limit</h4>
      Use upper/ lower limit to customize the axis limits.",
    "guide_zaxis" = "<h4>Data selection: z-axis</h4>
      Select a column type for visualization, then select an individual column of the chosen type. The data from the selected column will be mapped onto a color scale. <br />
      You can also set a customized label for the color bar. If left empty, the column name will be used as default.
      If there is a non numeric column selected scatterplot will attempt to do a categorized coloring approach, whereas numeric data will result in a gradient. Use 'force categories' to achieve categorization with numeric values.",
    "guide_color" = "<h4>Color palettes</h4>
      Based on the data distribution, select either a sequential or diverging color palette.<br/>
      Choose the range of the color legend by defining it's upper and lower limits with 'Winsorize to upper/lower'. Be aware that out of bounds values will be mapped to their nearest color.<br/>
      Additionally, color palettes can be reversed and colors can be adjusted in transparency.",
    "guide_pointsize" = "<h4>Pointsize/ Labelsize</h4>
      You can increase or decrease the point size by dragging the slider to the right or to the left. The same applies to the label size and it's respecting slider.",
    "guide_options" = "<h4>Additional options</h4>
      You can add a 2D kernel density estimate and/or a reference line in the form of y = x to the plot by ticking 'Add 2D kernel density estimate' or 'Add reference line'.<br/>
      Note that the computation of the 2D kernel density estimate might take a while.",
    "guide_buttons" = "<h4>Create the plot</h4>
      As a final step, a click on the 'Plot' button will render the plot, while a click on the 'Reset' button will reset the parameters to default."
  )

  # add marker text to guide
  if (marker) {
    steps <- append(steps,
                    list("guide_marker" = "<h4>Highlighting</h4>
                          If a set of features is selected, it is possible to either 'Highlight' those data points in the selected color or to show them 'Exclusively', omitting all other data points.<br/>
                          The label of each selected feature can be chosen from the 'Selected label columns' dropdown list."),
                    8
                    )
  }

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
