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
                         label = T
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
                         label = T
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
                         label = T
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
                       colorPicker2UI(id = ns("color"))
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
#' @param data data.table data visualized in plot (Supports reactive).
#' @param types data.table: (Supports reactive)
#'                        column1: colnames of data
#'                        column2: corresponding column type
#'                        column3: label (optional, used instead of id)
#'                        column4: sub_label (optional, added to id/ label)
#' @param x.names Character vector of column names(data column names) which will be available for x-axis. Can be reactive.
#' @param y.names Character vector of column names(data column names) which will be available for y-axis. Can be reactive.
#' @param z.names Character vector of column names(data column names) which will be available for z-axis. Can be reactive.
#' @param features data.table of the features to mark (first column = id)
#' @param markerReac reactive containing inputs of marker module.
#' @param plot.method Choose to rather render a 'interactive' or 'static' plot. Defaults to 'static'.
#' @param width Width of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param height Height of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param ppi Pixel per inch. Defaults to 72 and supports reactive.
#' @param scale Scale plot size. Defaults to 1, supports reactive.
#'
#' @return Returns reactive containing data used for plot.
#'
#' @details Make sure to have the same columnnames in data and features.
#'
#' @export
scatterPlot <- function(input, output, session, data, types, x.names = NULL, y.names = NULL, z.names = NULL, features = NULL, markerReac = NULL, plot.method = "static", width = "auto", height = "auto", ppi = 72, scale = 1) {
  #handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)){
      data <- data.table::copy(data())
    }else{
      data <- data.table::copy(data)
    }

    return(data)
  })
  #handle reactive features
  features.r <- shiny::reactive({
    if(shiny::is.reactive(features)){
      features <- features()
    }else{
      features <- features
    }

    return(features)
  })
  # handle reactive sizes
  size <- shiny::reactive({
    width <- ifelse(shiny::is.reactive(width), width(), width)
    height <- ifelse(shiny::is.reactive(height), height(), height)
    ppi <- ifelse(shiny::is.reactive(ppi), ppi(), ppi)
    scale <- ifelse(shiny::is.reactive(scale), scale(), scale)

    if(!is.numeric(width) || width <= 0) {
      width <- "auto"
    }
    if(!is.numeric(height) || height <= 0) {
      height <- "auto"
    }
    if(!is.numeric(ppi) || ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })

  # available types for corresponding axis
  types_x <- shiny::reactive({
    if(shiny::is.reactive(types)) {
      t <- types()
    } else {
      t <- types
    }

    if(shiny::is.reactive(x.names)) {
      x <- x.names()
    } else {
      x <- x.names
    }
    if(is.null(x)) return(t)

    t[key %in% x]
  })

  types_y <- shiny::reactive({
    if(shiny::is.reactive(types)) {
      t <- types()
    } else {
      t <- types
    }

    if(shiny::is.reactive(y.names)) {
      y <- y.names()
    } else {
      y <- y.names
    }
    if(is.null(y)) return(t)

    t[key %in% y]
  })

  types_z <- shiny::reactive({
    if(shiny::is.reactive(types)) {
      t <- types()
    } else {
      t <- types
    }

    if(shiny::is.reactive(z.names)) {
      z <- z.names()
    } else {
      z <- z.names
    }
    if(is.null(z)) return(t)

    t[key %in% z]
  })

  #Fetch the reactive guide for this module
  guide <- scatterPlotGuide(session, !is.null(markerReac))
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  data_x <- shiny::reactive(as.matrix(data.r()[, xaxis$selectedColumn(), with = FALSE]))
  data_y <- shiny::reactive(as.matrix(data.r()[, yaxis$selectedColumn(), with = FALSE]))

  # clear plot
  clearPlot <- shiny::reactiveVal(FALSE)

  #reset ui
  shiny::observeEvent(input$reset, {
    log_message("Scatterplot: reset", "INFO", token = session$token)

    shinyjs::reset("density")
    shinyjs::reset("line")
    shinyjs::reset("pointsize")
    shinyjs::reset("labelsize")
    shinyjs::reset("force_cat")
    xaxis <<- shiny::callModule(columnSelector, "xaxis", type.columns = types_x, columnTypeLabel = "Column type to choose from", labelLabel = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_x$method()))
    yaxis <<- shiny::callModule(columnSelector, "yaxis", type.columns = types_y, columnTypeLabel = "Column type to choose from", labelLabel = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_y$method()))
    zaxis <<- shiny::callModule(columnSelector, "zaxis", type.columns = types_z, columnTypeLabel = "Column type to choose from", multiple = FALSE, none = TRUE)
    colorPicker <<- shiny::callModule(colorPicker2, "color", distribution = list("sequential", "diverging"), winsorize = winsorize)
    transform_x <<- shiny::callModule(transformation, "transform_x", data = data_x)
    transform_y <<- shiny::callModule(transformation, "transform_y", data = data_y)
    limit_x <<- shiny::callModule(limit, "xaxis_limit", lower = shiny::reactive(result.data()$xlim[1]), upper = shiny::reactive(result.data()$xlim[2]))
    limit_y <<- shiny::callModule(limit, "yaxis_limit", lower = shiny::reactive(result.data()$ylim[1]), upper = shiny::reactive(result.data()$ylim[2]))
    clearPlot(TRUE)
  })

  winsorize <- shiny::reactive({
    if(zaxis$selectedColumn() != "") {
      equalize(result.data()$processed.data[, 4])
    } else {
      NULL
    }
  })

  xaxis <- shiny::callModule(columnSelector, "xaxis", type.columns = types_x, columnTypeLabel = "Column type to choose from", labelLabel = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_x$method()))
  yaxis <- shiny::callModule(columnSelector, "yaxis", type.columns = types_y, columnTypeLabel = "Column type to choose from", labelLabel = "Axis label", multiple = FALSE, suffix = shiny::reactive(transform_y$method()))
  zaxis <- shiny::callModule(columnSelector, "zaxis", type.columns = types_z, columnTypeLabel = "Column type to choose from", labelLabel = "Color label", multiple = FALSE, none = TRUE)

  colorPicker <- shiny::callModule(colorPicker2, "color", distribution = list("sequential", "diverging"), winsorize = winsorize)
  transform_x <- shiny::callModule(transformation, "transform_x", data = data_x)
  transform_y <- shiny::callModule(transformation, "transform_y", data = data_y)
  limit_x <- shiny::callModule(limit, "xaxis_limit", lower = shiny::reactive(result.data()$xlim[1]), upper = shiny::reactive(result.data()$xlim[2]))
  limit_y <- shiny::callModule(limit, "yaxis_limit", lower = shiny::reactive(result.data()$ylim[1]), upper = shiny::reactive(result.data()$ylim[2]))

  # select container dependend on plot.method
  if(plot.method == "static") {
    output$scatter <- shiny::renderUI({
      shinycssloaders::withSpinner(shiny::plotOutput(outputId = session$ns("static")), proxy.height = "800px")
    })
  }else if(plot.method == "interactive") {
    output$scatter <- shiny::renderUI({
      shinycssloaders::withSpinner(plotly::plotlyOutput(outputId = session$ns("interactive")), proxy.height = "800px")
    })
  }

  # show warning if there would more than 10 categories
  shiny::observe({
    shiny::req(!is.null(zaxis$selectedColumn()))

    # something selected?
    if(zaxis$selectedColumn() != "") {
      # categories used?
      if(input$force_cat || !is.numeric(data.r()[[zaxis$selectedColumn()]])) {
        cat_num <- length(unique(data.r()[[zaxis$selectedColumn()]]))

        if(cat_num > 10) {
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

  # disable plot if mandatory x- or y-axis missing
  shiny::observe({
    if(!isTruthy(xaxis$selectedColumn()) || !isTruthy(yaxis$selectedColumn())) {
      shinyjs::disable("plot")
    } else {
      shinyjs::enable("plot")
    }
  })

  transformed_data <- shiny::reactive({
    #reassemble after transformation
    if(zaxis$selectedColumn() != ""){
      z <- data.table::data.table(data.r()[, zaxis$selectedColumn(), with = FALSE])
      pre.data <- data.table::data.table(transform_x$data(), transform_y$data(), z)
    }else{
      pre.data <- data.table::data.table(transform_x$data(), transform_y$data())
    }

    #add rownames/ids
    return(data.table::data.table(data.r()[, 1], pre.data))
  })

  result.data <- shiny::eventReactive(input$plot, {
    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Computing data")

    result <- list(
      processed.data = NULL,
      highlight.color = NULL,
      highlight.labels = NULL,
      highlight.data = NULL,
      xlim = NULL,
      ylim = NULL
    )

    #get selected data
    progress$set(0.3, detail = "transforming")
    processed.data <- transformed_data()
    progress$set(0.5, detail = "selecting")

    #get axis limits
    result$xlim <- c(min(processed.data[, 2], na.rm = TRUE), max(processed.data[, 2], na.rm = TRUE))
    result$ylim <- c(min(processed.data[, 3], na.rm = TRUE), max(processed.data[, 3], na.rm = TRUE))

    #get highlighted data
    if(!is.null(markerReac) & !is.null(features.r())){

      result$highlight.color <- markerReac()$color
      if(markerReac()$highlight != "Disabled" & nrow(features.r()) > 0){
        # restrict label to 100 or less
        if(length(markerReac()$label) <= 100) {
          result$highlight.labels <- markerReac()$label
        } else {
          shiny::showNotification(
            id = session$ns("label-limit"),
            paste("Warning! Label restricted to 100 or less labels. Currently selected:", length(markerReac()$label), "Please select fewer genes to label."),
            duration = NULL,
            type = "warning"
          )
          shinyjs::runjs(paste0("$(document.getElementById('", paste0("shiny-notification-", session$ns("label-limit")), "')).addClass('notification-position-center');"))
        }
      }

      if(markerReac()$highlight == "Highlight" & nrow(features.r()) > 0){
        #overwrite columnnames because data.table makes unqiue names
        result$highlight.data <- processed.data[features.r()[, names(features.r())[1], with = FALSE], on = names(features.r())[1]]
        names(result$highlight.data) <- names(processed.data)
        #sort out highlighted data
        result$processed.data <- processed.data[!result$highlight.data, on = names(result$highlight.data)]
      }else if(markerReac()$highlight == "Exclusive" & nrow(features.r()) > 0){
        result$processed.data <- processed.data[features.r()[, names(features.r())[1], with = FALSE], on = names(features.r())[1]]
      }else{
        result$processed.data <- processed.data
      }
    }else{
      result$processed.data <- processed.data
    }

    progress$set(1)
    return(result)
  })

  #disable downloadbutton on init
  shinyjs::disable("download")

  plot <- shiny::eventReactive(input$plot, {
    log_message("Scatterplot: computing plot...", "INFO", token = session$token)

    #enable downloadbutton
    shinyjs::enable("download")
    clearPlot(FALSE)

    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Computing plot")

    colors <- colorPicker()$palette

    if(!is.null(limit_x())) {
      xlimit <- unlist(unname(limit_x()))
    } else {
      xlimit <- result.data()$xlim
    }
    if(!is.null(limit_y())) {
      ylimit <- unlist(unname(limit_y()))
    } else {
      ylimit <- result.data()$ylim
    }

    plot <- create_scatterplot(
      data = result.data()$processed.data,
      colors = colors,
      x_label = xaxis$label(),
      y_label = yaxis$label(),
      z_label = zaxis$label(),
      transparency = colorPicker()$transparency,
      pointsize = input$pointsize,
      labelsize = input$labelsize,
      density = input$density,
      line = input$line,
      highlight.data = result.data()$highlight.data,
      highlight.color = result.data()$highlight.color,
      highlight.labels = result.data()$highlight.labels,
      xlim = xlimit,
      ylim = ylimit,
      colorbar.limits = colorPicker()$winsorize,
      plot.method = plot.method,
      width = size()$width,
      height = size()$height,
      ppi = size()$ppi,
      scale = size()$scale,
      categorized = if(input$force_cat || ncol(result.data()$processed.data) >= 4 && !is.numeric(result.data()$processed.data[[4]])) TRUE else FALSE
    )

    progress$set(1)
    log_message("Scatterplot: done.", "INFO", token = session$token)
    return(plot)
  })

  # warning if plot size exceeds limits
  shiny::observe({
    if(plot()$exceed_size) {
      shiny::showNotification(
        ui = "Width and/ or height exceed limit. Using 500 cm instead.",
        id = "limit",
        type = "warning"
      )
    } else {
      shiny::removeNotification("limit")
    }
  })

  if(plot.method == "static") {
    output$static <- shiny::renderPlot(
      width = shiny::reactive(plot()$width * (plot()$ppi / 2.54)),
      height = shiny::reactive(plot()$height * (plot()$ppi / 2.54)),
      {
        if(clearPlot()) {
          return()
        } else {
          log_message("Scatterplot: render plot static", "INFO", token = session$token)

          plot()$plot
        }
      }
    )
  } else if(plot.method == "interactive") {
    output$interactive <- plotly::renderPlotly({
      if(clearPlot()) {
        return()
      } else {
        log_message("Scatterplot: render plot interactive", "INFO", token = session$token)

        #new progress indicator
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(0.2, message = "Render plot")

        plot <- plot()$plot

        progress$set(1)
        return(plot)
      }
    })
  }

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
      scheme = colorPicker()$name,
      reverse = colorPicker()$reverse,
      winsorize = colorPicker()$winsorize,
      transparency = colorPicker()$transparency,
      pointsize = input$pointsize,
      labelsize = input$labelsize)

    # format options
    options <- list(density = input$density, line = input$line)

    # format marker
    marker <- NULL
    if(!is.null(markerReac)) {
      marker <- markerReac()
    }

    #merge all
    all <- list(axis = axis, appearance = appearance, options = options, marker = marker)
  })

  return(shiny::reactive({unique(data.table::rbindlist(list(result.data()$processed.data, result.data()$highlight.data)))}))
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

  #add marker text to guide
  if(marker){
    steps <- append(steps,
                    list("guide_marker" = "<h4>Highlighting</h4>
                          If a set of features is selected, it is possible to either 'Highlight' those data points in the selected color or to show them 'Exclusively', omitting all other data points.<br/>
                          The label of each selected feature can be chosen from the 'Selected label columns' dropdown list."),
                    8
                    )
  }

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
