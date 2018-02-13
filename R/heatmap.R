#' heatmap module UI representation
#'
#' @param id The ID of the modules namespace.
#' @param row.label Boolean Value set initial Value for rowlabel checkbox (Default = TRUE).
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
heatmapUI <- function(id, row.label = TRUE) {
  ns <- shiny::NS(id)

  shiny::tagList(shiny::fluidPage(
    rintrojs::introjsUI(),
    shinyjs::useShinyjs(),
    shiny::fluidRow(shinydashboard::box(width = 12,
                                        shiny::div(style = "overflow-y: scroll; overflow-x: scroll; height: 800px; text-align: center",
                                                   shiny::uiOutput(ns("heatmap"))))),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_selection"),
                       columnSelectorUI(id = ns("select")))),
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_cluster"),
                       shiny::selectInput(
                         ns("clustering"),
                         label = "Choose clustering",
                         choices = c("columns and rows" = "both", "only columns" = "column", "only rows" = "row", "no clustering" = "none"),
                         multiple = FALSE
                       ),
                       shiny::selectInput(
                         ns("cluster.distance"),
                         label = "Cluster distance",
                         choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall"),
                         multiple = FALSE
                       ),
                       shiny::selectInput(
                         ns("cluster.method"),
                         label = "Cluster method",
                         choices = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty"),
                         multiple = FALSE))
          ),
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_transformation"),
                       transformationUI(id = ns("transform"), choices = list(`None` = "raw", `log2` = "log2", `-log2` = "-log2", `log10` = "log10", `-log10` = "-log10", `Z score` = "zscore"), transposeOptions = TRUE)
            ),
            shiny::div(id = ns("guide_coloring"),
                       shiny::selectInput(
                         ns("distribution"),
                         label = "Data distribution",
                         choices = c("Sequential", "Diverging"),
                         multiple = FALSE
                       ),
                       colorPicker2UI(ns("color"), show.transparency = FALSE)
            )
          ),
          shiny::column(
            width = 3,
            shiny::div(id = ns("guide_options"),
                       shiny::textInput(ns("label"), label = "Unit label", placeholder = "Enter unit..."),
                       shiny::checkboxInput(ns("row.label"), label = "Row label", value = row.label),
                       labelUI(ns("labeller")),
                       shiny::checkboxInput(ns("column.label"), label = "Column label", value = TRUE)
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(id = ns("guide_buttons"),
                shiny::actionButton(ns("plot"), "Plot", style = "color: #fff; background-color: #3c8dbc"),
                shiny::actionButton(ns("reset"), "Reset", style = "color: #fff; background-color: #3c8dbc"),
                shiny::actionButton(ns("guide"), "Launch guide", style = "color: #fff; background-color: #3c8dbc", icon = shiny::icon("question-circle")),
                shiny::downloadButton(outputId = ns("download"), label = "Download")
            )
          )
        )
      )
    )
  ))
}

#' heatmap module server logic
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#' @param data data.table data visualized in plot (Supports reactive).
#' @param types data.table: (Supports reactive)
#'                        column1: colnames of data
#'                        column2: corresponding column typ
#'                        column3: label (optional, used instead of id)
#'                        column4: sub_label (optional, added to id/ label)
#' @param plot.method Choose which method is used for plotting. Either "static" or "interactive" (Default = "static").
#' @param custom.row.label Data.table used for creating custom labels (supports reactive).
#' @param label.sep Seperator used for label merging (Default = ", ").
#' @param width Width of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param height Height of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param ppi Pixel per inch. Defaults to 72 and supports reactive.
#' @param scale Scale plot size. Defaults to 1, supports reactive.
#'
#' @return Reactive containing data used for plotting.
#'
#' @export
heatmap <- function(input, output, session, data, types, plot.method = "static", custom.row.label = NULL, label.sep = ", ", width = "auto", height = "auto", ppi = 72, scale = 1) {
  # cluster limitation
  static <- 11000
  interactive <- 3000

  #handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)){
      data <- data.table::copy(data())
    }else{
      data <- data.table::copy(data)
    }
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
      if(plot.method == "interactive") {
        height <- 28
      }else {
        height <- "auto"
      }
    }
    if(!is.numeric(ppi) || ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })

  # Fetch the reactive guide for this module
  guide <- heatmapGuide(session, !is.null(custom.row.label))
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  #notification
  shiny::observe({
    shiny::req(data.r())

    if(shiny::isTruthy(columns$selectedColumns())){
      if(input$clustering != "none") { # clustering
        if(plot.method == "static" && nrow(data.r()) > static) { # cluster limitation (static)
          shiny::showNotification(
            paste("Clustering limited to", static, "genes! Please disable clustering or select less genes."),
            duration = NULL,
            type = "error",
            id = session$ns("notification"),
            closeButton = FALSE
          )
        } else if(plot.method == "interactive" && nrow(data.r()) > interactive) { # cluster limitation (interactive)
          shiny::showNotification(
            paste("Clustering limited to", interactive, "genes! Please disable clustering or select less genes."),
            duration = NULL,
            type = "error",
            id = session$ns("notification"),
            closeButton = FALSE
          )
        } else {
          shiny::removeNotification(session$ns("notification"))
        }
      } else if(nrow(data.r()) > 200) { # computation warning
        shiny::showNotification(
          paste("Caution! You selected", nrow(data.r()), "genes. This will take a while to compute."),
          duration = 5,
          type = "warning",
          id = session$ns("notification"),
          closeButton = FALSE
        )
      } else {
        shiny::removeNotification(session$ns("notification"))
      }
    }else{
      shiny::removeNotification(session$ns("notification"))
    }
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

  # clear plot
  clearPlot <- shiny::reactiveVal(FALSE)

  # reset ui
  shiny::observeEvent(input$reset, {
    shinyjs::reset("cluster.distance")
    shinyjs::reset("cluster.method")
    shinyjs::reset("clustering")
    shinyjs::reset("distribution")
    shinyjs::reset("label")
    shinyjs::reset("row.label")
    shinyjs::reset("column.label")
    columns <<- shiny::callModule(columnSelector, "select", type.columns = types, columnTypeLabel = "Column types to choose from")
    transform <<- shiny::callModule(transformation, "transform", data = shiny::reactive(as.matrix(data.r()[, columns$selectedColumns(), with = FALSE])))
    colorPicker <<- shiny::callModule(colorPicker2, "color", distribution = shiny::reactive(tolower(input$distribution)), winsorize = shiny::reactive(equalize(transform$data())))
    if(!is.null(custom.row.label)) {
      custom_label <<- shiny::callModule(label, "labeller", data = custom.row.label, label = "Select row label", sep = label.sep, disable = shiny::reactive(!input$row.label))
    }
    clearPlot(TRUE)
  })

  columns <- shiny::callModule(columnSelector, "select", type.columns = types, columnTypeLabel = "Column types to choose from")
  transform <- shiny::callModule(transformation, "transform", data = shiny::reactive(as.matrix(data.r()[, columns$selectedColumns(), with = FALSE])))
  colorPicker <- shiny::callModule(colorPicker2, "color", distribution = shiny::reactive(tolower(input$distribution)), winsorize = shiny::reactive(equalize(transform$data())))
  if(!is.null(custom.row.label)) {
    custom_label <- shiny::callModule(label, "labeller", data = custom.row.label, label = "Select row label", sep = label.sep, disable = shiny::reactive(!input$row.label))
  }

  result.data <- shiny::eventReactive(input$plot, {
    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Compute data")

    processed_data <- data.table::as.data.table(transform$data())

    processed_data <- data.table::data.table(data.r()[, 1], processed_data)

    progress$set(1)
    return(processed_data)
  })

  # disable downloadButton on init
  shinyjs::disable("download")

  plot <- shiny::eventReactive(input$plot, {
    # enable downloadButton
    shinyjs::enable("download")
    clearPlot(FALSE)

    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Compute plot")

    colors <- colorPicker()$palette

    if(!is.null(custom.row.label)) {
      label <- custom_label()$label
    } else {
      label <- NULL
    }

    plot <- create_heatmap(
      data = result.data(),
      unitlabel = input$label,
      row.label = input$row.label,
      row.custom.label = label,
      column.label = input$column.label,
      column.custom.label = make.unique(columns$label()),
      clustering = input$clustering,
      clustdist = input$cluster.distance,
      clustmethod = input$cluster.method,
      colors = colors,
      width = size()$width,
      height = size()$height,
      ppi = size()$ppi,
      scale = size()$scale,
      plot.method = plot.method,
      winsorize.colors = colorPicker()$winsorize
    )

    progress$set(1)

    return(plot)
  })

  #render choosen plotUI
  if(plot.method == "interactive"){
    output$heatmap <- shiny::renderUI({
      shinycssloaders::withSpinner(plotly::plotlyOutput(session$ns("interactive")), proxy.height = "800px")
    })

    output$interactive <- plotly::renderPlotly({
      if(clearPlot()) {
        return()
      } else {
        #new progress indicator
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(0.2, message = "Render plot")

        plot <- plot()$plot

        progress$set(1)

        return(plot)
      }
    })
  }else{
    output$heatmap <- shiny::renderUI({
      shinycssloaders::withSpinner(shiny::plotOutput(session$ns("static")), proxy.height = "800px")
    })

    output$static <- shiny::renderPlot(
      width = shiny::reactive(plot()$width * (plot()$ppi / 2.54)),
      height = shiny::reactive(plot()$height * (plot()$ppi / 2.54)),
      {
        if(clearPlot()) {
          return()
        } else {
          #new progress indicator
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(0.2, message = "Render plot")

          plot <- plot()$plot

          # handle error
          if(is(plot, "try-error")) {
            # TODO add logging
            stop("An error occured! Please try a different dataset.")
          }

          progress$set(1)
          return(ComplexHeatmap::draw(plot, heatmap_legend_side = "bottom"))
        }
    })
  }

  output$download <- shiny::downloadHandler(filename = "heatmap.zip",
                                            content = function(file) {
                                              download(file = file, filename = "heatmap.zip", plot = plot()$plot, width = plot()$width, height = plot()$height, ppi = plot()$ppi, ui = user_input())
                                            })


  user_input <- shiny::reactive({
    # format selection
    selection <- list(type = columns$type(), selectedColumns = columns$selectedColumns())

    # format clustering
    clustering <- list(
      clustering = input$clustering,
      distance = input$cluster.distance,
      method = input$cluster.method
    )

    # format options
    options <- list(
      transformation = list(method = transform$method(), applied = transform$transpose()),
      color = list(distribution = input$distribution, scheme = colorPicker()$name, reverse = colorPicker()$reverse, winsorize = colorPicker()$winsorize),
      unit_label = input$label,
      row_label = input$row.label,
      custom_row_label = custom_label()$selected,
      column_label = input$column.label
    )

    # merge all
    all <- list(selection = selection, clustering = clustering, options = options)
  })

  # enable/ disable plot button
  # show warning if disabled
  shiny::observe({
    shinyjs::disable("plot")
    show_warning <- TRUE

    # are columns selected?
    if(shiny::isTruthy(columns$selectedColumns())) {
      row.num <- nrow(shiny::isolate(data.r()))
      col.num <- length(columns$selectedColumns())

      # minimal heatmap possible (greater 1x1)?
      if(row.num > 1 || col.num > 1) {
        # no clustering for single rows or columns
        if(row.num == 1 && !is.element(input$clustering, c("both", "row"))) {
          show_warning <- FALSE
          shinyjs::enable("plot")
        } else if(col.num == 1 && !is.element(input$clustering, c("both", "column"))) {
          show_warning <- FALSE
          shinyjs::enable("plot")
        } else if(row.num > 1 && col.num > 1) { # no border case heatmaps
          show_warning <- FALSE
          shinyjs::enable("plot")
        }
      }

      if(show_warning) {
        shiny::showNotification(
          ui = "Warning! Insufficient columns/ rows. Either disable the respective clustering or expand the dataset.",
          id = "insuf_data",
          type = "warning"
        )
      } else {
        shiny::removeNotification("insuf_data")
      }

      # maximum heatmap reached?
      if(plot.method == "static" && row.num > static || plot.method == "interactive" && row.num > interactive) {
        shinyjs::disable("plot")
      }
    }
  })

  # automatic unitlabel
  shiny::observe({
    shiny::updateTextInput(session = session, inputId = "label", value = transform$method())
  })

  return(result.data)
}

#' heatmap module guide
#'
#' @param session The shiny session
#' @param custom.row.label Boolean. Show additional info. Default = FALSE.
#'
#' @return A shiny reactive that contains the texts for the Guide steps.
#'
heatmapGuide <- function(session, custom.row.label = FALSE) {
  steps <- list(
    "guide_selection" = "<h4>Data selection</h4>
      Select a column type for visualization, then select individual columns based on the chosen type.",
    "guide_cluster" = "<h4>Row/Column clustering</h4>
      Choose where the clustering is applied, then select a clustering distance and method.",
    "guide_transformation" = "<h4>Data transformation</h4>
      Pick a transformation that you want to apply to your data or leave it as 'None' if no transformation is needed.<br/>
      In case of the Z-score transformation, you can additionally choose to apply it to either rows or columns.",
    "guide_coloring" = "<h4>Color palettes</h4>
      Based on the selected data distribution, available color palettes are either sequential or diverging.<br/>
      The selected palette can additionally be reversed.<br/>
      Set the limits of the color palette with 'Winsorize to upper/lower'. Out of bounds values will be mapped to the nearest color.",
    "guide_options" = "<h4>Additional options</h4>
      You can set a label for the color legend that describes the underlying data unit. Furthermore, you can enable/disable row and column labels.",
    "guide_buttons" = "<h4>Create the plot</h4>
      As a final step click, a click on the 'Plot' button will render the plot, while a click on the 'Reset' button will reset the parameters to default."
  )

  if(custom.row.label) {
    steps[["guide_options"]] <- paste(steps[["guide_options"]], "Use the input to generate custom row-labels. The selected columns will be merged and used as label.")
  }

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
