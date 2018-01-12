#' Method for scatter plot creation
#'
#' @param data data.table containing plot data
#'             column 1: id
#'             column 2, 3(, 4): x, y(, z)
#' @param transparency Set point transparency. See \code{\link[ggplot2]{geom_point}}.
#' @param pointsize Set point size. See \code{\link[ggplot2]{geom_point}}.
#' @param labelsize Set label size. See \code{\link[ggplot2]{geom_text}}.
#' @param colors Vector of colors used for color palette
#' @param x_label Label x-Axis
#' @param y_label Label Y-Axis
#' @param z_label Label Z-Axis
#' @param density Boolean value, perform 2d density estimate.
#' @param line Boolean value, add reference line.
#' @param categorized Z-Axis (if exists) as categories.
#' @param highlight.data data.table containing data to highlight.
#' @param highlight.labels Vector of labels used for highlighted data.
#' @param highlight.color String with hexadecimal color-code.
#' @param xlim Numeric vector of two setting min and max limit of x-axis. See \code{\link[ggplot2]{lims}}.
#' @param ylim Numeric vector of two setting min and max limit of y-axis. See \code{\link[ggplot2]{lims}}.
#' @param colorbar.limits Vector with min, max values for colorbar (Default = NULL).
#' @param width Set plot width in cm (Default = "auto").
#' @param height Set plot height in cm (Default = "auto").
#' @param ppi Pixel per inch (default = 72).
#' @param plot.method Whether the plot should be 'interactive' or 'static' (Default = 'static').
#' @param scale Modify plot size while preserving aspect ratio (Default = 1).
#'
#' @details Width/ height limit = 500. If exceeded default to 500 and issue exceed_size = TRUE.
#'
#' @import data.table
#'
#' @return Returns list(plot = ggplotly/ ggplot, width, height, ppi, exceed_size).
create_scatterplot <- function(data, transparency = 1, pointsize = 1, labelsize = 3, colors = NULL, x_label = "", y_label = "", z_label = "", density = T, line = T, categorized = F, highlight.data = NULL, highlight.labels = NULL, highlight.color = "#FF0000", xlim = NULL, ylim = NULL, colorbar.limits = NULL, width = "auto", height = "auto", ppi = 72, plot.method = "static", scale = 1){
  ########## prepare data ##########
  #set labelnames if needed
  x_label <- ifelse(nchar(x_label), x_label, names(data)[2])
  y_label <- ifelse(nchar(y_label), y_label, names(data)[3])
  if(ncol(data) >= 4){ z_label <- ifelse(nchar(z_label), z_label, names(data)[4])}

  # make column names unqiue to prevent overwrite
  columnnames <- names(data)
  names(data) <- make.unique(columnnames)
  if(!is.null(highlight.data)) {
    columnnames.highlight <- names(highlight.data)
    names(highlight.data) <- make.unique(columnnames.highlight)
  }

  # get intern columnnames
  x_head <- names(data)[2]
  y_head <- names(data)[3]
  if(ncol(data) >= 4){ z_head <- names(data)[4]}

  #delete rows where both 0 or at least one NA
  rows.to.keep.data <- which(as.logical((data[, 2] != 0) + (data[, 3] != 0)))
  data <- data[rows.to.keep.data]
  if(!is.null(highlight.data)){
    rows.to.keep.high <- which(as.logical((highlight.data[, 2] != 0) + (highlight.data[, 3 != 0])))
    highlight.data <- highlight.data[rows.to.keep.high]
  }

  #delete labels accordingly
  if(is.null(highlight.data)){
    highlight.labels <- highlight.labels[rows.to.keep.data]
  }else{
    highlight.labels <- highlight.labels[rows.to.keep.high]
  }

  ########## assemble plot ##########
  theme1 <- ggplot2::theme (											#no gray background or helper lines
    plot.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = .3),
    axis.line.y = ggplot2::element_line(size = .3),
    axis.title.x = ggplot2::element_text(face = "bold", color = "black", size = 10 * scale),
    axis.title.y = ggplot2::element_text(face = "bold", color = "black", size = 10 * scale),
    plot.title = ggplot2::element_text(face = "bold", color = "black", size = 12 * scale),
    text = ggplot2::element_text(size = 10 * scale)
    #		legend.background = element_rect(color = "red")			#border color
    #		legend.key = element_rect("green")						#not working!
  )

  ###z-axis exists?
  if(ncol(data) >= 4){
    plot <- ggplot2::ggplot(data = data)

    ###scatter with color axis
    if(categorized == FALSE){
      plot <- plot +
        ###color_gradient
        ggplot2::scale_color_gradientn(colors = colors, name = z_label, limits = colorbar.limits, oob = scales::squish)

      ###scatter with categories
    }else if(categorized == TRUE){
      #change categorized column to factor
      data <- data[, (z_head) := as.factor(data[[z_head]])]

      ###categorized plot
      plot <- plot +

        ggplot2::scale_color_manual (
          #labels = data[,z_head],
          values = grDevices::colorRampPalette(colors)(length(unique(data[[z_head]]))), #get color for each value,
          #breaks = ,
          drop=FALSE,								#to avoid dropping empty factors
          name = z_label
          #			guide=guide_legend(title="sdsds" )					#legend for points
        )
    }
    #set names
    plot <- plot + ggplot2::aes_(x = as.name(x_head), y = as.name(y_head), color = as.name(z_head))
  }else{
    plot <- ggplot2::ggplot(data = data, ggplot2::aes_(x = as.name(x_head),y = as.name(y_head)))
  }

  if(density == TRUE){
    ### kernel density
    #plot$layers <- c(stat_density2d(geom="tile", aes(fill=..density..^0.25), n=200, contour=FALSE,) + aes_(fill=as.name(var)), plot$layers)#n=resolution; density less sparse
    plot <- plot + ggplot2::stat_density2d(geom="tile", ggplot2::aes(fill=..density..^0.25), n=200, contour=FALSE)

    plot <- plot + ggplot2::scale_fill_gradient(low="white", high="black") +
      #guides(fill=FALSE) +		#remove density legend

      ggplot2::labs(fill="Density")
  }

  if(line == TRUE){
    ### diagonal line
    plot <- plot + ggplot2::geom_abline(intercept=0, slope=1)
  }

  plot <- plot +
    ggplot2::xlab(x_label) +								#axis labels
    ggplot2::ylab(y_label)

  # interactive points with hovertexts
  if(plot.method == "interactive") {
    #set hovertext
    if(ncol(data) >=4){
      hovertext <- paste0("</br>", data[[1]],
                          "</br>", x_label, ": ", data[[x_head]],
                          "</br>", y_label, ": ", data[[y_head]],
                          "</br>", z_label, ": ", data[[z_head]])
    }else{
      hovertext <- paste0("</br>", data[[1]],
                          "</br>", x_label, ": ", data[[x_head]],
                          "</br>", y_label, ": ", data[[y_head]])
    }

    #set points
    plot <- plot + ggplot2::geom_point(size = pointsize * scale, alpha = transparency, ggplot2::aes(text = hovertext))

    if(!is.null(highlight.data)){
      #set highlighted hovertext
      if(ncol(data) >=4){
        hovertext.high <- paste0("</br>", highlight.data[[1]],
                                 "</br>", x_label, ": ", highlight.data[[x_head]],
                                 "</br>", y_label, ": ", highlight.data[[y_head]],
                                 "</br>", z_label, ": ", highlight.data[[z_head]])
      }else{
        hovertext.high <- paste0("</br>", highlight.data[[1]],
                                 "</br>", x_label, ": ", highlight.data[[x_head]],
                                 "</br>", y_label, ": ", highlight.data[[y_head]])
      }

      #set highlighted points
      plot <- plot + ggplot2::geom_point(size = pointsize * scale, alpha = transparency, inherit.aes = TRUE, data = highlight.data, color = highlight.color, show.legend = FALSE, ggplot2::aes(text = hovertext.high))
    }
  # static points without hovertexts
  } else if(plot.method == "static") {
    seed <- Sys.getpid() + Sys.time()
    # set points
    plot <- plot + ggplot2::geom_point(size = pointsize * scale, alpha = transparency)

    # set highlighted points
    if(!is.null(highlight.data)) {
      plot <- plot + ggplot2::geom_point(size = pointsize * scale, alpha = transparency, inherit.aes = TRUE, data = highlight.data, color = highlight.color, show.legend = FALSE)

      # set repelling point labels
      if(!is.null(highlight.labels)) {
        plot <- plot + ggrepel::geom_label_repel(data = highlight.data, mapping = ggplot2::aes(label = highlight.labels), size = labelsize * scale, color = "black", segment.color = "gray65", force = 2, max.iter = 10000, point.padding = grid::unit(0.1, "lines"), label.size = NA, alpha = 0.5, seed = seed)
        plot <- plot + ggrepel::geom_label_repel(data = highlight.data, mapping = ggplot2::aes(label = highlight.labels), size = labelsize * scale, color = "black", segment.color = "gray65", force = 2, max.iter = 10000, point.padding = grid::unit(0.1, "lines"), label.size = NA, fill = NA, seed = seed)
      }
    # set repelling labels (for only highlighted points shown)
    } else if(!is.null(highlight.labels) & length(highlight.labels) == nrow(data)) {
      plot <- plot + ggrepel::geom_label_repel(data = highlight.data, mapping = ggplot2::aes(label = highlight.labels), size = labelsize * scale, color = "black", segment.color = "gray65", force = 2, max.iter = 10000, point.padding = grid::unit(0.1, "lines"), label.size = NA, alpha = 0.5, seed = seed)
      plot <- plot + ggrepel::geom_label_repel(data = highlight.data, mapping = ggplot2::aes(label = highlight.labels), size = labelsize * scale, color = "black", segment.color = "gray65", force = 2, max.iter = 10000, point.padding = grid::unit(0.1, "lines"), label.size = NA, fill = NA, seed = seed)
    }
  }

  #set axis limits
  if(!is.null(xlim)){
    plot <- plot + ggplot2::xlim(xlim)
  }
  if(!is.null(ylim)){
    plot <- plot + ggplot2::ylim(ylim)
  }

  plot <- plot + theme1



  #estimate legend width
  legend.width <- 0
  legend.padding <- 20 # 10 on both sides
  legend.thickness <- 30
  if(density){
    legend.width <- nchar("Density")
  }
  if(ncol(data) > 3){
    legend.width <- ifelse(legend.width > nchar(z_label), legend.width, nchar(z_label))
  }
  if(density | ncol(data) > 3){
    #estimate tickwidth
    min.tick <- nchar(as.character(min(data[[3]], na.rm = TRUE))) * 8.75
    max.tick <- nchar(as.character(max(data[[3]], na.rm = TRUE))) * 8.75
    legend.thickness <- legend.thickness + ifelse(min.tick < max.tick, max.tick, min.tick)

    legend.width <- legend.width * 8.75
    legend.width <- ifelse(legend.width > legend.thickness, legend.width, legend.thickness) + legend.padding
  }

  #set width/ height
  if(width == "auto"){
    # cm to px
    width <- 28 * (ppi / 2.54) + legend.width
  } else {
    width <- width * (ppi / 2.54)
  }
  if(height == "auto"){
    # cm to px
    height <- 28 * (ppi / 2.54)
  } else {
    height <- height * (ppi / 2.54)
  }

  # apply scale factor
  width <- width * scale
  height <- height * scale

  # size exceeded?
  exceed_size <- FALSE
  limit <- 500 * (ppi / 2.54)
  if(width > limit) {
    exceed_size <- TRUE
    width <- limit
  }
  if(height > limit) {
    exceed_size <- TRUE
    height <- limit
  }

  if(plot.method == "interactive") {
    plot <- plotly::ggplotly(plot, width = width + legend.width, height = height, tooltip = "text")

    # add labels with arrows
    if(!is.null(highlight.labels)) {
      if(!is.null(highlight.data)) {
        plot <- plotly::add_annotations(p = plot, x = highlight.data[[x_head]], y = highlight.data[[y_head]], text = highlight.labels, standoff = pointsize * scale, font = list(size = labelsize * scale), bgcolor = 'rgba(255, 255, 255, 0.5)')
      } else if(nrow(data) == length(highlight.labels)) {
        plot <- plotly::add_annotations(p = plot, x = data[[x_head]], y = data[[y_head]], text = highlight.labels, standoff = pointsize * scale, font = list(size = labelsize * scale), bgcolor = 'rgba(255, 255, 255, 0.5)')
      }
    }
  }

  # pixel to cm
  width <- width / (ppi / 2.54)
  height <- height / (ppi / 2.54)


  return(list(plot = plot, width = width, height = height, ppi = ppi, exceed_size = exceed_size))
}

#' Method for pca creation.
#'
#' @param data data.table from which the plot is created (First column will be handled as rownames if not numeric).
#' @param dimensionA Number of dimension displayed on X-Axis.
#' @param dimensionB Number of dimension displayed on Y-Axis.
#' @param dimensions Number of dimesions to create.
#' @param on.columns Boolean perform pca on columns or rows.
#' @param labels Boolean show labels.
#' @param custom.labels Vector of custom labels. Will replace columnnames.
#' @param pointsize Size of the data points.
#' @param labelsize Size of texts inside plot (default = 3).
#' @param width Set the width of the plot in cm (default = 28).
#' @param height Set the height of the plot in cm (default = 28).
#' @param ppi Pixel per inch (default = 72).
#' @param scale Modify plot size while preserving aspect ratio (Default = 1).
#'
#' @details If width and height are the same axis ratio will be set to one (quadratic plot).
#' @details Width/ height limit = 500. If exceeded default to 500 and issue exceed_size = TRUE.
#'
#' @import data.table
#'
#' @return A named list(plot = ggplot object, data = pca.data, width = width of plot (cm), height = height of plot (cm), ppi = pixel per inch, exceed_size = Boolean whether width/ height exceeded max).
create_pca <- function(data, dimensionA = 1, dimensionB = 2, dimensions = 6, on.columns = TRUE, labels = FALSE, custom.labels = NULL, pointsize = 2, labelsize = 3, width = 28, height = 28, ppi = 72, scale = 1) {
  requireNamespace("FactoMineR", quietly = TRUE)
  requireNamespace("factoextra", quietly = TRUE)

  # prepare data ------------------------------------------------------------
  # set custom labels
  if(!is.null(custom.labels)) {
    if(!is.numeric(data[[1]])) {
      colnames(data)[-1] <- custom.labels
    } else {
      colnames(data) <- custom.labels
    }
  }

  #remove rows with NA
  data <- stats::na.omit(data)

  #check for rownames
  if(!is.numeric(data[[1]])){
    rownames <- data[[1]]
    data[, 1 := NULL]
  }else{
    rownames <- NULL
  }

  #transpose
  if(on.columns){
    data_t <- t(data)
    if(!is.null(rownames)){
      colnames(data_t) <- rownames
    }
  }else{
    data_t <- as.matrix(data)
    if(!is.null(rownames)){
      rownames(data_t) <- rownames
    }
  }

  #check if PCA possible
  if(ncol(data_t) < 3){
    stop(paste("PCA requires at least 3 elements. Found:", ncol(data_t)))
  }

  #remove constant rows (=genes with the same value for all samples)
  data_t <- data_t[, apply(data_t, 2, function(x) min(x, na.rm = TRUE) != max(x, na.rm = TRUE))]

  pca <- FactoMineR::PCA(data_t, scale.unit = TRUE, ncp = dimensions, graph = FALSE)

  # plot --------------------------------------------------------------------
  theme1 <- ggplot2::theme (								#no gray background or helper lines
    plot.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size=.3),
    axis.line.y = ggplot2::element_line(size=.3),
    axis.title.x = ggplot2::element_text(color="black", size = 11 * scale),
    axis.title.y = ggplot2::element_text(color="black", size = 11 * scale),
    #plot.title = element_text(color="black", size=12),
    plot.title = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    text = ggplot2::element_text(size = 12 * scale)						#size for all (legend?) labels
    #legend.key = element_rect(fill="white")
  )

  pca_plot <- factoextra::fviz_pca_ind(pca, axes = c(dimensionA, dimensionB), invisible = "none", pointsize = pointsize * scale, label = "none", axes.linetype = "blank", repel = FALSE)
  pca_plot <- pca_plot + theme1

  if(labels) {
    pca_plot <- pca_plot + ggrepel::geom_text_repel(
      data = data.frame(pca$ind$coord),
      mapping = ggplot2::aes_(x = pca$ind$coord[, dimensionA], y = pca$ind$coord[, dimensionB], label = rownames(pca$ind$coord)),
      segment.color = "gray65",
      size = labelsize * scale,
      force = 2,
      max.iter = 10000,
      point.padding = grid::unit(0.1, "lines")
    )
  }

  #ensure quadratic plot
  # if(width == height){
  #   pca_plot <- pca_plot + ggplot2::coord_fixed(ratio = 1)
  # }

  # add scale factor
  width <- width * scale
  height <- height * scale

  # size exceeded?
  exceed_size <- FALSE
  if(width > 500) {
    exceed_size <- TRUE
    width <- 500
  }
  if(height > 500) {
    exceed_size <- TRUE
    height <- 500
  }

  return(list(plot = pca_plot, data = pca, width = width, height = height, ppi = ppi, exceed_size = exceed_size))
}

#' Method for heatmap creation
#'
#' @param data data.table containing plot data. First column contains row labels.
#' @param unitlabel label of the colorbar
#' @param row.label Logical whether or not to show row labels.
#' @param row.custom.label Vector of custom row labels.
#' @param column.label Logical whether or not to show column labels.
#' @param column.custom.label Vector of custom column labels.
#' @param clustering How to apply clustering on data. c("none", "both", "column", "row")
#' @param clustdist Which cluster distance to use. See \code{\link[heatmaply]{heatmapr}}.
#' @param clustmethod Which cluster method to use. See \code{\link[heatmaply]{heatmapr}}.
#' @param colors Vector of colors used for color palette.
#' @param winsorize.colors NULL or a vector of length two, giving the values of colorbar ends (default = NULL).
#' @param plot.method Choose which method is used for plotting. Either "plotly" or "complexHeatmap" (Default = "complexHeatmap").
#' @param width Set width of plot in cm (Default = "auto").
#' @param height Set height of plot in cm (Default = "auto").
#' @param ppi Pixel per inch (default = 72).
#' @param scale Modify plot size while preserving aspect ratio (Default = 1).
#'
#' @details Width/ height limit = 500. If exceeded default to 500 and issue exceed_size = TRUE.
#'
#' @return Returns list(plot = complexHeatmap/ plotly object, width = width in cm, height = height in cm, ppi = pixel per inch, exceed_size = Boolean whether width/ height exceeded max) depending on plot.method.
create_heatmap <- function(data, unitlabel='auto', row.label=T, row.custom.label = NULL, column.label=T, column.custom.label = NULL, clustering='none', clustdist='auto', clustmethod='auto', colors=NULL, winsorize.colors = NULL, plot.method = "static", width = "auto", height = "auto", ppi = 72, scale = 1) {
  requireNamespace("heatmaply", quietly = TRUE)
  requireNamespace("ComplexHeatmap", quietly = TRUE)
  requireNamespace("grDevices", quietly = TRUE)
  requireNamespace("circlize", quietly = TRUE)

  #row label
  if(!is.null(row.custom.label)) {
    row.label.strings <- row.custom.label
  } else {
    row.label.strings <- data[[1]]
  }

  # column label
  if(!is.null(column.custom.label)) {
    column.label.strings <- column.custom.label
  } else {
    column.label.strings <- names(data)[-1]
  }

  # cm to pixel
  if(is.numeric(width)) {
    width <- width * (ppi / 2.54)
  }
  if(is.numeric(height)) {
    height <- height * (ppi / 2.54)
  }

  # plot --------------------------------------------------------------------
  if(plot.method == "interactive"){
    #estimate label sizes
    #row label
    rowlabel_size <- ifelse(row.label, max(nchar(data[[1]]), na.rm = TRUE) * 8 * scale, 0)
    #column label
    collabel_size <- ifelse(column.label, (2 + log2(max(nchar(names(data)), na.rm = TRUE))^2) * 10, 0)
    #legend
    legend <- nchar(unitlabel) * 10
    legend <- ifelse(legend < 90, 90, legend)
    #plot size
    #auto_width <- 20 * (ncol(data) - 1) + rowlabel_size + legend
    auto_height <- 10 * nrow(data) + collabel_size

    #data
    plot <- heatmaply::heatmapr(data[, -1],
                                labRow = row.label.strings,
                                labCol = column.label.strings,
                                hclust_method = clustmethod,
                                dist_method = clustdist,
                                dendrogram = clustering,
                                distfun = factoextra::get_dist
                                #width = width, #not working
                                #height = height
    )

    #layout
    plot <- heatmaply::heatmaply(plot,
                                 plot_method = "ggplot",
                                 scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(colors = colors, name = unitlabel, limits = winsorize.colors, oob = scales::squish),
                                 heatmap_layers = ggplot2::theme(text = ggplot2::element_text(size = 12 * scale))
    )

    # scale axis ticks
    ticks <- list(tickfont = list(size = 12 * scale))

    plot <- plotly::layout(plot, autosize = ifelse(width == "auto", TRUE, FALSE), margin = list(l = rowlabel_size, r = legend, b = collabel_size), xaxis = ticks, yaxis2 = ticks)

    # decide which sizes should be used
    if(width == "auto") {
      width <- 0
    # } else if(width <= auto_width) {
    #   width <- auto_width
    }
    if(height == "auto") {
      height <- auto_height
    }

    # add scale
    width <- width * scale
    height <- height * scale

    # size exceeded?
    exceed_size <- FALSE
    limit <- 500 * (ppi / 2.54)
    if(width > limit) {
      exceed_size <- TRUE
      width <- limit
    }
    if(height > limit) {
      exceed_size <- TRUE
      height <- limit
    }

    plot$x$layout$width <- width
    plot$x$layout$height <- height

    #address correct axis
    if(clustering == "both" || clustering == "column"){
      plot <- plotly::layout(plot, xaxis = list(showticklabels = column.label),
                             yaxis2 = list(showticklabels = row.label)
      )
    }else if(clustering == "row" || clustering == "none"){
      plot <- plotly::layout(plot, xaxis = list(showticklabels = column.label),
                             yaxis = list(showticklabels = row.label)
      )
    }

    #don't show dendrogram ticks
    if(clustering == "row"){
      plot <- plotly::layout(plot, xaxis2 = list(showticklabels = FALSE)
      )
    }else if(clustering == "column"){
      plot <- plotly::layout(plot, yaxis = list(showticklabels = FALSE)
      )
    }

    # pixel to cm
    width <- width / (ppi / 2.54)
    height <- height / (ppi / 2.54)

    plot <- list(plot = plot, width = width, height = height, ppi = ppi, exceed_size = exceed_size)
  }else if(plot.method == "static"){

    #clustering
    if (clustering=='none') {
      cluster_rows=F
      cluster_columns=F
    } else if (clustering=='row') {
      cluster_rows=T
      cluster_columns=F
    } else if (clustering=='column') {
      cluster_rows=F
      cluster_columns=T
    } else if (clustering=='both') {
      cluster_rows=T
      cluster_columns=T
    }

    #
    # Create new colour brakepoints in case of winsorizing
    #
    if(!is.null(winsorize.colors)) {
      breaks <- seq(winsorize.colors[1], winsorize.colors[2], length = length(colors))
      colors <- circlize::colorRamp2(breaks, colors)
    }

    #convert data to data.frame so rownames can be used for annotation
    prep.data <- as.data.frame(data[, -1])

    row.names(prep.data) <- row.label.strings
    colnames(prep.data) <- column.label.strings

    plot <- ComplexHeatmap::Heatmap(
      prep.data,
      name = unitlabel,
      col = colors,
      cluster_rows = cluster_rows,
      cluster_columns = cluster_columns,
      clustering_distance_rows = clustdist,
      clustering_distance_columns = clustdist,
      clustering_method_rows = clustmethod,
      clustering_method_columns = clustmethod,
      show_row_names = row.label,
      show_column_names = column.label,
      row_names_side = "left",
      row_dend_side = "right",
      row_dend_width = grid::unit(1 * scale, "inches"),
      # row_dend_gp = grid::gpar(lwd = 1, lex = scale), # don't seem to work
      column_dend_height = grid::unit(1 * scale, "inches"),
      # column_dend_gp = grid::gpar(lwd = 1, lex = scale), # don't seem to work
      row_names_max_width = grid::unit(8 * scale, "inches"),
      column_names_max_height = grid::unit(4 * scale, "inches"),
      row_names_gp = grid::gpar(fontsize = 12 * scale),
      column_names_gp = grid::gpar(fontsize = 12 * scale),
      column_title_gp = grid::gpar(fontsize = 10 * scale, units = "in"),
      heatmap_legend_param = list(
        color_bar = "continuous",
        legend_direction = "horizontal",
        title_gp = grid::gpar(fontsize = 10 * scale),
        labels_gp = grid::gpar(fontsize = 8 * scale),
        grid_height = grid::unit(0.15 * scale, "inches")
      )
    )

    #width/ height calculation
    col_names_maxlength_label_width=max(sapply(colnames(prep.data), graphics::strwidth, units="in", font = 12))	#longest column label when plotted in inches
    col_names_maxlength_label_height=max(sapply(colnames(prep.data), graphics::strheight, units="in", font = 12))	#highest column label when plotted in inches
    row_names_maxlength_label_width=max(sapply(rownames(prep.data), graphics::strwidth, units="in", font = 12))	#longest row label when plotted in inches
    row_names_maxlength_label_height=max(sapply(rownames(prep.data), graphics::strheight, units="in", font = 12))	#highest row label when plotted in inches

    # width
    if(row.label){
      auto_width <- row_names_maxlength_label_width + 0.3 #width buffer: labels + small whitespaces
    }else{
      auto_width <- 0.3 #no labels
    }

    if(cluster_rows) auto_width <- auto_width + 1 #width buffer: dendrogram + small whitespaces between viewports

    auto_width <- ncol(prep.data) * (col_names_maxlength_label_height + 0.08) + auto_width #readable rowlabels
    # inch to px
    auto_width <- auto_width * ppi

    # height
    auto_height <- 0.2 + 0.5 + (5 * row_names_maxlength_label_height) #height buffer: small whitespaces + color legend + 2 title rows(+whitespace)

    if(column.label) auto_height <- auto_height + col_names_maxlength_label_width
    if(cluster_columns) auto_height <- auto_height + 1

    auto_height <- auto_height + nrow(prep.data) * (row_names_maxlength_label_height + 0.06)
    # inch to px
    auto_height <- auto_height * ppi

    # use auto sizes
    if(height == "auto") {
      height <- auto_height
    }
    if(width == "auto") {
      width <- auto_width
    }

    # pixel to cm
    width <- width / (ppi / 2.54)
    height <- height / (ppi / 2.54)

    # size exceeded?
    exceed_size <- FALSE
    if(width > 500) {
      exceed_size <- TRUE
      width <- 500
    }
    if(height > 500) {
      exceed_size <- TRUE
      height <- 500
    }

    plot <- list(plot = plot, width = width * scale, height = height * scale, ppi = ppi, exceed_size = exceed_size)
  }

  return(plot)
}

#' Method for geneView creation
#'
#' @param data data.table containing plot data
#' @param grouping data.table metadata containing:
#'                                                column1 : key
#'                                                column2 : factor1
#' @param plot.type String specifing which plot type is used c("box", "line", "violin", "bar").
#' @param facet.target Target to plot on x-Axis c("gene", "condition").
#' @param facet.cols Number of plots per row.
#' @param colors Vector of colors used for color palette
#' @param ylabel Label of the y-axis (default = NULL).
#' @param ylimits Vector defining scale of y-axis (default = NULL).
#' @param gene.label Vector of labels used instead of gene names (default = NULL).
#' @param plot.method Choose which method used for plotting. Either "static" or "interactive" (Default = "static").
#' @param width Set the width of the plot in cm (default = "auto").
#' @param height Set the height of the plot in cm (default = "auto").
#' @param ppi Pixel per inch (default = 72).
#' @param scale Modify plot size while preserving aspect ratio (Default = 1).
#'
#' @details Width/ height limit = 500. If exceeded default to 500 and issue exceed_size = TRUE.
#'
#' @import data.table
#'
#' @return Returns depending on plot.method list(plot = ggplot/ plotly object, width = width in cm, height = height in cm, ppi = pixel per inch, exceed_size = Boolean).
create_geneview <- function(data, grouping, plot.type = "line", facet.target = "gene", facet.cols = 2, colors = NULL, ylabel = NULL, ylimits = NULL, gene.label = NULL, plot.method = "static", width = "auto", height = "auto", ppi = 72, scale = 1){
  #grouping
  #group by factor if existing (fill with key if empty)
  grouping[grouping[[2]] == "", 2 := grouping[grouping[[2]] == "", 1]]

  genes <- nrow(data)												#number of genes (rows in matrix)
  conditions <- length(unique(grouping[[2]]))											#number of conditions (columns in matrix)

  ###################
  # Combine and transform dataframes
  ###################
  #detach ids from data/ replace with gene.label
  if(is.null(gene.label)) {
    data_id <- data[[1]]
  } else {
    data_id <- gene.label
  }
  data <- data[, sapply(data, is.numeric), with = FALSE]

  data_cols <- names(data)
  data <- data.table::transpose(data) 								#switch columns <> rows

  #place former colnames in cols
  data$cols <- data_cols
  data.table::setcolorder(data, c("cols", colnames(data)[1:ncol(data)-1]))
  #reattach ids as colnames
  names(data)[2:ncol(data)] <- data_id

  names(grouping)[1:2] <- c("cols", "condition") #add header for condition
  data <- data[grouping, on = c(names(grouping)[1])]					#merge dataframes by rownames
  names(data)[1] <- "sample"							#change Row.names to sample
  data[, sample := NULL]								#completely remove sample column again
  #order conditions in plot according to grouping (instead of alphabetic)
  data[, condition := factor(condition, levels = unique(condition))]

  data <- data.table::melt(data, id.vars = "condition")

  ###################
  # Choose color palette
  ###################
  if (facet.target == "gene") {											#facet = gene
    num_colors <- conditions
  }
  if (facet.target == "condition") {										#facet = condition
    num_colors <- genes
  }


  if (is.null(colors)) {
    color_fill_grayscale="grey75"										#color to use for filling geoms in grayscale mode
    colors <- rep(color_fill_grayscale,num_colors)
  }else{
    colors <- grDevices::colorRampPalette(colors)(num_colors)
  }

  ###################
  # Function to get standard error for error bars (box, bar, violin)
  ###################
  get.se <- function(y){
    se <- stats::sd(y) / sqrt(length(y))
    mu <- mean(y)
    data.frame(ymin = mu-se, y = y, ymax = mu+se)
  }

  ###################
  # Function to collapse the dataframe to the mean and the standard deviation/error before plotting (ONLY used for line plot)
  ###################

  # data : a data frame
  # varname : the name of a column containing the variable to be summarized
  # groupnames : vector of column names to be used as grouping variables
  data_summary <- function(data, varname, groupnames){
    summary_func <- function(x, col){
      c(
        mean = mean(x[[col]], na.rm=TRUE),
        sd = stats::sd(x[[col]], na.rm=TRUE),
        se = stats::sd(x[[col]], na.rm=TRUE) / sqrt(length(x[[col]]))
      )
    }
    data_sum <- plyr::ddply(data, groupnames, .fun=summary_func, varname)
    data_sum <- reshape::rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  if (plot.type == "line") {
    data = data_summary(data, varname = "value", groupnames = c("condition", "variable"))			#collapse the dataframe to the mean and the standard deviation for line plot
  }

  if (plot.type == "box" || plot.type == "violin" || plot.type == "bar" || plot.type == "line") {
    ###################
    # Set common parameters for all plots
    ###################

    # plot --------------------------------------------------------------------

    theme1 <- ggplot2::theme(															#no gray background or helper lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 1),										#x-axis sample lables = 90 degrees
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black"),
      legend.position = "none",														#remove legend
      legend.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "mono", size = 15 * scale)

      #axis.line.x = element_line(size = .3),
      #axis.line.y = element_line(size = .3),
      #panel.background = element_blank(),
      #axis.title.y = element_text(face = "bold", color = "black", size = 10),
      #plot.title = element_text(face = "bold", color = "black", size = 12),
      #axis.text.x = element_text(angle = 90, hjust = 1)											#x-axis sample lables = vertical
    )

    matrixplot <- ggplot2::ggplot(data, ggplot2::aes(y = value))

    matrixplot <- matrixplot +
      ggplot2::theme_bw() + theme1 +
      ggplot2::ylab(ylabel) +
      ggplot2::xlab("") +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_color_manual(values = colors)

    ###################
    # Handle facetting and special parameters for line plot (no facetting, etc.)
    ###################

    if (facet.target == "gene") {														#facet = gene
      matrixplot = matrixplot + ggplot2::aes(x = condition, fill = condition)

      if (plot.type == "line") {													#line plot: no facetting, different size algorithm
        matrixplot <- matrixplot + ggplot2::aes(x = variable, colour = condition, group = condition, fill = NULL)
        matrixplot <- matrixplot + ggplot2::scale_x_discrete(expand = c(0.05, 0.05))								#expand to reduce the whitespace inside the plot (left/right)
      } else {
        #compute number of rows to get facet.cols columns (works better with plotly)
        rows <- ceiling(length(levels(data$variable)) / facet.cols)

        matrixplot <- matrixplot + ggplot2::facet_wrap(~variable, nrow = rows, scales = "free_x")
      }
    }
    if (facet.target == "condition") {													#facet = condition
      matrixplot <- matrixplot + ggplot2::aes(x = variable, fill = variable)

      if (plot.type == "line") {													#line plot: no facetting, different size algorithm
        matrixplot <- matrixplot + ggplot2::aes(x = condition, colour = variable, group = variable, fill = NULL)
        matrixplot <- matrixplot + ggplot2::scale_x_discrete(expand = c(0.05,0.05))								#expand to reduce the whitespace inside the plot (left/right)
      } else {
        #compute number of rows to get facet.cols columns (works better with plotly)
        rows <- ceiling(length(levels(data$condition)) / facet.cols)

        matrixplot <- matrixplot + ggplot2::facet_wrap(~condition, nrow = rows, scales = "free_x")
      }
    }

    ###################
    # Further handle plot types
    ###################

    if (plot.type == "box") {																#plot type: box
      matrixplot <- matrixplot + ggplot2::geom_boxplot(position = ggplot2::position_dodge(1))
      matrixplot <- matrixplot + ggplot2::stat_boxplot(geom = 'errorbar', size = 0.2, width = 0.5) 										#add horizontal line for errorbar
      #matrixplot <- matrixplot + stat_summary(fun.data = get.se, geom = "errorbar", width = 0.2)					#error bar of standard error
    }
    if (plot.type == "violin") {																#plot type: violin
      matrixplot <- matrixplot + ggplot2::geom_violin()
      #matrixplot <- matrixplot + stat_summary(fun.y = "median", geom = "point")										#add median dot
      #matrixplot <- matrixplot + stat_summary(fun.data = get.se, geom = "errorbar", width = 0.2, position = position_dodge())					#error bar of standard error
    }
    if (plot.type == "bar") {																#plot type: box
      matrixplot <- matrixplot + ggplot2::stat_summary(fun.y = mean, geom = "bar", position = "dodge")							#bar plot of the mean (color=condition)
      matrixplot <- matrixplot + ggplot2::stat_summary(fun.data = get.se, geom = "errorbar", size = 0.2, width = 0.2, position = ggplot2::position_dodge())					#error bar of standard error
    }
    if (plot.type == "line") {
      matrixplot <- matrixplot + ggplot2::theme(legend.position = "right")
      #matrixplot <- matrixplot + geom_errorbar(aes(ymin = value-sd, ymax = value + sd), width = 0.05)								#error bar = standard deviation
      matrixplot <- matrixplot + ggplot2::geom_errorbar(ggplot2::aes(ymin = value - se, ymax = value + se), size = 0.2, width = 0.05)								#error bar = standard error
      matrixplot <- matrixplot + ggplot2::geom_line() + ggplot2::geom_point()											#bar plot of the mean (color=condition)
      #set hovertext
      matrixplot <- matrixplot + ggplot2::aes(text = paste("ID: ", data$variable, "\n",
                                                           "Condition: ", data$condition, "\n",
                                                           "Value: ", data$value
      ))
    }

    # set y-axis ticks
    y_ticks <- pretty(data[["value"]])
    if(length(data[["value"]]) != 1) {
      if(!is.null(ylimits)) {
        y_ticks <- pretty(ylimits)
      }

      matrixplot <- matrixplot + ggplot2::scale_y_continuous(breaks = y_ticks, limits = ylimits)
    } else {
      # change yaxis limits
      if(!is.null(ylimits)) {
        matrixplot <- matrixplot + ggplot2::ylim(ylimits)
      }
    }
  }

  #get names of columns / rows
  if(plot.type == "line"){
    if(facet.target == "gene"){
      column.names <- data[["variable"]]
      legend.names <- data[["condition"]]
    }else{
      column.names <- data[["condition"]]
      legend.names <- data[["variable"]]
    }
  }else{
    if(facet.target == "condition"){
      column.names <- data[["variable"]]
      title.names <- data[["condition"]]
    }else{
      column.names <- data[["condition"]]
      title.names <- data[["variable"]]
    }
  }

  #dynamic plot in inches

  #calculate cex for better strwidth calculation
  ccex <- function(x){
    2.3 - (x * log(1 + 1/x))
  }

  ###width estimation
  yaxis_label_height <- graphics::strheight(ylabel, units = "inches")
  if(length(data[["value"]]) == 1 && floor(data[["value"]]) == data[["value"]]) {
    # adds three characters '.05'; account for single integer value plots
    value <- data[["value"]] + 0.05
  } else {
    value <- y_ticks
  }
  yaxis_tick_width <- max(graphics::strwidth(value, units = "inches"), na.rm = TRUE)
  xaxis_tick_height <- max(graphics::strheight(column.names, units = "inches", cex = 2), na.rm = TRUE) * length(levels(column.names))
  ###height estimation
  xaxis_tick_width <- max(graphics::strwidth(column.names, units = "inches", cex = ccex(max(nchar(levels(column.names))))), na.rm = TRUE)

  if(plot.type == "line"){
    ###width estimation
    max_chars <- max(nchar(levels(legend.names)), na.rm = TRUE)
    legend_width <- max(graphics::strwidth(legend.names, units = "inches", cex = ccex(max_chars)), na.rm = TRUE)
    legend_columns <- 1 + (length(levels(legend.names))-1) %/% 20

    auto_width <- 0.25 + yaxis_label_height + yaxis_tick_width + xaxis_tick_height + ((legend_width + 0.5) * legend_columns)

    ###height estimation
    plot_height <- 4

    #top margin to prevent legend cut off
    top <- 0
    if(plot.method == "static"){
      margin.multiplier <- ceiling(length(levels(legend.names)) / legend_columns)
      margin.multiplier <- ifelse(margin.multiplier < 17, 0, margin.multiplier - 17)

      top <- 0.1 * margin.multiplier
      matrixplot <- matrixplot + ggplot2::theme(plot.margin = grid::unit(c(top + 0.1, 0, 0, 0), "inches"))
    }

    auto_height <- plot_height + xaxis_tick_width + top
  }else{
    ###width estimation
    max_chars <- max(nchar(levels(title.names)), na.rm = TRUE)

    title_width <- max(graphics::strwidth(title.names, units = "inches", cex = ccex(max_chars)), na.rm = TRUE)
    # prevent cut off for small titles
    title.chars <- sum(nchar(levels(title.names)))
    if(facet.cols == 1 && max(nchar(levels(title.names))) <= 20) {
        title_width <- title_width + (-log10(max(nchar(levels(title.names)))) + 1.6) / 3
    } else if(title.chars <= 20) {
      title_width <- title_width + (-log10(title.chars) + 1.4) / 3
    }
    #TODO margin between plots (not really needed)
    plots_per_row <- ceiling(length(levels(title.names))/ rows)

    auto_width <- yaxis_label_height + yaxis_tick_width + (ifelse(title_width > xaxis_tick_height, title_width, xaxis_tick_height) * plots_per_row)

    ###height estimation
    title_height <- max(graphics::strheight(title.names, units = "inches", cex = 2), na.rm = TRUE)
    plot_height <- 2


    auto_height <- (title_height + plot_height + xaxis_tick_width) * rows
  }

  # size inch -> cm
  auto_width <- auto_width * 2.54
  auto_height <- auto_height * 2.54

  # use greater/ automatic sizes
  if(width == "auto") {
    width <- auto_width
  }
  if(height == "auto") {
    height <- auto_height
  }

  # add scaleing factor
  width <- width * scale
  height <- height * scale

  # size exceeded?
  exceed_size <- FALSE
  if(width > 500) {
    exceed_size <- TRUE
    width <- 500
  }
  if(height > 500) {
    exceed_size <- TRUE
    height <- 500
  }

  # plotly ------------------------------------------------------------------
  if(plot.method == "interactive"){
    matrixplotly <- plotly::ggplotly(
      tooltip = "text",
      matrixplot,
      width = width * (ppi / 2.54),
      height = height * (ppi / 2.54)
    )

    plotly::layout(matrixplotly, autosize = FALSE)

    return(list(plot = matrixplotly, width = width, height = height, ppi = ppi, exceed_size = exceed_size))
  }else{
    return(list(plot = matrixplot, width = width, height = height, ppi = ppi, exceed_size = exceed_size))
  }
}

#' Method to get equalized min/max values from vector
#'
#' @param values Numeric vector or table
#'
#' @return Vector with c(min, max).
equalize <- function(values){
  if(is.vector(values)){
    min <- abs(min(values, na.rm = TRUE))
    max <- abs(max(values, na.rm = TRUE))
  }else{
    min <- abs(min(apply(values, 2, function(x) {min(x, na.rm = TRUE)})))
    max <- abs(max(apply(values, 2, function(x) {max(x, na.rm = TRUE)})))
  }

  if(min > max){
    result <- min
  }else{
    result <- max
  }

  return(c(-1 * result, result))
}

#' Function to search data for selection
#'
#' @param input Vector length one (single) or two (ranged) containing numeric values for selection.
#' @param choices Vector on which input values are applied.
#' @param options Vector on how the input and choices should be compared. It can contain: single = c("=", "<", ">") or ranged = c("inner", "outer").
#' @param min. Minimum value that can be selected on slider (defaults to min(choices)).
#' @param max. Maximum value that can be selected on slider (defaults to max(choices)).
#'
#' @return Returns a logical vector with the length of choices, where every matched position is TRUE.
searchData <- function(input, choices, options = c("=", "<", ">"), min. = min(choices, na.rm = TRUE), max. = max(choices, na.rm = TRUE)) {
  #don't apply if no options selected
  if(is.null(options)){
    return(rep(TRUE, length(choices)))
  }

  if(length(input) > 1){
    #don't compare if everything is selected
    if(options == "inner" & input[1] == min. & input[2] == max.){
      return(rep(TRUE, length(choices)))
    }

    selection <- vapply(choices, FUN.VALUE = logical(1), function(x) {
      # NA & NaN == FALSE
      if(is.na(x) | is.nan(x)){
        return(FALSE)
      }

      #range
      if("inner" == options){
        if(x >= input[1] & x <= input[2]) return(TRUE)
      }
      if("outer" == options){
        if(x < input[1] | x > input[2]) return(TRUE)
      }

      return(FALSE)
    })
  }else{
    selection <- vapply(choices, FUN.VALUE = logical(1), function(x) {
      # NA & NaN == FALSE
      if(is.na(x) | is.nan(x)){
        return(FALSE)
      }

      #single point
      if(any("=" == options)){
        if(x == input) return(TRUE)
      }
      if(any("<" == options)){
        if(x < input) return(TRUE)
      }
      if(any(">" == options)){
        if(x > input) return(TRUE)
      }

      return(FALSE)
    })
  }

  return(selection)
}

#' Function used for downloading.
#' Creates a zip container containing plot in png, pdf and user input in json format.
#' Use inside \code{\link[shiny]{downloadHandler}} content function.
#'
#' @param file See \code{\link[shiny]{downloadHandler}} content parameter.
#' @param filename See \code{\link[shiny]{downloadHandler}}.
#' @param plot Plot to download.
#' @param width in centimeter.
#' @param height in centimeter.
#' @param ppi pixel per inch. Defaults to 72.
#' @param ui List of user inputs. Will be converted to Javascript Object Notation. See \code{\link[RJSONIO]{toJSON}}
#'
#' @return See \code{\link[utils]{zip}}.
download <- function(file, filename, plot, width, height, ppi = 72, ui = NULL) {
  # cut off file extension
  name <- sub("(.*)\\..*$", replacement = "\\1", filename)

  # create tempfile names
  plot_file_pdf <- tempfile(pattern = name, fileext = ".pdf")
  plot_file_png <- tempfile(pattern = name, fileext = ".png")
  if(!is.null(ui)) {
    selection_file <- tempfile(pattern = "selection", fileext = ".json")
  } else {
    selection_file <- NULL
  }

  # save plots depending on given plot object
  if(ggplot2::is.ggplot(plot)) {
    # ggplot

    ggplot2::ggsave(plot_file_pdf, plot = plot, width = width, height = height, units = "cm", device = "pdf", useDingbats = FALSE)
    ggplot2::ggsave(plot_file_png, plot = plot, width = width, height = height, units = "cm", device = "png", dpi = ppi)
  } else if(class(plot)[1] == "plotly") {
    # plotly
    # change working directory temporary so mounted drives are not a problem
    wd <- getwd()
    setwd(tempdir())
    plotly::export(p = plot, file = plot_file_pdf)
    plotly::export(p = plot, file = plot_file_png)
    setwd(wd)
  } else if(class(plot) == "Heatmap") { # TODO: find better way to check for complexHeatmap object
    # complexHeatmap
    grDevices::pdf(plot_file_pdf, width = width / 2.54, height = height / 2.54, useDingbats = FALSE) # cm to inch
    ComplexHeatmap::draw(plot, heatmap_legend_side = "bottom")
    grDevices::dev.off()
    grDevices::png(plot_file_png, width = width, height = height, units = "cm", res = ppi)
    ComplexHeatmap::draw(plot, heatmap_legend_side = "bottom")
    grDevices::dev.off()
  }

  # vector with files to zip
  files <- c(plot_file_pdf, plot_file_png)

  # save user input
  if(!is.null(selection_file)) {
    # make key = value pair using value of name variable
    ui_list <- list()
    ui_list[[name]] <- ui

    json <- RJSONIO::toJSON(ui_list, pretty = TRUE)
    write(json, file = selection_file)

    files <- c(files, selection_file)
  }

  # create zip file
  utils::zip(zipfile = file, files = files, flags = "-j") # discard file path
}
