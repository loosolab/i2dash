#' Prepare a heatmap to be rendered with the i2dash package.
#'
#' @param object A \code{\link[i2dash]{i2dashboard}} object.
#' @param countTable A matrix with features as rows and observations as columns. The rownames and columnnames should be provided and are used in building the heatmap.
#' @param group_by A vector with numerical values or a named list will be mapped to the y-axis. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of vectors x and y should be the same as well as the length of all vectors in case of a named list.
#' @param title (Optional) The title of the components chunk.
#' @param ... Further parameters which are compatible with wilsons create_heatmap() method. See \code{\link{create_heatmap}}.
#'
#' @return A string containing markdown code for the rendered textbox
#' @importFrom magrittr %<>%
#' @export
heatmap_to_i2dash <- function(object, countTable, group_by, title = NULL, ...) {
  if (!requireNamespace("i2dash", quietly = TRUE) ||
      !requireNamespace("stringi", quietly = TRUE) ||
      !requireNamespace("magrittr", quietly = TRUE)) {
    stop("Packages i2dash, stringi & magrittr are needed to use this function. Please install those.")
  }

  # Create env id
  id <- stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]")
  env_id <- paste0("env_", id)

  # Create list if element is not a list already
  if (!is.list(group_by)) group_by <- list(group_by)

  # Name the lists
  if (is.null(names(group_by))) group_by %<>% magrittr::set_names("grouping")

  # Validate input
  if ((!is.matrix(countTable) & !is.data.frame(countTable))) stop("'countTable' should be a class of 'matrix' or 'data.frame'.")

  # Check, if lengths in a list are the same and if x and y and label and color_by are the same length
  if (length(unique(sapply(group_by, length))) != 1) stop("The list 'group_by' should contain elements with the same length.")

  if (!identical(ncol(countTable), length(group_by[[1]])) & !is.null(expression)) stop("The number of columns in 'countTable' should be equal to the length of the vector 'group_by'.")

  additional_arguments <- list(...)
  if ("data" %in% names(additional_arguments)) warning("The parameters 'countTable' and 'group_by' will be used instead of 'data.table'")
  valid_arguments <- names(as.list(args(create_scatterplot)))
  invalid_args <- setdiff(names(additional_arguments), valid_arguments)
  if (length(invalid_args) != 0) stop(paste0(" The following parameter is not a valid parameter of 'Wilson::create_heatmap': ", invalid_args))

  # Create component environment
  env <- new.env()
  env$countTable <- countTable
  env$group_by_selection <- FALSE

  env$group_by <- group_by
  env$group_by_selection <- length(env$group_by) > 1

  env$additional_arguments <- additional_arguments

  # Save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "heatmap_wilson.Rmd", package = "wilson"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

#' Prepare a geneview to be rendered with the i2dash package.
#'
#' @param object A \code{\link[i2dash]{i2dashboard}} object.
#' @param countTable A matrix with features as rows and observations as columns. The rownames and columnnames should be provided and are used in buiding the heatmap.
#' @param group_by A vector with values or a named list will be mapped to the y-axis. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of vectors x and y should be the same as well as the length of all vectors in case of a named list.
#' @param title (Optional) The title of the components chunk.
#' @param ... Further parameters which are compatible with wilsons create_geneview() method. See \code{\link{create_geneview}}.
#'
#' @return A string containing markdown code for the rendered textbox
#' @importFrom magrittr %<>%
#' @export
geneview_to_i2dash <- function(object, countTable, group_by, title = NULL, ...) {
  if (!requireNamespace("i2dash", quietly = TRUE) ||
      !requireNamespace("stringi", quietly = TRUE) ||
      !requireNamespace("magrittr", quietly = TRUE)) {
    stop("Packages i2dash, stringi & magrittr are needed to use this function. Please install those.")
  }

  # Create env id
  id <- stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]")
  env_id <- paste0("env_", id)

  # Create list if element is not a list already
  if (!is.list(group_by)) group_by <- list(group_by)

  # Name the lists
  if (is.null(names(group_by))) group_by %<>% magrittr::set_names("grouping")

  # Validate input
  if (length(unique(sapply(group_by, length))) != 1) stop("The list 'group_by' should contain elements with the same length.")
  if (!identical(ncol(countTable), length(group_by[[1]])) & !is.null(expression)) stop("The number of columns in 'countTable' should be equal to the length of the vector 'group_by'.")

  additional_arguments <- list(...)
  if ("data" %in% names(additional_arguments)) warning("The parameter 'countTable' will be used instead of 'data'")
  if ("grouping" %in% names(additional_arguments)) warning("The parameter 'group_by' will be used instead of 'grouping'")
  valid_arguments <- names(as.list(args(create_scatterplot)))
  invalid_args <- setdiff(names(additional_arguments), valid_arguments)
  if (length(invalid_args) != 0) stop(paste0(" The following parameter is not a valid parameter of 'Wilson::create_scatterplot': ", invalid_args))

  # Create component environment
  env <- new.env()
  env$countTable <- countTable
  env$group_by_selection <- FALSE

  env$group_by <- group_by
  env$group_by_selection <- length(env$group_by) > 1

  env$additional_arguments <- additional_arguments

  # Save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "geneView_wilson.Rmd", package = "wilson"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

#' Prepare a scatterplot to be rendered with the i2dash package.
#'
#' @param object A \code{\link[i2dash]{i2dashboard}} object.
#' @param x A vector with numerical values or a named list will be mapped to the x-axis. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of vectors x and y should be the same as well as the length of all vectors in case of a named list.
#' @param y A vector with numerical values or a named list will be mapped to the y-axis. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of vectors x and y should be the same as well as the length of all vectors in case of a named list.
#' @param colour_by (Optional) A vector with factorial (= categorical coloring), numerical (= sequential colouring; can be forced to use categorical colouring by providing the parameter '"categorized" = TRUE') or character (= categorical colouring) values or a named list that will be used for colouring. In case of a named list, a dropdown menu will be provided in the interactive mode. Note: The length of the vector should be of the same length as x and y as well as the length of all vectors in case of a named list.
#' @param expression (Optional) A matrix or dataframe with the same length of columns as 'x'. The sequence and number of the columns should be equal to the sequence and length of 'x'. The rownames represent the feature i.e. gene names and the values represent the expression level. Note: This feature is not compatible with the statical mode (parameter '"interactive" = TRUE'). Alternatively you can provide a vector as colour_by.
#' @param title (Optional) The title of the components chunk.
#' @param ... Further parameters which are compatible with wilsons create_scatterplot() method. See \code{\link{create_scatterplot}}.
#'
#' @return A string containing markdown code for the rendered textbox
#' @importFrom magrittr %<>%
#' @export
scatterplot_to_i2dash <- function(object, x, y, colour_by = NULL, expression = NULL, title = NULL, ...) {
  if (!requireNamespace("i2dash", quietly = TRUE) ||
      !requireNamespace("stringi", quietly = TRUE) ||
      !requireNamespace("magrittr", quietly = TRUE)) {
    stop("Packages i2dash, stringi & magrittr are needed to use this function. Please install those.")
  }

  # Create env id
  id <- stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]")
  env_id <- paste0("env_", id)

  # Create list if element is not a list already
  if (!is.list(x)) x <- list(x)
  if (!is.list(y)) y <- list(y)
  if (!is.list(colour_by) & !is.null(colour_by)) colour_by <- list(colour_by)

  # Name the lists
  if (is.null(names(x))) x %<>% magrittr::set_names("x")
  if (is.null(names(y))) y %<>% magrittr::set_names("y")
  if (is.null(names(colour_by)) & !is.null(colour_by)) colour_by %<>% magrittr::set_names("colour")

  # Validate input
  if (!all(sapply(x, is.numeric))) stop("'x' should only contain numerical values.")
  if (!all(sapply(y, is.numeric))) stop("'y' should only contain numerical values.")
  if ((!is.matrix(expression) & !is.data.frame(expression)) & !is.null(expression)) stop("'expression' should be a class of 'matrix' or 'data.frame'.")

  # Check, if lengths in a list are the same and if x and y and label and color_by are the same length
  if (length(unique(sapply(x, length))) != 1) stop("The list 'x' should contain elements with the same length.")
  if (length(unique(sapply(y, length))) != 1) stop("The list 'y' should contain elements with the same length.")
  if (length(unique(sapply(colour_by, length))) != 1  & !is.null(colour_by)) stop("The list 'colour_by' should contain elements with the same length.")

  if (!identical(length(x[[1]]), length(y[[1]]))) stop("All arguments should be of the the same length.")
  if (!identical(length(x[[1]]), length(colour_by[[1]])) & !is.null(colour_by)) stop("All arguments should be of the the same length.")
  if (!identical(ncol(expression), length(x[[1]])) & !is.null(expression)) stop("The number of columns in 'expression' should be equal to the length of the vector 'x'.")

  additional_arguments <- list(...)
  if ("data" %in% names(additional_arguments)) warning("The parameters 'x' and 'y' will be used instead of 'data.table'")
  if ("plot.method" %in% names(additional_arguments)) warning("This parameter will be ignored and 'plot.method' = 'interactive' will be used.")
  valid_arguments <- names(as.list(args(create_scatterplot)))
  invalid_args <- setdiff(names(additional_arguments), valid_arguments)
  if (length(invalid_args) != 0) stop(paste0(" The following parameter is not a valid parameter of 'Wilson::create_scatterplot': ", invalid_args))

  # Create component environment
  env <- new.env()

  env$x_selection <- F
  env$y_selection <- F
  env$colour_by_selection <- F

  env$x <- x
  env$x_selection <- length(env$x) > 1

  env$y <- y
  env$y_selection <- length(env$y) > 1

  env$colour_by <- colour_by
  env$colour_by_selection <- length(env$colour_by) > 1

  env$expression <- expression

  env$additional_arguments <- additional_arguments

  # Save environment object
  saveRDS(env, file = file.path(object@workdir, "envs", paste0(env_id, ".rds")))

  # Expand component
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "scatterplot_wilson.Rmd", package = "wilson"), title = title, env_id = env_id, date = timestamp)
  return(expanded_component)
}
