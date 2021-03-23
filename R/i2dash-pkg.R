#' 'i2dash': A package for programmatic creation of interactive, web-based dashboards
#'
#' 'i2dash' relies on the widely used R packages 'flexdashboard', 'knitr' and 'rmarkdown'. 'i2dash' introduces a new class from R's S4 object system named \linkS4class{i2dashboard}, which by design provides the main functionality of the package. Besides global properties such as the dashboard title, author and theme, an instance of the \linkS4class{i2dashboard} class also stores individual dashboard pages and the navigation menu, as well as all components that make up the content of individual pages.
#'
#' @author Arsenij Ustjanzew \email{arsenij.ustjanzew@@gmail.com}
#' @author Jens Preussner \email{Jens.Preussner@@mpi-bn.mpg.de}
#' @author Mario Looso \email{Mario.Looso@@mpi-bn.mpg.de}
#'
#' @section Citation:
#'
#' When using the package in your work, please cite:
#' i2dash: Creation of Flexible, Interactive and Web-based Dashboards for Visualization of Omics-pipeline Results
#' Arsenij Ustjanzew, Jens Preussner, Mette Bentsen, Carsten Kuenne, Mario Looso
#' bioRxiv 2020.07.06.189563; doi: https://doi.org/10.1101/2020.07.06.189563.
#'
#' @importFrom assertive.sets is_subset
#' @importFrom assertive.types assert_is_data.frame is_character
#' @importFrom flexdashboard flex_dashboard
#' @importFrom rmarkdown render run
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringi stri_rand_strings stri_detect_regex stri_rand_lipsum
#' @importFrom ymlthis yml yml_title yml_author yml_output use_rmarkdown
#' @importFrom methods setClass setMethod callNextMethod new setGeneric packageSlot
#' @importFrom utils menu write.csv
#' @importFrom knitr knit_expand
#' @importFrom xfun embed_file
#' @importFrom glue glue
#' @importFrom htmltools img br
#'
#' @docType package
#' @name i2dash
NULL