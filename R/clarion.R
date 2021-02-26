#' @title Clarion R6-class definition
#'
#' @name Clarion
#'
#' @description
#' Use this to create a clarion object.
#' This object is used by all top-level wilson modules.
#'
#' @section Constructor: Clarion$new(header = NULL, metadata, data, validate = TRUE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#'   \strong{Variable} \tab \strong{Return} \cr
#'   \code{header} \tab A named list. Defaults to NULL. \cr
#'   \code{metadata} \tab Clarion metadata in form of a data.table. \cr
#'   \code{data} \tab Data.table according to metadata. \cr
#'   \code{validate} \tab Logical value to validate on initialization. Defaults to TRUE. \cr
#' }
#'
#' @examples
#' \dontrun{
#' # initializing a new object
#' object <- Clarion$new(header, metadata, data, validate = TRUE)
#'
#' # create a deep copy
#' object_copy <- object$clone(deep = TRUE)
#' }
#'
#' @format NULL
#' @usage NULL
#' @importFrom R6 R6Class
#' @export
Clarion <- R6::R6Class("Clarion",
                       public = list(
                         #' @field header List of global information regarding the whole experiment.
                         header = NULL,
                         #' @field metadata Data.table with additional information for each column.
                         metadata = NULL,
                         #' @field data Data.table containing experiment result data.
                         data = NULL,
                         #' @description
                         #' Returns name of unique identifier column. Assumes first feature to be unique if not specified.
                         #'
                         #' @return Name of the id column.
                         get_id = function() {
                           # return unique_id
                           # if no type return first feature
                           if (is.element("type", names(self$metadata))) {
                             return(self$metadata[type == "unique_id"][["key"]][1])
                           } else {
                             return(self$metadata[level == "feature"][["key"]][1])
                           }
                         },
                         #' @description
                         #' Returns name of name column. If not specified return unique Id.
                         #'
                         #' @return Name of the name column.
                         get_name = function() {
                           # return name
                           # if not existing fall back to unqiue_id
                           if (is.element("type", names(self$metadata)) && is.element("name", self$metadata[["type"]])) {
                             return(self$metadata[type == "name"][["key"]])
                           }
                           return(self$get_id())
                         },
                         #' @description
                         #' Return delimiter used within multi-value fields (no delimiter = NULL).
                         get_delimiter = function() {
                           self$header$delimiter
                         },
                         #' @description
                         #' Logical whether the given column name is delimited.
                         #' @param x Name of the column.
                         #'
                         #' @return boolean
                         is_delimited = function(x) {
                           if (is.element("type", names(self$metadata))) {
                             return(self$metadata[key == x, type] == "array")
                           } else {
                             return(FALSE)
                           }
                         },
                         #' @description
                         #' Get factors to all columns.
                         #' @details  Named factors (e.g. factor1="name") will be cropped to their name.
                         #' @return Returns a data.table columns: key and factor(s) if any.
                         get_factors = function() {
                           # returns data.table key(, factor columns)
                           # only name for named factors (e.g. factor1="name")

                           # get factor columns
                           columns <- grep("^factor\\d+", names(self$metadata), perl = TRUE, value = TRUE)
                           # on no factors return key column
                           if (length(columns) == 0) return(self$metadata[, "key"])

                           # extract names
                           ext_names <- sub("^factor\\d+=\"(.*)\"", replacement = "\\1", columns, perl = TRUE)

                           # get factor table
                           factor_table <- self$metadata[, c("key", columns), with = FALSE]
                           # rename columns
                           names(factor_table)[-1] <- ext_names

                           return(factor_table)
                         },
                         #' @description
                         #' Get level(s) to given column name(s).
                         #'
                         #' @param column One or more column name(s).
                         #'
                         #' @return Provide a vector of levels to the given columnnames in column. Returns NA for missing columns and character(0) if column = NULL.
                         get_level = function(column) {
                           # return levels to given columns
                           self$metadata[match(column, key)][["level"]]
                         },
                         #' @description
                         #' Get label(s) to given column name(s).
                         #'
                         #' @param column One or more column name(s).
                         #' @param sub_label Whether the sub_label should be included.
                         #' @param sep Separator between label and sub_label.
                         #'
                         #' @details If a column does not have a label the key is returned.
                         #' @return Provides a vector of labels (+ sub_label) to the given columnnames in column. Returns NA for missing columns and all labels if column = NULL.
                         get_label = function(column = NULL, sub_label = TRUE, sep = " ") {
                           # return label to given columns

                           # identify row index for given column(s)
                           if (is.null(column)) {
                             index <- seq_len(nrow(self$metadata))
                           } else {
                             index <- match(column, self$metadata[["key"]])
                           }

                           if (is.element("label", names(self$metadata))) {
                             label <- self$metadata[["label"]]
                             # replace empty label with key
                             replace_ind <- which(label == "")
                             label[replace_ind] <- self$metadata[replace_ind][["key"]]
                           } else {
                             label <- self$metadata[["key"]]
                           }

                           # add sub_label
                           if (sub_label && is.element("sub_label", names(self$metadata))) {
                             # TODO don't add sep when sub_label empty
                             label <- paste(label, self$metadata[["sub_label"]], sep = sep)
                           }

                           return(label[index])
                         },
                         #' @description
                         #' Check the object for inconsistencies.
                         #'
                         #' @param solve For solve = TRUE try to resolve some warnings.
                         validate = function(solve = TRUE) {
                           # validate header
                           private$check_delimiter()
                           # validate metadata
                           private$check_metadata_header()
                           private$check_key()
                           private$check_level()
                           private$check_type()
                           private$check_label()
                           private$check_order(solve)
                           # validate data
                           private$check_data_header(solve)
                           private$check_data_min()
                           private$check_data_column_types()
                         },
                         #' @description
                         #' Initialize a new clarion object.
                         #'
                         #' @param header A named list. Defaults to NULL.
                         #' @param metadata Clarion metadata in form of a data.table.
                         #' @param data Data.table according to metadata.
                         #' @param validate Logical value to validate on initialization. Defaults to TRUE.
                         #'
                         #' @return Clarion object.
                         initialize = function(header = NULL, metadata, data, validate = TRUE) {
                           self$header <- header
                           self$metadata <- metadata
                           self$data <- data

                           if (validate) self$validate()

                           # coerce unique_id and name to character
                           if (self$get_id() == self$get_name()) {
                             cols <- self$get_id()
                           } else {
                             cols <- c(self$get_id(), self$get_name())
                           }
                           self$data[, (cols) := lapply(.SD, as.character), .SDcols = cols]

                           # set index
                           data.table::setindexv(self$metadata, "key")
                           data.table::setindexv(self$data, self$get_id())
                         },
                         #' @description
                         #' Save the object as a clarion file.
                         #'
                         #' @param file Filename for the file to be written.
                         write = function(file) {
                           # prepare
                           if (!is.null(self$header)) {
                             flat_header <- private$flatten_header()
                           }
                           flat_metadata <- private$flatten_metadata()

                           # write
                           if (!is.null(self$header)) data.table::fwrite(flat_header, file = file, col.names = FALSE, sep = "\t", quote = FALSE)
                           data.table::fwrite(flat_metadata, file = file, col.names = TRUE, sep = "\t", append = ifelse(is.null(self$header), FALSE, TRUE), quote = FALSE)
                           data.table::fwrite(self$data, file = file, col.names = TRUE, sep = "\t", append = TRUE, quote = FALSE)
                         }
                       ),
                       private = list(
                         # deep clone to force data.table copy
                         deep_clone = function(name, value) {
                           # invoke a deep copy for metadata and data field
                           if (name %in% c("metadata", "data")) {
                             data.table::copy(value)
                           } else {
                             value
                           }
                         },
                         # prepare header to be saved as clarion flat-file
                         flatten_header = function() {
                           # reformat header list to !name=value
                           flat_header <- paste0("!", names(self$header), "=", self$header)
                           data.table::data.table(flat_header)
                         },
                         # prepare metadata to be saved as clarion flat-file
                         flatten_metadata = function() {
                           flat_metadata <- data.table::copy(self$metadata)
                           # add leading '#'
                           names(flat_metadata)[1] <- paste0("#", names(flat_metadata)[1])
                           flat_metadata[, names(flat_metadata)[1] := paste0("#", flat_metadata[[1]])]

                           return(flat_metadata)
                         },
                         # checks ####
                         ## header checks
                         check_delimiter = function() {
                           if (is.element("delimiter", names(self$header))) {
                             # case: no type column/ no type = array
                             if (!is.element("type", names(self$metadata)) || !is.element("array", self$metadata[["type"]])) {
                               warning("Found in-field-delimiter '", self$header$delimiter, "' but no type=array columns (in metadata) to apply to.")
                             }
                           }
                         },
                         ## metadata checks
                         check_metadata_header = function() {
                           # case: invalid column names
                           valid_names <- c("key", "factor\\d+(=\".*\")?", "level", "type", "label", "sub_label")
                           regex <- paste0("^", valid_names, "$", collapse = "|")
                           invalid_names <- grep(regex, names(self$metadata), invert = TRUE, value = TRUE, perl = TRUE)
                           if (length(invalid_names) > 0) {
                             warning("Metadata: Unexpected column names detected: ", paste0(invalid_names, collapse = ", "))
                           }
                           # case: missing mandatory column
                           requires <- c("key", "level")
                           missing <- !is.element(requires, names(self$metadata))
                           if (any(missing)) {
                             stop("Metadata: Mandatory column(s) missing! ", paste0(requires[missing], collapse = ", "))
                           }
                         },
                         check_key = function() {
                           # case: duplicated keys
                           if (anyDuplicated(self$metadata[["key"]])) {
                             stop("Metadata: Duplicate(s) in key detected! The following key(s) are duplicated: ", paste0(unique(self$metadata[["key"]][duplicated(self$metadata[["key"]])]), collapse = ", "))
                           }
                           # case: key not in data
                           missing <- setdiff(self$metadata[["key"]], names(self$data))
                           if (length(missing) > 0) {
                             warning("Metadata rows and data columns differ! Following rows are not defined in data: ", paste0(missing, collapse = ", "))
                           }
                         },
                         check_level = function() {
                           # case: invalid level
                           valid <- c("feature", "sample", "condition", "contrast")
                           unknown <- grep(pattern = paste0(valid, collapse = "|"), x = self$metadata[["level"]], perl = TRUE, invert = TRUE, value = TRUE)
                           if (length(unknown) > 0) {
                             warning("Metadata: Unknown level(s) found: ", paste0(unknown, collapse = ", "))
                           }
                           # case: minimal level requirements (feature + sample|condition|contrast)
                           if (!is.element("feature", self$metadata[["level"]]) && !any(is.element(c("sample", "condition", "contrast"), self$metadata[["level"]]))) {
                             stop("Metadata: Minimum level requirements not met! At least one feature (unique_id) and one sample, condition or contrast needed.")
                           }
                         },
                         check_type = function() {
                           if (is.element("type", names(self$metadata))) {
                             feature_types <- c("unique_id", "name", "category", "array")
                             remaining_types <- c("score", "ratio", "probability", "array")
                             # case: type doesn't fit level
                             # select and return keys with unknown type
                             unknown <- self$metadata[level == "feature"][!type %in% feature_types][["key"]]
                             unknown <- append(unknown, self$metadata[level %in% c("sample", "condition", "contrast")][!type %in% remaining_types][["key"]])
                             if (length(unknown) > 0) {
                               warning("Metadata: Level doesn't match type:", paste0(unknown, collapse = ", "))
                             }
                             # case: no unique_id defined
                             if (!is.element("unique_id", self$metadata[["type"]])) {
                               stop("Metadata: No unique_id defined in type! Please define a unique_id.")
                             }
                             # case: multiple unique_ids
                             if (sum(is.element(self$metadata[["type"]], "unique_id")) > 1) {
                               warning("Metadata: Found multiple unique_ids! Only first will be used.")
                             }
                             # case: type = array but no delimiter
                             if (is.element("array", self$metadata[["type"]]) && !is.element("delimiter", names(self$header))) {
                               stop("Found type=array but no delimiter! Columns with multi-value fields require delimiter (in header) and type=array (in metadata).")
                             }
                           }
                         },
                         check_label = function() {
                           if (is.element("label", names(self$metadata))) {
                             # case: contrast label not delimited by '|'
                             contrast_labels <- grep(pattern = "\\|", x = self$metadata[level == "contrast"][["label"]], perl = TRUE, invert = TRUE, value = TRUE)
                             if (length(contrast_labels) > 0) {
                               warning("Metadata: Missing '|' delimiter in contrast label(s): ", paste0(contrast_labels, collapse = ", "))
                             }
                           }
                         },
                         check_order = function(solve = TRUE) {
                           # case: key order differs to data column order
                           if (!isTRUE(all.equal(self$metadata[["key"]], names(self$data)))) {
                             # re-order metadata to match data column order
                             if (solve) {
                               self$metadata <- self$metadata[na.omit(match(names(self$data), self$metadata[["key"]]))]
                             }
                             warning("Metadata: Order between metadata keys and data columns differ. This is discouraged as it can eventually lead to problems.", if (solve) "\nAdjusting metadata order.")
                           }
                         },
                         ## data checks
                         check_data_header = function(solve = TRUE) {
                           # case: column not defined in metadata
                           missing <- setdiff(names(self$data), self$metadata[["key"]])
                           if (length(missing) > 0) {
                             if (solve) {
                               # omit undefined columns
                               self$data[, (missing) := NULL]
                             }
                             warning("Metadata rows and data columns differ! Following rows are missing in metadata: ", paste0(missing, collapse = ", "), if (solve) "\nOmitting data column(s)!")
                           }
                           # case: duplicated column names
                           if (anyDuplicated(names(self$data))) {
                             stop("Data: Column names not unique! Following names occur more than once: ", paste0(unique(names(self$data)[duplicated(names(self$data))]), collapse = ", "))
                           }
                         },
                         check_data_min = function() {
                           # case: minimum requirements not met (two columns: feature(unique_id) + sample|condition|contrast)
                           if (ncol(self$data) < 2) {
                             stop("Data: Minimum requirements not met! At least two columns needed, one with unique identifier and one with numeric values.")
                           }
                         },
                         check_data_column_types = function() {
                           # case: level = sample, condition, contrast not numeric
                           # except type=array because of delimiter
                           if (is.element("type", names(self$metadata))) {
                             expected_numeric_cols <- self$metadata[level %in% c("sample", "condition", "contrast")][type != "array"][["key"]]
                           } else {
                             expected_numeric_cols <- self$metadata[level %in% c("sample", "condition", "contrast")][["key"]]
                           }
                           not_numeric <- names(self$data[, expected_numeric_cols, with = FALSE][, which(!vapply(self$data[, expected_numeric_cols, with = FALSE], is.numeric, FUN.VALUE = logical(1)))])
                           if (length(not_numeric) > 0) {
                             stop("Data: Column(s): ", paste0(not_numeric, collapse = ", "), " not numeric! Probably wrong decimal separator.")
                           }
                         }
                       ),
                       lock_class = TRUE # prevent class modification
)
