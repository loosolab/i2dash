

#' Converting MaxQuant Output file proteinGroups.txt to CLARION format
#' by creating a headline of metadata for each column
#'
#' List with columns of reduced version (see config.json file)
#' If you only want the samples of a specific keyword write: column;exp
#' For example:
#'   You got:
#'       Intensity
#'       Intensity 'experiment_name'
#' Do you want both add "Intensity" to the list.
#' Do you only want the sample add "Intensity;exp" to the list
#' Anything else like 'Intensity;ex' or 'Intensity;' results in writing both.
#' Only works if there are samples of that type. If not, column does not show up in file
#'
#' @author Rene Wiegandt
#' @param proteinGroups_in path of proteinGroup.txt file
#' @param summary_in path of belonging summary.txt file
#' @param outfile path of full CLARION output file
#' @param outfile_reduced path of reduced CLARION output file
#' @param config path of config file (containing information about metadata)
#' @param delimiter delimiter (Default = ;)
#' @param format pre-header information about format (optional)
#' @param version pre-header information about version (optional)
#' @param experiment_id pre-header information about experiment id (optional)
#'
#' @return TRUE on success
#'
#' @export
parse_MaxQuant <- function(proteinGroups_in, summary_in, outfile, outfile_reduced, config = system.file("extdata", "parser_MaxQuant_config.json", package = "wilson"), delimiter = ";", format = NULL, version = NULL, experiment_id = NULL){
  if (missing(proteinGroups_in)) {
    stop("The proteinGroups file was not given")
  }
  if (missing(summary_in)) {
    stop("The summary file was not given")
  }
  if (missing(outfile)) {
    stop("The output file was not given")
  }
  if (missing(outfile_reduced)) {
    stop("The output_reduced file was not given")
  }

  # parsers json config file
  # @param config path of config file
  # @return data.table with metadata
  get_meta_from_config <- function(meta_config) {
    dr <- lapply(meta_config$meta, function(r) {
      data.table::data.table("col_name" = r$col_name,
                                 "level" = r$level,
                                 "type" = r$type,
                                 "label" = r$label,
                                 "sublabel" = r$sublabel
                                )
    })
    do.call("rbind", dr)
  }


  # Get the type of the sample
  # Cutting of the experiment name and checking in given lists if left column name is in one of those lists
  # Depending on in which list it is returns the type
  # @param exp_name experiment name

  # @param scores,ratios,category,ary vectors with Strings
  # @return String type of given column
  get_sample_type <- function(name, scores, ratios, prob, category, ary) {

    name_split <- strsplit(name, " ")
    if (grepl("[[:digit:]]{1,2}", utils::tail(name_split[[1]], 1))) {
      name <- paste(utils::head(name_split[[1]], -1), collapse = " ")
    }
    if (name %in% scores) return("score")
    if (name %in% ratios) return("ratio")
    if (name %in% prob) return("probability")
    if (name %in% category) return("category")
    if (name %in% ary) return("array")

    return("unknown")
  }

  # Get the level of the sample column
  # Checking for keywords inside of the column name
  # Each keyword is given one level
  # @param col_head column head
  # @return String level of given column
  get_sample_level <- function(col_head, isSample, full_list) {
    # Get the level of all 'sample' columns.
    # Default: level is "sample"
    if (grepl("Ratio", col_head, perl = TRUE)) {
      if (grepl("type", col_head, perl = TRUE)) return("feature")
      return("contrast")
    }
    if (grepl("type", col_head, perl = TRUE)) return("feature")
    if (grepl("Fraction [[:digit:]]{1,2}", col_head, perl = TRUE)) return("condition")
    if (isSample) return("sample")
    if (col_head %in% full_list) return("contrast")
    return("unknown")
  }


  # Get label and sublabel of remaining columns
  # remaining columns <- columns which are not in meta or a sample column
  # @param col_head column head
  # @return list with label and sublabel
  get_remaining_labeling <- function(col_head) {
    col_head_split <- strsplit(col_head, " ")
    if (length(col_head_split) > 1) {
      sublabel <- col_head_split[[1]][length(col_head_split[[1]])]
      if (grepl(sublabel, "[%]", fixed = TRUE)) {
        label <- paste(utils::head(col_head_split[[1]], -2), collapse = " ")
        sublabel <- paste(utils::tail(col_head_split[[1]], 2), collapse = " ")
      } else {
        label <- paste(utils::head(col_head_split[[1]], -1), collapse = " ")
        sublabel <- col_head_split[[1]][length(col_head_split[[1]])]
      }
      return(list(label, sublabel))
    }
    return(list(col_head, ""))
  }


  # Get list of all column names with experiment name, which are in the reduced version
  # @param meta_full data table with metadata
  # @param reduced_list raw list of all columns of every quantification method, which need to in the reduced version
  # @param exp_names list of experiment names
  # @param col_names list of all column names
  # @return list: 1 <- list of all reduced column names, 2 <- columns which are in the raw list but not in the final list
  get_reduced_version <- function(meta_full, reduced_list, exp_names, col_names) {
    red_split <- strsplit(reduced_list, ";")
    red_exp_list <- unlist(lapply(red_split, function(split_name) {
      # if length == 2 entry of reduced list is column_name;exp
      # only adding column_name + experiment name to the list
      if (length(split_name) == 2) {
        red_col_name <- lapply(exp_names, function(exp_name) {
          rcn <- paste(split_name[1], exp_name)
          # special case for Sequence Coverage exp_name [%]
          if (!(rcn %in% col_names) && (paste(rcn, "[%]") %in% col_names)) {
            rcn <- paste(rcn, "[%]")
          }
          return(rcn)
        })
      } else {
        # adding both column name +  experiment_name and column name without experiment name to list
        red_col_name <- lapply(1:(length(exp_names) + 1), function(i) {
          if (i <= length(exp_names)) {
            if (paste(split_name, exp_names[i]) %in% col_names) {
              return(paste(split_name, exp_names[i]))
            }
          } else {
            return(split_name)
          }
        })
      }
    }))
    overlap <- setdiff(red_exp_list, col_names)
    red_exp_list <- red_exp_list[!(red_exp_list %in% overlap)]
    return(list(red_exp_list, overlap))
  }


  # writing clarion file
  # @param meta data table with meta data
  # @param out output file
  # @param format format
  # @param version version number
  # @param exp_id experiment id
  # @param pGroups data table protein groups file
  write_clarion_file <- function(meta, out, format, version, exp_id, pGroups, delimiter) {
    to_append <- FALSE
    if (!missing(format)) {
      write(paste0("!format=", format), file = out, append = to_append)
      to_append <- TRUE
    }
    if (!missing(version)) {
      write(paste0("!version=", version), file = out, append = to_append)
      to_append <- TRUE
    }
    if (!missing(exp_id)) {
      write(paste0("!experiment_id=", exp_id), file = out, append = to_append)
      to_append <- TRUE
    }
    write(paste0("!delimiter=", delimiter), file = out, append = to_append)
    write("#key\tfactor1\tlevel\ttype\tlabel\tsub_label", file = out, append = TRUE)
    data.table::fwrite(meta, file = out, sep = "\t", append = TRUE, col.names = FALSE, quote = FALSE)
    data.table::fwrite(pGroups, file = out, sep = "\t", append = TRUE, col.names = TRUE, quote = FALSE)
  }

  # reading files in data tables
  proteinGroups <- data.table::fread(proteinGroups_in, header = TRUE, quote = "")
  summary_file <- data.table::fread(summary_in, header = TRUE)

  meta_config <- tryCatch({
    rjson::fromJSON(file = config)
  }, error = function(cond) {
    stop("Could not read config file")
  }, warning = function(w) {
    stop("Could not read config file")
  })

  # getting experiment names
  if ("Experiment" %in% colnames(summary_file)) {
    exp_names <- unique(summary_file[Experiment != "", Experiment])
  } else {
    stop("wrong format on summary file: column \'Experiment\' misssing")
  }

  meta <- get_meta_from_config(meta_config = meta_config)

  sample_scores <- meta_config$type_scores
  sample_ratios <- meta_config$type_ratios
  sample_probability <- meta_config$type_probability

  sample_category <- meta_config$type_category
  sample_ary <- meta_config$type_array
  reduced_list <- meta_config$reduced_list
  full_sample_list <- c(sample_scores, sample_ratios, sample_probability, sample_category, sample_ary)
  if (is.null(reduced_list)) {
    stop("reduced_list is missing in config file")
  }

  # get column names
  col_names <- colnames(proteinGroups)

  # delete rows from meta, which are not in col_names
  meta_trim <- meta[col_name %in% col_names]

  # get metadata for each sample column
  # append rows to data table with metadata
  samples_list <- lapply(col_names, function(col_head) {

    unlist(lapply(exp_names, function(name) {
      name_brackets <- paste0("\\Q", name)
      exp_regex <- paste0("\\Q ", name)
      sample_description <- strsplit(col_head, exp_regex)
      if (length(grep(name_brackets, col_head, perl = TRUE)) == 1 ) {  # Does column name contains experiment name?
        de <- data.table::data.table("col_name" = c(col_head),
                                   "level" = c(get_sample_level(col_head = col_head, isSample =  TRUE, full_list = full_sample_list)),
                                   "type" = c(get_sample_type(name = sample_description[[1]][1], scores =  sample_scores,
                                                              ratios = sample_ratios, prob = sample_probability, category =  sample_category,
                                                              ary =  sample_ary)),
                                   "label" = c(name),
                                   "sublabel" = c(trimws(gsub(name_brackets, "", col_head), "r"))
        )
        return(de)
      }
    }))

  })
  samples <- do.call("rbind", Filter(Negate(is.null), samples_list))
  meta_half <- rbind(meta_trim, samples)

  # get metadata for each remaining column(columsn which are specific for a certain quantification methode)
  # append rows to data table with metadata
  remaining_list <- lapply(col_names, function(col_head) {
    if (!(col_head %in% meta_half[["col_name"]])) {
      label_sublabel <- get_remaining_labeling(col_head = col_head)
      de2 <- data.table::data.table("col_name" = c(col_head),
                                  "level" = c(get_sample_level(col_head = col_head, isSample =  FALSE, full_list = full_sample_list)),
                                  "type" = c(get_sample_type(name = col_head, scores = sample_scores, ratios =  sample_ratios,
                                                             prob = sample_probability, category = sample_category, ary = sample_ary)),
                                  "label" = c(label_sublabel[1]),
                                  "sublabel" = c(label_sublabel[2])
      )
      return(de2)
    }
  })
  remaining <- do.call("rbind", Filter(Negate(is.null), remaining_list))
  meta_full <- rbind(meta_half, remaining)
  meta_full$col_name <- sub("^", "#", meta_full$col_name)

  # add column factor 1 and reorder the columns
  meta_full$factor1 <- ""
  meta_full <- meta_full[, c("col_name", "factor1", "level", "type", "label", "sublabel")]

  # get data.table with reduced metadata
  reduced <- get_reduced_version(meta_full = meta_full, reduced_list =  reduced_list,
                                 exp_names = exp_names, col_names = col_names)
  meta_reduced <- meta_full[meta_full$col_name %in% sub("^", "#", reduced[[1]]), ]

  # if there are unknown columns the user gets a warning with all unknown columns
  # unknown columns wont be writen in the meta data header
  if (nrow(meta_full[level == "unknown"]) > 0) {
    meta_warn <- gsub("#", "", meta_full[level == "unknown", col_name])
    warning("Following columns are unknown and have been removed (Check JSON config file: 'meta'): ", paste(meta_warn, collapse = ", "))
    meta_full <- meta_full[level != "unknown"]
  }

  # if there are unknown column types the user gets a warning with all columns with unknown type
  # columns with unknown types wont be writen in the meta data header
  if (nrow(meta_full[type == "unknown"]) > 0) {
    meta_warn <- gsub("#", "", meta_full[type == "unknown", col_name])
    warning("Due to unknown type follwing columns were removed (Check JSON config file: 'type_X'): ", paste(meta_warn, collapse = ", "))
    meta_full <- meta_full[type != "unknown"]
  }

  # writing advanced CLARION file
  write_clarion_file(meta = meta_full, out = outfile, format =  format,
                     version = version, exp_id = experiment_id, pGroups = proteinGroups, delimiter = delimiter)

  # writing reduced CLARION file
  write_clarion_file(meta = meta_reduced, out = outfile_reduced, format = format,
                     version = version, exp_id = experiment_id, pGroups = proteinGroups, delimiter = delimiter)

  return(TRUE)
}

#' Method to parse input file.
#'
#' @param file Path to file that needs parsing.
#' @param dec The decimal separator. See \code{\link[data.table]{fread}}.
#'
#' @return Clarion object. See \code{\link[wilson]{Clarion}}
#'
#' @import data.table
#'
#' @export
parser <- function(file, dec = ".") {
  message(paste("Parsing file:", file))

  # number of rows for each part
  con <- file(file, open = "r")
  num_header <- 0
  num_metadata <- 0

  tryCatch(expr = {
    while (TRUE) {
      line <- readLines(con = con, n = 1)

      if (grepl("^!", line, perl = TRUE)) {
        num_header <- num_header + 1
      } else if (grepl("^#", line, perl = TRUE)) {
        num_metadata <- num_metadata + 1
      } else {
        break
      }
    }
  }, finally = {
    close(con = con)
  })

  ### parse header
  if (num_header > 0) {
    header <- data.table::fread(input = file, fill = TRUE, header = FALSE, dec = dec, nrows = num_header, integer64 = "double")
    # cut of leading !
    header <- as.list(gsub("^!", "", header[[1]]))
    # make named list
    header_names <- gsub("=.*$", "", header, perl = TRUE)
    header <- as.list(gsub("^.*?=", "", header, perl = TRUE))
    names(header) <- header_names
    # remove quotes from delimiter
    if (!is.null(header$delimiter) && grepl(header$delimiter, pattern = '^".*"$', perl = TRUE)) {
      header$delimiter <- substr(header$delimiter, start = 2, stop = nchar(header$delimiter) - 1)
    }
  } else {
    header <- NULL
  }

  ### parse metadata
  metadata <- data.table::fread(input = file, skip = num_header, header = FALSE, nrows = num_metadata, fill = TRUE, dec = dec, integer64 = "double")
  # cut off leading #
  metadata[, names(metadata)[1] := gsub("^#", "", metadata[[1]])]
  # set first line as header
  names(metadata) <- as.character(metadata[1])

  # remove empty columns
  empty_cols <- union(
    which(colSums(is.na(metadata)) == nrow(metadata)), # indices of full na columns
    which(colSums(metadata == "") == nrow(metadata)) # indices of full "" columns
  )
  if (length(empty_cols) > 0) {
    metadata[, (empty_cols) := NULL]
  }

  # delete first line
  metadata <- metadata[-1]

  ### parse data
  data <- data.table::fread(input = file, header = TRUE, skip = num_header + num_metadata, fill = FALSE, dec = dec, integer64 = "double")

  data.table::setindexv(metadata, names(metadata)[1])
  data.table::setindexv(data, names(data)[1])

  return(Clarion$new(header = header, metadata = metadata, data = data))
}

#' TOBIAS TFBS table to clarion parser
#'
#' Click \href{https://github.molgen.mpg.de/loosolab/TOBIAS}{here} for more information about TOBIAS.
#'
#' @param input Path to input table
#' @param output Output path.
#' @param filter_columns Either a vector of columnnames or a file containing one columnname per row.
#' @param filter_pattern Keep columns matching the given pattern. Uses parameter filter_columns for matching if set. In the case of no matches a warning will be issued and all columns will be used.
#' @param config Json file containing metadata information for all columns. Will use first occurence for duplicate column names.
#' @param omit_NA Logical whether all rows containing NA should be removed.
#' @param condition_names Vector of condition names. Default = NULL. Used to classify columns not provided in config.
#' @param condition_pattern Used to identify condition names by matching and removing given pattern with \code{\link[base]{grep}}. Ignored when condition_names is set.
#' @param in_field_delimiter Delimiter for multi value fields. Default = ','.
#' @param dec Decimal separator. Used in file reading and writing.
#' @param ... Used as header information.
#'
#' @details During conversion the parser will try to use the given config (if provided) to create the \href{https://github.molgen.mpg.de/loosolab/wilson-apps/wiki/CLARION-Format}{Clarion} metadata. In the case of insufficient config information it will try to approximate by referencing condition names issuing warnings in the process.
#' @details As the format requires an unique id the parser will create one if necessary.
#' @details Factor grouping (metadata factor columns) is currently not implemented!
#'
#' @export
tobias_parser <- function(input, output, filter_columns = NULL, filter_pattern = NULL, config = system.file("extdata", "tobias_config.json", package = "wilson"), omit_NA = FALSE, condition_names = NULL, condition_pattern = "_bound$", in_field_delimiter = ",", dec = ".", ...) {
  ## filter data columns
  # check if filter columns is a file or a vector
  if (!is.null(filter_columns) && file.exists(filter_columns)) {
    select_columns <- scan(file = filter_columns, what = character())
  } else {
    select_columns <- filter_columns
  }

  # filter pattern
  if (!is.null(filter_pattern)) {
    # use file header if filter_columns is empty
    if (is.null(select_columns)) {
      # only read header
      select_columns <- names(data.table::fread(input = input, header = TRUE, nrows = 0))
    }

    select_columns <- grep(pattern = filter_pattern, x = select_columns, value = TRUE)

    if (identical(select_columns, character(0))) {
      warning("No column matches for given filter pattern! Proceeding with all columns.")
    }
  }

  ##### data
  data <- data.table::fread(input, dec = dec, select = select_columns, header = TRUE)

  # omit na rows
  if (omit_NA) {
    data <- stats::na.omit(data)
  }

  ##### metadata
  metadata <- data.table::data.table(names(data))

  # load config
  if (!is.null(config)) {
    config_file <- RJSONIO::fromJSON(config)
    # get column names from config file
    col_names <- vapply(X = config_file$meta, FUN.VALUE = character(1), FUN = function(x) {
      x[["col_name"]]
    })
  } else {
    config_file <- NULL
  }

  # identify conditions
  if (is.null(condition_names)) {
    conditions <- gsub(pattern = condition_pattern, replacement = "", x = grep(pattern = condition_pattern, x = metadata[[1]], value = TRUE))
  } else {
    conditions <- condition_names
  }


  ## create metadata row by row
  unique_id_fallback <- NULL
  condition_pattern <- paste0(conditions, collapse = "|")
  approx_columns <- NULL

  meta_rows <- lapply(metadata[[1]], function(x) {
    # is column information provided in config?
    if (!is.null(config_file) && is.element(x, col_names)) {
      # uses first appearance in config file in case of duplicates
      return(config_file$meta[[which.max(col_names == x)]][-1])
    }

    # if no information is provided via config try to guess meta-information
    # utilize condition names to do so
    approx_columns <<- c(approx_columns, x)

    ## level
    # get distance of all conditions matched to x
    # count number of exact substring matches
    match_dist <- utils::adist(conditions, x) - nchar(x) + nchar(conditions)
    count_matches <- sum(match_dist == 0)

    if (count_matches == 1) {
      level <- "condition"
    } else if (count_matches > 1) {
      level <- "contrast"
    } else {
      level <- "feature"
    }

    ## type
    if (level == "feature") {
      if (any(grepl(pattern = in_field_delimiter, x = data[[x]], fixed = TRUE))) {
        type <- "array"
      } else {
        # define fallback unique_id in case none is defined through config
        # this will redefine first unique feature with type category as unqiue_id
        if (is.null(unique_id_fallback) && anyDuplicated(data[[x]]) == 0) {
          unique_id_fallback <<- x
        }
        type <- "category"
      }
    } else {
      if (!is.numeric(data[[x]])) {
        type <- "array"
      } else if (grepl(pattern = "fc|fold[\\._\\- ]?change", x = x, perl = TRUE, ignore.case = TRUE)) {
        type <- "ratio"
      } else if (grepl(pattern = "p[\\._\\- ]?val|p[\\._\\- ]?adj", x = x, perl = TRUE, ignore.case = TRUE)) {
        type <- "probability"
      } else {
        type <- "score"
      }
    }

    ## label/ sub_label
    label <- sub_label <- ""
    label_parts <- unlist(strsplit(x = x, split = "_"))

    if (length(label_parts) == 1) {
      label <- label_parts
    } else if (length(label_parts) == 2) {
      label <- label_parts[1]
      sub_label <- label_parts[2]
    } else if (length(label_parts) >= 3 && level == "contrast") {
      # replace '_' with whitespace
      condition_pattern <- gsub(pattern = "_", replacement = " ", x = condition_pattern, fixed = TRUE)
      x <- gsub(pattern = "_", replacement = " ", x = x, fixed = TRUE)
      # get first condition using all identified conditions as pattern
      first_condition <- gsub(pattern = paste0("(^", condition_pattern, ").*"), replacement = "\\1", x = x)
      # strip first condition
      stripped_condition <- substring(x, first = nchar(first_condition) + 2) # + 1 because parameter is inclusive and + 1 for whitespace
      # get new first condition
      second_condition <- gsub(pattern = paste0("(^", condition_pattern, ").*"), replacement = "\\1", x = stripped_condition)

      label <- paste0(first_condition, "|", second_condition)
      sub_label <- substring(stripped_condition, first = nchar(second_condition) + 2) # + 1 because parameter is inclusive and + 1 for whitespace
    } else {
      label <- paste0(label_parts[-length(label_parts)], collapse = " ")
      sub_label <- label_parts[length(label_parts)]
    }

    return(c(level, type, label, sub_label))
  })

  # meta approximation warning
  if (!is.null(approx_columns)) {
    warning("Missing information in config! Tried guessing meta-information for ", paste0(approx_columns, collapse = ", "))
  }

  # list of vectors (rows) to matrix
  meta_matrix <- do.call(rbind, meta_rows)
  metadata <- cbind(metadata, meta_matrix)
  names(metadata) <- c("key", "level", "type", "label", "sub_label")

  # set unique_id fallback
  if (!any(metadata[["type"]] == "unique_id")) {
    if (!is.null(unique_id_fallback)) {
      metadata[key == unique_id_fallback, "type"] <- "unique_id"
    } else {
      # setup unique_id column if there is neither a defined column nor a fallback

      # create id column
      data[, "id" := seq_len(nrow(data))]
      # move id column to first position
      new_order <- c("id", names(data)[ names(data) != "id"])
      data <- data[, new_order, with = FALSE]

      id_row <- data.table::data.table("id", level = "feature", type = "unique_id", label = "id", sub_label = "")
      names(id_row)[1] <- "key"
      # add meta entry
      metadata <- rbind(id_row, metadata)
    }
  }

  ##### header
  header <- c(
    list(
      format = "Clarion",
      version = "1.0"),
    list(...)
  )

  # add delimiter if necessary
  if (any(metadata[["type"]] == "array")) {
    header <- append(x = header, values = list(delimiter = in_field_delimiter), after = 2)
  }

  ##### validate
  # create clarion object for validation
  clarion <- Clarion$new(header = header, metadata = metadata, data = data)

  ##### write
  # TODO implement and use clarion write function
  # write clarion
  # header
  flat_header <- data.table::data.table(paste0("!", names(clarion$header), "=", clarion$header))
  data.table::fwrite(x = flat_header, file = output, col.names = FALSE, sep = "\t", dec = dec)
  # metadata
  # add '#'
  names(clarion$metadata)[1] <- paste0("#", names(clarion$metadata)[1])
  clarion$metadata[, names(clarion$metadata)[1] := paste0("#", clarion$metadata[[1]])]
  data.table::fwrite(x = clarion$metadata, file = output, col.names = TRUE, sep = "\t", append = TRUE, quote = FALSE, dec = dec)
  # data
  data.table::fwrite(x = clarion$data, file = output, col.names = TRUE, sep = "\t", append = TRUE, dec = dec)
}
