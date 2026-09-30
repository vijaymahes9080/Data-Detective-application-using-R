# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/data_loader.R
# Description: CSV validation, safe loading, and data sanitization
# ==============================================================================

#' Validate an uploaded CSV file path
#' @param file_path String path to file
#' @return List with status, errors, and warnings
validate_csv_file <- function(file_path) {
  result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )

  if (is.null(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "No file path provided.")
    return(result)
  }

  if (!file.exists(file_path)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "File does not exist on disk.")
    return(result)
  }

  file_size <- file.info(file_path)$size
  if (is.na(file_size) || file_size == 0) {
    result$valid <- FALSE
    result$errors <- c(result$errors, "The uploaded file is empty (0 bytes).")
    return(result)
  }

  # File extension check
  ext <- tolower(tools::file_ext(file_path))
  if (ext != "csv" && ext != "txt") {
    result$warnings <- c(result$warnings, paste0("File extension is '.", ext, "'. Expected a standard .csv format."))
  }

  return(result)
}

#' Safely load and validate CSV data
#' @param file_path Path to the CSV file
#' @return A list with data, raw_data, warnings, and metadata
load_csv_data <- function(file_path) {
  val <- validate_csv_file(file_path)
  if (!val$valid) {
    return(list(
      success = FALSE,
      error = paste(val$errors, collapse = " | "),
      data = NULL,
      raw_data = NULL,
      warnings = character(0),
      metadata = list()
    ))
  }

  warnings_list <- val$warnings
  df <- NULL

  # Attempt fast and safe read
  tryCatch({
    # Prefer readr if installed, otherwise robust read.csv
    if (requireNamespace("readr", quietly = TRUE)) {
      df <- suppressWarnings(
        readr::read_csv(
          file_path,
          show_col_types = FALSE,
          name_repair = "unique",
          na = c("", "NA", "N/A", "null", "NULL", "NaN")
        )
      )
      df <- as.data.frame(df)
    } else {
      df <- utils::read.csv(
        file_path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA", "N/A", "null", "NULL", "NaN")
      )
    }
  }, error = function(e) {
    warnings_list <<- c(warnings_list, paste0("Read error encountered: ", e$message))
  })

  # Fallback if first read failed or produced null
  if (is.null(df) || nrow(df) == 0 && ncol(df) == 0) {
    tryCatch({
      df <- utils::read.csv(
        file_path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA", "N/A", "null", "NULL", "NaN")
      )
    }, error = function(e) {
      df <<- NULL
    })
  }

  if (is.null(df)) {
    return(list(
      success = FALSE,
      error = "Could not parse dataset. Please ensure it is a valid comma-separated text file.",
      data = NULL,
      raw_data = NULL,
      warnings = warnings_list,
      metadata = list()
    ))
  }

  if (nrow(df) == 0) {
    return(list(
      success = FALSE,
      error = "Dataset is empty: 0 rows found.",
      data = NULL,
      raw_data = NULL,
      warnings = warnings_list,
      metadata = list()
    ))
  }

  if (ncol(df) == 0) {
    return(list(
      success = FALSE,
      error = "Dataset contains no columns.",
      data = NULL,
      raw_data = NULL,
      warnings = warnings_list,
      metadata = list()
    ))
  }

  raw_df <- df
  orig_names <- names(df)

  # Check and repair column names
  clean_col_names <- orig_names
  # Handle empty column names
  empty_names <- which(is.na(clean_col_names) | trimws(clean_col_names) == "")
  if (length(empty_names) > 0) {
    clean_col_names[empty_names] <- paste0("V_", empty_names)
    warnings_list <- c(warnings_list, paste0("Renamed ", length(empty_names), " empty column header(s)."))
  }

  # Handle duplicate column names
  if (any(duplicated(clean_col_names))) {
    clean_col_names <- make.unique(clean_col_names, sep = "_")
    warnings_list <- c(warnings_list, "Duplicate column names were renamed internally for analysis.")
  }

  names(df) <- clean_col_names

  # Inspect for date strings and parse if appropriate
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      valid_vals <- df[[col]][!is.na(df[[col]]) & nzchar(trimws(df[[col]]))]
      if (length(valid_vals) >= 5) {
        sample_subset <- head(valid_vals, 30)
        parsed_dates <- suppressWarnings(as.Date(sample_subset, optional = TRUE))
        if (all(!is.na(parsed_dates))) {
          # Attempt full conversion
          full_parsed <- suppressWarnings(as.Date(df[[col]]))
          if (sum(!is.na(full_parsed)) >= 0.8 * length(valid_vals)) {
            df[[col]] <- full_parsed
            warnings_list <- c(warnings_list, paste0("Column '", col, "' detected and parsed as Date."))
          }
        }
      }
    }
  }

  file_size_bytes <- file.info(file_path)$size

  metadata <- list(
    file_name = basename(file_path),
    file_size_bytes = file_size_bytes,
    file_size_formatted = format_bytes(file_size_bytes),
    n_rows = nrow(df),
    n_cols = ncol(df),
    original_column_names = orig_names,
    repaired_column_names = clean_col_names
  )

  return(list(
    success = TRUE,
    error = NULL,
    data = df,
    raw_data = raw_df,
    warnings = warnings_list,
    metadata = metadata
  ))
}
