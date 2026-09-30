# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/validation_functions.R
# Description: Input dataset validation, delimiter detection, and sanitization
# Shinylive / WebAssembly compatible
# ==============================================================================

#' Detect delimiter from the first few lines of a text/csv file
detect_delimiter <- function(file_path) {
  if (!file.exists(file_path)) return(",")
  lines <- readLines(file_path, n = 5, warn = FALSE)
  if (length(lines) == 0) return(",")

  first_line <- lines[1]
  delims <- c("," = 0, ";" = 0, "\t" = 0, "|" = 0)

  for (d in names(delims)) {
    counts <- vapply(lines, function(l) length(strsplit(l, d, fixed = TRUE)[[1]]) - 1, numeric(1))
    # Consistent count across lines preferred
    if (length(unique(counts)) == 1 && counts[1] > 0) {
      delims[d] <- counts[1] * 2
    } else {
      delims[d] <- mean(counts)
    }
  }

  best_delim <- names(which.max(delims))
  if (delims[best_delim] > 0) best_delim else ","
}

#' Validate file existence, size, and readable text format
validate_csv_file <- function(file_path) {
  res <- list(valid = TRUE, errors = character(0), warnings = character(0))

  if (is.null(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
    res$valid <- FALSE
    res$errors <- c(res$errors, "No file path provided.")
    return(res)
  }

  if (!file.exists(file_path)) {
    res$valid <- FALSE
    res$errors <- c(res$errors, "The specified file does not exist on disk.")
    return(res)
  }

  sz <- file.info(file_path)$size
  if (is.na(sz) || sz == 0) {
    res$valid <- FALSE
    res$errors <- c(res$errors, "The uploaded file is empty (0 bytes).")
    return(res)
  }

  ext <- tolower(tools::file_ext(file_path))
  if (!(ext %in% c("csv", "txt", "tsv", "rds"))) {
    res$warnings <- c(res$warnings, paste0("File extension is '.", ext, "'. Expected a standard CSV, TSV, or TXT file."))
  }

  res
}

#' Validate parsed dataset structure (rows, columns, headers)
validate_dataset <- function(df) {
  res <- list(valid = TRUE, errors = character(0), warnings = character(0))

  if (is.null(df)) {
    res$valid <- FALSE
    res$errors <- c(res$errors, "Dataset could not be loaded into memory.")
    return(res)
  }

  if (nrow(df) == 0) {
    res$valid <- FALSE
    res$errors <- c(res$errors, "Dataset contains 0 rows (empty dataset).")
    return(res)
  }

  if (ncol(df) == 0) {
    res$valid <- FALSE
    res$errors <- c(res$errors, "Dataset contains 0 columns.")
    return(res)
  }

  # Check duplicate column names
  col_names <- names(df)
  if (any(duplicated(col_names))) {
    res$warnings <- c(res$warnings, "Duplicate column headers were detected and renamed internally with sequential suffixes.")
  }

  # Check all-NA columns
  all_na_cols <- vapply(df, function(c) all(is.na(c)), logical(1))
  if (any(all_na_cols)) {
    res$warnings <- c(res$warnings, paste0("Column(s) ", paste(names(df)[all_na_cols], collapse = ", "), " contain 100% missing values."))
  }

  res
}
