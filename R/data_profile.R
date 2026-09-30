# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/data_profile.R
# Description: Automated dataset profiling and variable characterization
# ==============================================================================

#' Profile an entire dataset
#' @param df Data frame to profile
#' @return A list with dataset dimensions, column types, and per-column metrics
profile_dataset <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      n_rows = 0,
      n_cols = 0,
      total_cells = 0,
      memory_bytes = 0,
      memory_formatted = "0 B",
      type_counts = list(),
      columns_summary = data.frame()
    ))
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  total_cells <- n_rows * n_cols
  mem_bytes <- as.numeric(utils::object.size(df))

  col_names <- names(df)
  col_types <- vapply(df, detect_column_type, character(1))

  # Build column-by-column profile
  profile_rows <- vector("list", n_cols)

  for (i in seq_along(col_names)) {
    cname <- col_names[i]
    col_data <- df[[i]]
    ctype <- col_types[i]

    # Missing counts
    if (is.character(col_data)) {
      n_missing <- sum(is.na(col_data) | trimws(col_data) == "")
    } else {
      n_missing <- sum(is.na(col_data) | is.nan(col_data))
    }
    missing_pct <- n_missing / n_rows

    # Unique counts
    non_na <- col_data[!is.na(col_data)]
    if (is.character(non_na)) {
      non_na <- non_na[trimws(non_na) != ""]
    }
    n_unique <- length(unique(non_na))
    unique_pct <- ifelse(n_rows > 0, n_unique / n_rows, 0)

    # Statistical summaries
    val_min <- NA_character_
    val_max <- NA_character_
    val_mean <- NA_real_
    val_median <- NA_real_
    val_sd <- NA_real_
    val_skew <- NA_real_

    if (ctype %in% c("numeric", "integer")) {
      num_clean <- col_data[!is.na(col_data) & is.finite(col_data)]
      if (length(num_clean) > 0) {
        val_min <- as.character(round(min(num_clean), 3))
        val_max <- as.character(round(max(num_clean), 3))
        val_mean <- round(safe_mean(num_clean), 3)
        val_median <- round(safe_median(num_clean), 3)
        val_sd <- round(safe_sd(num_clean), 3)
        val_skew <- round(safe_skewness(num_clean), 3)
      }
    } else if (ctype %in% c("date", "datetime")) {
      date_clean <- col_data[!is.na(col_data)]
      if (length(date_clean) > 0) {
        val_min <- as.character(min(date_clean))
        val_max <- as.character(max(date_clean))
      }
    }

    profile_rows[[i]] <- list(
      column = cname,
      data_type = ctype,
      missing_count = n_missing,
      missing_pct = missing_pct,
      non_missing_count = n_rows - n_missing,
      unique_count = n_unique,
      unique_pct = unique_pct,
      min = val_min,
      max = val_max,
      mean = val_mean,
      median = val_median,
      sd = val_sd,
      skewness = val_skew,
      is_constant = (n_unique <= 1 && n_missing == 0),
      is_all_na = (n_missing == n_rows)
    )
  }

  summary_df <- do.call(rbind, lapply(profile_rows, as.data.frame, stringsAsFactors = FALSE))

  # Aggregated type counts
  type_counts <- as.list(table(col_types))

  return(list(
    n_rows = n_rows,
    n_cols = n_cols,
    total_cells = total_cells,
    memory_bytes = mem_bytes,
    memory_formatted = format_bytes(mem_bytes),
    type_counts = type_counts,
    n_numeric = sum(col_types %in% c("numeric", "integer")),
    n_categorical = sum(col_types %in% c("character", "factor")),
    n_date = sum(col_types %in% c("date", "datetime", "date_candidate")),
    n_logical = sum(col_types == "logical"),
    columns_summary = summary_df
  ))
}
