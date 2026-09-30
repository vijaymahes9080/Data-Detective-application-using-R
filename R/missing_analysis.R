# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/missing_analysis.R
# Description: Detailed missing value analysis, row/column diagnostics & severity
# ==============================================================================

#' Analyze missing values across dataset
#' @param df Data frame to analyze
#' @param thresholds List with warning (low), high, and critical percentage boundaries
#' @return A list with column-level summary, row-level summary, and overall stats
analyze_missing <- function(df, thresholds = list(low = 5, moderate = 20, critical = 50)) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      total_missing = 0,
      total_cells = 0,
      missing_pct = 0,
      columns_with_missing = 0,
      column_summary = data.frame(),
      row_summary = list(),
      thresholds = thresholds
    ))
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  total_cells <- n_rows * n_cols

  # Column-by-column missing counts
  col_results <- lapply(names(df), function(col_name) {
    col_data <- df[[col_name]]
    if (is.character(col_data)) {
      n_miss <- sum(is.na(col_data) | trimws(col_data) == "")
    } else {
      n_miss <- sum(is.na(col_data) | is.nan(col_data))
    }

    miss_pct <- (n_miss / n_rows) * 100

    severity <- if (n_miss == 0) {
      "Complete"
    } else if (miss_pct <= thresholds$low) {
      "Low"
    } else if (miss_pct <= thresholds$moderate) {
      "Moderate"
    } else if (miss_pct <= thresholds$critical) {
      "High"
    } else {
      "Critical"
    }

    data.frame(
      column = col_name,
      missing_count = n_miss,
      missing_pct = round(miss_pct, 2),
      non_missing_count = n_rows - n_miss,
      severity = severity,
      stringsAsFactors = FALSE
    )
  })

  col_df <- do.call(rbind, col_results)
  # Sort descending by missing count
  col_df <- col_df[order(-col_df$missing_count), ]
  rownames(col_df) <- NULL

  total_missing <- sum(col_df$missing_count)
  overall_pct <- round((total_missing / total_cells) * 100, 2)
  cols_with_miss <- sum(col_df$missing_count > 0)

  # Row-level missingness calculation
  # Efficient matrix check
  is_missing_matrix <- is.na(df) | is.nan(as.matrix(df))
  # For character columns check whitespace
  char_cols <- which(vapply(df, is.character, logical(1)))
  for (cc in char_cols) {
    is_missing_matrix[, cc] <- is_missing_matrix[, cc] | (trimws(df[[cc]]) == "")
  }
  row_miss_counts <- rowSums(is_missing_matrix)

  row_summary <- list(
    complete_rows = sum(row_miss_counts == 0),
    complete_rows_pct = round((sum(row_miss_counts == 0) / n_rows) * 100, 2),
    rows_with_any_missing = sum(row_miss_counts > 0),
    rows_with_any_missing_pct = round((sum(row_miss_counts > 0) / n_rows) * 100, 2),
    rows_1_to_2_missing = sum(row_miss_counts >= 1 & row_miss_counts <= 2),
    rows_over_2_missing = sum(row_miss_counts > 2),
    max_missing_in_single_row = max(row_miss_counts)
  )

  return(list(
    total_missing = total_missing,
    total_cells = total_cells,
    missing_pct = overall_pct,
    columns_with_missing = cols_with_miss,
    total_columns = n_cols,
    column_summary = col_df,
    row_summary = row_summary,
    thresholds = thresholds,
    disclaimer = "Severity thresholds are heuristic and can be configured in Settings."
  ))
}
