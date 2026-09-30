# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/outlier_analysis.R
# Description: IQR and Z-score outlier detection, bounds, and observation profiling
# ==============================================================================

#' Analyze statistical outliers across all numeric variables
#' @param df Data frame to analyze
#' @param iqr_multiplier Multiplier for IQR bounds (default 1.5)
#' @param z_threshold Threshold for standard score method (default 3.0)
#' @return A list containing overall summary table, per-column details, and methods
analyze_outliers <- function(df, iqr_multiplier = 1.5, z_threshold = 3.0) {
  num_cols <- names(df)[vapply(df, function(x) is.numeric(x) && !is.logical(x), logical(1))]

  if (length(num_cols) == 0 || nrow(df) == 0) {
    return(list(
      summary = data.frame(),
      has_numeric = FALSE,
      iqr_multiplier = iqr_multiplier,
      z_threshold = z_threshold,
      total_outlier_variables = 0
    ))
  }

  summary_rows <- list()
  details_list <- list()

  for (col in num_cols) {
    vals <- df[[col]]
    valid_idx <- which(!is.na(vals) & is.finite(vals))
    valid_vals <- vals[valid_idx]
    n_valid <- length(valid_vals)

    if (n_valid < 4) {
      next
    }

    # IQR Method
    q <- stats::quantile(valid_vals, probs = c(0.25, 0.75), na.rm = TRUE)
    q1 <- as.numeric(q[1])
    q3 <- as.numeric(q[2])
    iqr_val <- q3 - q1

    lower_bound <- q1 - (iqr_multiplier * iqr_val)
    upper_bound <- q3 + (iqr_multiplier * iqr_val)

    # Detect observations
    is_lower <- valid_vals < lower_bound
    is_upper <- valid_vals > upper_bound
    is_iqr_outlier <- is_lower | is_upper
    n_iqr_outliers <- sum(is_iqr_outlier)
    iqr_pct <- round((n_iqr_outliers / n_valid) * 100, 2)

    # Z-Score Method
    m <- mean(valid_vals)
    s <- stats::sd(valid_vals)
    n_z_outliers <- 0
    z_pct <- 0
    if (!is.na(s) && s > 0) {
      z_scores <- (valid_vals - m) / s
      is_z_outlier <- abs(z_scores) > z_threshold
      n_z_outliers <- sum(is_z_outlier)
      z_pct <- round((n_z_outliers / n_valid) * 100, 2)
    }

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      column = col,
      n_valid = n_valid,
      q1 = round(q1, 3),
      q3 = round(q3, 3),
      iqr = round(iqr_val, 3),
      lower_bound = round(lower_bound, 3),
      upper_bound = round(upper_bound, 3),
      outlier_count = n_iqr_outliers,
      outlier_pct = iqr_pct,
      z_outlier_count = n_z_outliers,
      z_outlier_pct = z_pct,
      stringsAsFactors = FALSE
    )

    # Save detailed indices and values
    outlier_row_ids <- valid_idx[is_iqr_outlier]
    details_list[[col]] <- list(
      q1 = q1,
      q3 = q3,
      iqr = iqr_val,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      outlier_indices = outlier_row_ids,
      outlier_values = vals[outlier_row_ids],
      is_lower_count = sum(is_lower),
      is_upper_count = sum(is_upper)
    )
  }

  summary_df <- if (length(summary_rows) > 0) {
    do.call(rbind, summary_rows)
  } else {
    data.frame()
  }

  if (nrow(summary_df) > 0) {
    summary_df <- summary_df[order(-summary_df$outlier_count), ]
    rownames(summary_df) <- NULL
  }

  return(list(
    summary = summary_df,
    details = details_list,
    has_numeric = TRUE,
    numeric_columns = num_cols,
    total_outlier_variables = sum(summary_df$outlier_count > 0),
    iqr_multiplier = iqr_multiplier,
    z_threshold = z_threshold,
    wording_note = "An outlier is a statistically unusual observation, not automatically an error."
  ))
}

#' Extract rows containing outliers for a specific numeric column
#' @param df Dataset
#' @param outlier_info Output from analyze_outliers
#' @param col Column name
#' @return Data frame of unusual observations
get_column_outliers <- function(df, outlier_info, col) {
  if (is.null(outlier_info$details[[col]])) return(data.frame())
  ids <- outlier_info$details[[col]]$outlier_indices
  if (length(ids) == 0) return(data.frame())
  res <- df[ids, , drop = FALSE]
  res$Row_Index <- ids
  res$Flagged_Value <- df[[col]][ids]
  # Put Row_Index and Flagged_Value first
  cols_order <- c("Row_Index", "Flagged_Value", setdiff(names(res), c("Row_Index", "Flagged_Value")))
  res[, cols_order, drop = FALSE]
}
