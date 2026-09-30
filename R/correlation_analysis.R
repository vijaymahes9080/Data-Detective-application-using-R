# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/correlation_analysis.R
# Description: Pearson correlation matrix, pairwise strength flags, multicollinearity
# ==============================================================================

#' Compute correlation analysis across numeric features
#' @param df Data frame
#' @param strong_thresh Threshold for strong correlation (default 0.70)
#' @param very_strong_thresh Threshold for very strong correlation (default 0.90)
#' @return A list with matrix, pairwise table, multicollinear pairs, and notes
analyze_correlations <- function(df, strong_thresh = 0.70, very_strong_thresh = 0.90) {
  # Select numeric columns with non-zero variance and at least 4 valid rows
  num_cols <- names(df)[vapply(df, function(x) is.numeric(x) && !is.logical(x), logical(1))]

  valid_cols <- character(0)
  for (col in num_cols) {
    clean_v <- df[[col]][!is.na(df[[col]]) & is.finite(df[[col]])]
    if (length(clean_v) >= 4) {
      v <- stats::var(clean_v)
      if (!is.na(v) && v > 1e-9) {
        valid_cols <- c(valid_cols, col)
      }
    }
  }

  if (length(valid_cols) < 2) {
    return(list(
      has_correlations = FALSE,
      message = "At least two varying numeric columns with >= 4 observations are required.",
      matrix = matrix(numeric(0)),
      pairs = data.frame(),
      multicollinear_pairs = data.frame(),
      numeric_columns = valid_cols
    ))
  }

  sub_df <- df[, valid_cols, drop = FALSE]

  # Compute pairwise Pearson correlation using pairwise.complete.obs
  corr_mat <- suppressWarnings(stats::cor(sub_df, use = "pairwise.complete.obs", method = "pearson"))
  # Round matrix
  corr_mat_rounded <- round(corr_mat, 3)

  # Build pairwise table (upper triangle)
  n <- length(valid_cols)
  pair_rows <- list()

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      r_val <- corr_mat[i, j]
      var_a <- valid_cols[i]
      var_b <- valid_cols[j]

      if (is.na(r_val) || is.nan(r_val)) {
        next
      }

      abs_r <- abs(r_val)
      strength <- if (abs_r >= very_strong_thresh) {
        "Very Strong Correlation Detected"
      } else if (abs_r >= strong_thresh) {
        "Strong Correlation Detected"
      } else if (abs_r >= 0.40) {
        "Moderate Association"
      } else if (abs_r >= 0.20) {
        "Weak Association"
      } else {
        "Negligible Association"
      }

      note <- if (abs_r >= very_strong_thresh) {
        "These variables contain highly overlapping linear information. Check for redundancy or potential leakage."
      } else if (abs_r >= strong_thresh) {
        "Substantial linear co-movement observed. Worth checking if one variable approximates the other."
      } else {
        "Variables exhibit limited linear dependency."
      }

      pair_rows[[length(pair_rows) + 1]] <- data.frame(
        variable_a = var_a,
        variable_b = var_b,
        pearson_r = round(r_val, 3),
        abs_r = round(abs_r, 3),
        strength = strength,
        investigation_note = note,
        is_strong = (abs_r >= strong_thresh),
        is_very_strong = (abs_r >= very_strong_thresh),
        stringsAsFactors = FALSE
      )
    }
  }

  pairs_df <- if (length(pair_rows) > 0) {
    do.call(rbind, pair_rows)
  } else {
    data.frame()
  }

  if (nrow(pairs_df) > 0) {
    pairs_df <- pairs_df[order(-pairs_df$abs_r), ]
    rownames(pairs_df) <- NULL
  }

  # Multicollinear pairs
  multi_pairs <- pairs_df[pairs_df$abs_r >= strong_thresh, , drop = FALSE]

  return(list(
    has_correlations = TRUE,
    matrix = corr_mat_rounded,
    pairs = pairs_df,
    multicollinear_pairs = multi_pairs,
    numeric_columns = valid_cols,
    strong_threshold = strong_thresh,
    very_strong_threshold = very_strong_thresh,
    wording_note = "Correlation indicates statistical association, not causation. Data Detective does not automatically remove variables."
  ))
}
