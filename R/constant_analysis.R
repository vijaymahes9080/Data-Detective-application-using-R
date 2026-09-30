# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/constant_analysis.R
# Description: Detection of constant, near-constant, and zero-variance columns
# ==============================================================================

#' Analyze constant and low-variance columns
#' @param df Data frame to analyze
#' @param concentration_threshold Dominant value proportion threshold (default 0.90)
#' @return A list with uninformative columns table and counts
analyze_constants <- function(df, concentration_threshold = 0.90) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      uninformative_columns = data.frame(),
      constant_count = 0,
      near_constant_count = 0,
      low_variance_count = 0
    ))
  }

  n_rows <- nrow(df)
  results <- list()

  for (col in names(df)) {
    vals <- df[[col]]
    non_na <- vals[!is.na(vals)]

    if (length(non_na) == 0) {
      results[[length(results) + 1]] <- data.frame(
        column = col,
        unique_values = 0,
        dominant_value = "ALL NA",
        dominant_pct = 100.0,
        issue_type = "All Missing",
        finding = "Column contains 100% missing values.",
        label = "Potentially Uninformative Variable",
        stringsAsFactors = FALSE
      )
      next
    }

    uniq_vals <- unique(non_na)
    n_uniq <- length(uniq_vals)

    # 1. Strictly constant (1 unique value)
    if (n_uniq == 1) {
      dom_val <- as.character(uniq_vals[1])
      dom_pct <- round((length(non_na) / n_rows) * 100, 2)
      results[[length(results) + 1]] <- data.frame(
        column = col,
        unique_values = 1,
        dominant_value = dom_val,
        dominant_pct = dom_pct,
        issue_type = "Strictly Constant",
        finding = paste0("All non-missing records contain the exact same value: '", dom_val, "'."),
        label = "Potentially Uninformative Variable",
        stringsAsFactors = FALSE
      )
      next
    }

    # 2. Near-constant (dominant value >= threshold)
    freq_tab <- table(non_na)
    max_freq <- max(freq_tab)
    dom_val <- names(freq_tab)[which.max(freq_tab)]
    dom_pct <- round((max_freq / n_rows) * 100, 2)

    if (dom_pct >= (concentration_threshold * 100)) {
      results[[length(results) + 1]] <- data.frame(
        column = col,
        unique_values = n_uniq,
        dominant_value = dom_val,
        dominant_pct = dom_pct,
        issue_type = "Near-Constant",
        finding = paste0(dom_pct, "% of records contain the same value: '", dom_val, "'."),
        label = "Potentially Uninformative Variable",
        stringsAsFactors = FALSE
      )
      next
    }

    # 3. Numeric near-zero variance
    if (is.numeric(vals)) {
      num_clean <- vals[!is.na(vals) & is.finite(vals)]
      if (length(num_clean) > 5) {
        v <- stats::var(num_clean)
        s <- stats::sd(num_clean)
        m <- abs(mean(num_clean))
        if (!is.na(v) && (v < 1e-9 || (m > 1e-4 && (s / m) < 1e-5))) {
          results[[length(results) + 1]] <- data.frame(
            column = col,
            unique_values = n_uniq,
            dominant_value = paste0("Var = ", formatC(v, format = "e", digits = 2)),
            dominant_pct = dom_pct,
            issue_type = "Near-Zero Variance",
            finding = "Numeric variability is virtually zero across observations.",
            label = "Potentially Uninformative Variable",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  summary_df <- if (length(results) > 0) {
    do.call(rbind, results)
  } else {
    data.frame(
      column = character(0),
      unique_values = integer(0),
      dominant_value = character(0),
      dominant_pct = numeric(0),
      issue_type = character(0),
      finding = character(0),
      label = character(0),
      stringsAsFactors = FALSE
    )
  }

  return(list(
    uninformative_columns = summary_df,
    constant_count = sum(summary_df$issue_type == "Strictly Constant"),
    near_constant_count = sum(summary_df$issue_type == "Near-Constant"),
    low_variance_count = sum(summary_df$issue_type == "Near-Zero Variance"),
    disclaimer = "Variables flagged as uninformative lack variability. Data Detective does not automatically remove columns."
  ))
}
