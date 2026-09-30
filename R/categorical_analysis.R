# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/categorical_analysis.R
# Description: Categorical profiling, frequency tables, concentration & rare classes
# ==============================================================================

#' Analyze categorical variables across a dataset
#' @param df Data frame
#' @param concentration_threshold Proportion threshold for category concentration (default 0.90)
#' @return A list with summary across all categorical columns and detailed frequency helpers
analyze_categorical <- function(df, concentration_threshold = 0.90) {
  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]

  if (length(cat_cols) == 0 || nrow(df) == 0) {
    return(list(
      summary = data.frame(),
      has_categorical = FALSE,
      categorical_columns = character(0)
    ))
  }

  n_rows <- nrow(df)
  summary_rows <- list()

  for (col in cat_cols) {
    vals <- df[[col]]
    non_na <- vals[!is.na(vals)]
    if (is.character(non_na)) {
      non_na <- non_na[trimws(non_na) != ""]
    }
    n_valid <- length(non_na)

    if (n_valid == 0) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        column = col,
        n_categories = 0,
        dominant_category = "ALL MISSING",
        dominant_count = 0,
        dominant_pct = 0,
        rare_categories_count = 0,
        has_concentration = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }

    tab <- table(non_na)
    n_cat <- length(tab)
    dom_count <- max(tab)
    dom_cat <- names(tab)[which.max(tab)]
    dom_pct <- round((dom_count / n_rows) * 100, 2)

    # Rare categories: < 1% of valid records or count < 5
    rare_thresh_count <- max(5, floor(0.01 * n_valid))
    n_rare <- sum(tab < rare_thresh_count)

    has_conc <- dom_pct >= (concentration_threshold * 100)

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      column = col,
      n_categories = n_cat,
      dominant_category = dom_cat,
      dominant_count = dom_count,
      dominant_pct = dom_pct,
      rare_categories_count = n_rare,
      has_concentration = has_conc,
      stringsAsFactors = FALSE
    )
  }

  summary_df <- if (length(summary_rows) > 0) {
    do.call(rbind, summary_rows)
  } else {
    data.frame()
  }

  return(list(
    summary = summary_df,
    has_categorical = TRUE,
    categorical_columns = cat_cols,
    concentration_threshold = concentration_threshold,
    wording_note = "High concentration in one category is described as 'Category concentration'. It is not automatically labeled as bias."
  ))
}

#' Get detailed frequency table for a selected categorical column
#' @param df Dataset
#' @param col Column name
#' @param max_categories Cap on distinct categories shown before grouping (default 30)
#' @return Data frame of frequencies, percentages, and cumulative percentages
get_category_frequencies <- function(df, col, max_categories = 30) {
  if (is.null(df[[col]])) return(data.frame())

  vals <- df[[col]]
  non_na <- vals[!is.na(vals)]
  if (is.character(non_na)) {
    non_na <- non_na[trimws(non_na) != ""]
  }
  n_total <- length(non_na)

  if (n_total == 0) {
    return(data.frame(
      Category = "No non-missing values",
      Frequency = 0,
      Percentage = 0,
      Cumulative_Pct = 0,
      stringsAsFactors = FALSE
    ))
  }

  tab <- sort(table(non_na), decreasing = TRUE)

  if (length(tab) > max_categories) {
    top_tab <- tab[seq_len(max_categories)]
    other_count <- sum(tab[(max_categories + 1):length(tab)])
    categories <- c(names(top_tab), paste0("Other (", length(tab) - max_categories, " categories)"))
    counts <- c(as.numeric(top_tab), other_count)
  } else {
    categories <- names(tab)
    counts <- as.numeric(tab)
  }

  pcts <- round((counts / n_total) * 100, 2)
  cum_pcts <- cumsum(pcts)

  data.frame(
    Category = categories,
    Frequency = counts,
    Percentage = pcts,
    Cumulative_Pct = pmin(100, cum_pcts),
    stringsAsFactors = FALSE
  )
}
