# ==============================================================================
# DATA DETECTIVE PRO: Feature Engineering & Enrichment Engine
# File: R/feature_engine.R
# Description: Transform raw variables into modeling-ready, informative feature sets
# Shinylive / WebAssembly compatible (Pure Base R + stats)
# ==============================================================================

# ---- 1. Date & Temporal Feature Extraction ----------------------------------

#' Extract rich temporal features from date or datetime variables
#' @param df Data frame
#' @param col Target date column name
#' @return List with success, data, new_columns, message
derive_date_features <- function(df, col) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Invalid dataset or column not found."))
  }

  vec <- df[[col]]
  d_parsed <- suppressWarnings(as.Date(as.character(vec)))
  
  valid_count <- sum(!is.na(d_parsed))
  if (valid_count == 0) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = paste0("Column '", col, "' could not be parsed as Date.")))
  }

  res_df <- df
  col_clean <- gsub("[^[:alnum:]_]", "_", col)
  
  y_col <- paste0(col_clean, "_year")
  m_col <- paste0(col_clean, "_month")
  q_col <- paste0(col_clean, "_quarter")
  d_col <- paste0(col_clean, "_day")
  dow_col <- paste0(col_clean, "_day_of_week")
  wknd_col <- paste0(col_clean, "_is_weekend")

  m_int <- as.integer(format(d_parsed, "%m"))

  res_df[[y_col]] <- as.integer(format(d_parsed, "%Y"))
  res_df[[m_col]] <- m_int
  res_df[[q_col]] <- as.integer((m_int - 1) %/% 3 + 1)
  res_df[[d_col]] <- as.integer(format(d_parsed, "%d"))
  res_df[[dow_col]] <- weekdays(d_parsed)
  res_df[[wknd_col]] <- as.integer(weekdays(d_parsed) %in% c("Saturday", "Sunday"))

  new_cols <- c(y_col, m_col, q_col, d_col, dow_col, wknd_col)

  list(
    success = TRUE,
    data = res_df,
    new_columns = new_cols,
    message = paste0("Derived 6 temporal features from '", col, "': Year, Month, Quarter, Day, DayOfWeek, IsWeekend.")
  )
}

# ---- 2. Logarithmic & Skew Normalization -------------------------------------

#' Apply log(1 + x) transformation to continuous variables with skewness
#' @param df Data frame
#' @param col Numeric column name
#' @return List with success, data, new_columns, message
derive_log_transform <- function(df, col) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Column not found."))
  }

  vec <- df[[col]]
  if (!is.numeric(vec)) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Log transformation requires a numeric column."))
  }

  clean_v <- vec[!is.na(vec) & is.finite(vec)]
  min_v <- if (length(clean_v) > 0) min(clean_v) else 0

  res_df <- df
  new_col_name <- paste0(col, "_log")

  if (min_v < 0) {
    # Shift so minimum value is 0 before log1p
    shift <- abs(min_v)
    res_df[[new_col_name]] <- log1p(vec + shift)
    msg <- paste0("Applied shifted log1p transformation to '", col, "' (shift offset = +", round(shift, 2), ").")
  } else {
    res_df[[new_col_name]] <- log1p(vec)
    msg <- paste0("Applied log1p transformation to '", col, "'.")
  }

  list(
    success = TRUE,
    data = res_df,
    new_columns = new_col_name,
    message = msg
  )
}

# ---- 3. Statistical Scaling & Normalization ---------------------------------

#' Standardize or normalize continuous variables
#' @param df Data frame
#' @param col Numeric column name
#' @param method 'standardize_z' (z-score), 'min_max' (0 to 1), 'robust_iqr'
#' @return List with success, data, new_columns, message
derive_scaled_features <- function(df, col, method = "standardize_z") {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Column not found."))
  }

  vec <- df[[col]]
  if (!is.numeric(vec)) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Scaling requires a numeric column."))
  }

  clean_v <- vec[!is.na(vec) & is.finite(vec)]
  if (length(clean_v) < 2) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Insufficient values for scaling."))
  }

  res_df <- df
  new_col_name <- paste0(col, "_", switch(method, "standardize_z" = "zscore", "min_max" = "scaled", "robust_iqr" = "robust"))

  if (method == "standardize_z") {
    m <- mean(clean_v)
    s <- stats::sd(clean_v)
    res_df[[new_col_name]] <- if (s > 0) round((vec - m) / s, 4) else 0
    msg <- paste0("Derived Z-score standardized feature '", new_col_name, "' (mean = 0, sd = 1).")
  } else if (method == "min_max") {
    min_v <- min(clean_v)
    max_v <- max(clean_v)
    rng <- max_v - min_v
    res_df[[new_col_name]] <- if (rng > 0) round((vec - min_v) / rng, 4) else 0
    msg <- paste0("Derived Min-Max normalized feature '", new_col_name, "' bounded in [0, 1].")
  } else if (method == "robust_iqr") {
    med_v <- stats::median(clean_v)
    iqr_v <- stats::IQR(clean_v)
    res_df[[new_col_name]] <- if (iqr_v > 0) round((vec - med_v) / iqr_v, 4) else 0
    msg <- paste0("Derived Robust IQR scaled feature '", new_col_name, "' (median = 0, IQR = 1).")
  } else {
    return(list(success = FALSE, data = df, new_columns = character(0), message = paste0("Unsupported scaling method: ", method)))
  }

  list(
    success = TRUE,
    data = res_df,
    new_columns = new_col_name,
    message = msg
  )
}

# ---- 4. Interaction & Ratio Features ----------------------------------------

#' Compute business ratio features between two numeric columns
#' @param df Data frame
#' @param num_col_a Numerator column
#' @param num_col_b Denominator column
#' @param new_name Optional custom name
#' @return List with success, data, new_columns, message
derive_ratio_features <- function(df, num_col_a, num_col_b, new_name = NULL) {
  if (is.null(df) || !(num_col_a %in% names(df)) || !(num_col_b %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "One or both columns not found."))
  }

  vec_a <- df[[num_col_a]]
  vec_b <- df[[num_col_b]]

  if (!is.numeric(vec_a) || !is.numeric(vec_b)) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Ratio calculation requires two numeric columns."))
  }

  res_df <- df
  out_name <- if (!is.null(new_name) && nzchar(trimws(new_name))) trimws(new_name) else paste0(num_col_a, "_to_", num_col_b)

  # Safe division (zero denominator -> NA)
  ratio_v <- ifelse(!is.na(vec_b) & vec_b != 0, round(vec_a / vec_b, 4), NA_real_)
  res_df[[out_name]] <- ratio_v

  list(
    success = TRUE,
    data = res_df,
    new_columns = out_name,
    message = paste0("Created business ratio feature '", out_name, "' (", num_col_a, " / ", num_col_b, ").")
  )
}

# ---- 5. Quantile Binning & Discretization -----------------------------------

#' Discretize continuous metric into informative quantile tiers
#' @param df Data frame
#' @param col Numeric column name
#' @param n_bins Number of quantile bins (default 4 for quartiles)
#' @param labels Custom character labels (optional)
#' @return List with success, data, new_columns, message
derive_binned_features <- function(df, col, n_bins = 4, labels = NULL) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Column not found."))
  }

  vec <- df[[col]]
  if (!is.numeric(vec)) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Binning requires a numeric column."))
  }

  clean_v <- vec[!is.na(vec) & is.finite(vec)]
  if (length(unique(clean_v)) < n_bins) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Insufficient distinct values for requested bin count."))
  }

  probs <- seq(0, 1, length.out = n_bins + 1)
  breaks <- stats::quantile(clean_v, probs = probs, na.rm = TRUE)
  breaks <- unique(breaks) # avoid identical breaks

  if (length(breaks) < 2) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Cannot compute distinct quantile break points."))
  }

  default_labels <- if (n_bins == 4) c("Low (Q1)", "Moderate (Q2)", "High (Q3)", "Very High (Q4)") else paste0("Tier_", seq_len(length(breaks) - 1))
  bin_labels <- if (!is.null(labels) && length(labels) == (length(breaks) - 1)) labels else default_labels[seq_len(length(breaks) - 1)]

  res_df <- df
  new_col_name <- paste0(col, "_tier")
  res_df[[new_col_name]] <- as.character(cut(vec, breaks = breaks, labels = bin_labels, include.lowest = TRUE))

  list(
    success = TRUE,
    data = res_df,
    new_columns = new_col_name,
    message = paste0("Binned '", col, "' into ", length(bin_labels), " quantile tiers: '", new_col_name, "'.")
  )
}

# ---- 6. One-Hot Indicator Encoding ------------------------------------------

#' Generate binary indicator columns for top category levels
#' @param df Data frame
#' @param col Categorical column
#' @param max_levels Maximum top distinct levels to encode (default 6)
#' @return List with success, data, new_columns, message
derive_one_hot_encoding <- function(df, col, max_levels = 6) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Column not found."))
  }

  vec <- as.character(df[[col]])
  tab <- sort(table(vec[!is.na(vec)]), decreasing = TRUE)
  top_levels <- head(names(tab), max_levels)

  if (length(top_levels) == 0) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "No valid levels to encode."))
  }

  res_df <- df
  new_cols <- character(0)

  for (lvl in top_levels) {
    safe_lvl <- gsub("[^[:alnum:]_]", "", gsub("[[:space:]]+", "_", lvl))
    dummy_name <- paste0("is_", col, "_", tolower(safe_lvl))
    res_df[[dummy_name]] <- as.integer(!is.na(vec) & vec == lvl)
    new_cols <- c(new_cols, dummy_name)
  }

  list(
    success = TRUE,
    data = res_df,
    new_columns = new_cols,
    message = paste0("Derived ", length(new_cols), " one-hot binary indicator columns for '", col, "'.")
  )
}

# ---- 7. Cyclical Sine / Cosine Encoding ------------------------------------

#' Derive cyclical Sine and Cosine encoding for periodic features (months, days of week, hours)
#' @param df Data frame
#' @param col Numeric column name
#' @param period Cycle period (e.g. 12 for months, 7 for days of week, 24 for hours)
#' @return List with success, data, new_columns, message
derive_cyclical_features <- function(df, col, period = 12) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Column not found."))
  }

  vec <- df[[col]]
  if (!is.numeric(vec)) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Cyclical encoding requires a numeric column."))
  }

  res_df <- df
  col_clean <- gsub("[^[:alnum:]_]", "_", col)
  sin_col <- paste0(col_clean, "_sin")
  cos_col <- paste0(col_clean, "_cos")

  res_df[[sin_col]] <- round(sin(2 * pi * vec / period), 4)
  res_df[[cos_col]] <- round(cos(2 * pi * vec / period), 4)

  list(
    success = TRUE,
    data = res_df,
    new_columns = c(sin_col, cos_col),
    message = paste0("Derived cyclical Sine and Cosine encodings for '", col, "' (period = ", period, ").")
  )
}

# ---- 8. Text Metadata Extraction --------------------------------------------

#' Extract rich text metadata features: character length and word count
#' @param df Data frame
#' @param col Character column name
#' @return List with success, data, new_columns, message
derive_text_features <- function(df, col) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, new_columns = character(0), message = "Column not found."))
  }

  vec <- as.character(df[[col]])
  res_df <- df
  col_clean <- gsub("[^[:alnum:]_]", "_", col)

  len_col <- paste0(col_clean, "_char_len")
  wc_col  <- paste0(col_clean, "_word_count")

  char_lens <- vapply(vec, function(s) if (is.na(s)) NA_integer_ else nchar(s), integer(1))
  word_counts <- vapply(vec, function(s) {
    if (is.na(s) || trimws(s) == "") return(0L)
    length(unlist(strsplit(trimws(s), "[[:space:]]+")))
  }, integer(1))

  res_df[[len_col]] <- char_lens
  res_df[[wc_col]]  <- word_counts

  list(
    success = TRUE,
    data = res_df,
    new_columns = c(len_col, wc_col),
    message = paste0("Derived text metadata features for '", col, "': character length and word count.")
  )
}

