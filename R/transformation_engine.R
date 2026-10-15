# ==============================================================================
# DATA DETECTIVE PRO: Interactive Data Quality Transformation Engine
# File: R/transformation_engine.R
# Description: Deterministic, validated, reversible data cleaning operations
# Shinylive / WebAssembly compatible (Pure Base R + stats)
# ==============================================================================

# ---- 1. Missing Value Remediation -------------------------------------------

#' Transform missing values in a specified column
#' @param df Data frame to clean
#' @param col Target column name
#' @param method One of 'remove_rows', 'median', 'mean', 'mode', 'custom', 'forward_fill', 'backward_fill', 'grouped_median', 'indicator_col'
#' @param custom_val Constant value for 'custom' method
#' @param group_col Grouping column for 'grouped_median'
#' @return List with success (bool), data (transformed df), affected_rows (int), affected_cells (int), message (str)
transform_missing <- function(df, col, method = "median", custom_val = NULL, group_col = NULL) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Empty dataset provided."))
  }
  if (!(col %in% names(df))) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Column '", col, "' not found.")))
  }

  vec <- df[[col]]
  
  # Identify missingness index (NA, NaN, or whitespace-only for characters)
  if (is.character(vec)) {
    is_miss <- is.na(vec) | trimws(vec) == "" | tolower(trimws(vec)) %in% c("na", "null", "n/a", "none")
  } else if (is.numeric(vec)) {
    is_miss <- is.na(vec) | is.nan(vec)
  } else {
    is_miss <- is.na(vec)
  }

  n_miss <- sum(is_miss)
  if (n_miss == 0) {
    return(list(success = TRUE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Column '", col, "' has zero missing values.")))
  }

  res_df <- df
  aff_rows <- n_miss
  aff_cells <- n_miss

  if (method == "remove_rows") {
    res_df <- df[!is_miss, , drop = FALSE]
    rownames(res_df) <- NULL
    msg <- paste0("Removed ", n_miss, " row(s) containing missing values in '", col, "'.")
  } else if (method == "median") {
    if (!is.numeric(vec)) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Median imputation requires a numeric column."))
    }
    clean_vals <- vec[!is_miss & is.finite(vec)]
    if (length(clean_vals) == 0) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Cannot compute median: all values are missing."))
    }
    med_val <- stats::median(clean_vals)
    res_df[[col]][is_miss] <- med_val
    msg <- paste0("Imputed ", n_miss, " missing value(s) in '", col, "' with median (", round(med_val, 4), ").")
  } else if (method == "mean") {
    if (!is.numeric(vec)) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Mean imputation requires a numeric column."))
    }
    clean_vals <- vec[!is_miss & is.finite(vec)]
    if (length(clean_vals) == 0) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Cannot compute mean: all values are missing."))
    }
    mean_val <- mean(clean_vals)
    res_df[[col]][is_miss] <- mean_val
    msg <- paste0("Imputed ", n_miss, " missing value(s) in '", col, "' with mean (", round(mean_val, 4), ").")
  } else if (method == "mode") {
    clean_vals <- vec[!is_miss]
    if (length(clean_vals) == 0) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Cannot compute mode: all values are missing."))
    }
    tab <- table(clean_vals)
    mode_val <- names(tab)[which.max(tab)]
    if (is.numeric(vec)) mode_val <- as.numeric(mode_val)
    res_df[[col]][is_miss] <- mode_val
    msg <- paste0("Imputed ", n_miss, " missing value(s) in '", col, "' with mode ('", mode_val, "').")
  } else if (method == "custom") {
    if (is.null(custom_val) || is.na(custom_val)) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Custom replacement value cannot be empty."))
    }
    fill_val <- if (is.numeric(vec)) as.numeric(custom_val) else as.character(custom_val)
    res_df[[col]][is_miss] <- fill_val
    msg <- paste0("Imputed ", n_miss, " missing value(s) in '", col, "' with custom constant ('", fill_val, "').")
  } else if (method == "forward_fill") {
    new_vec <- vec
    last_val <- NA
    for (idx in seq_along(new_vec)) {
      if (is_miss[idx]) {
        if (!is.na(last_val)) new_vec[idx] <- last_val
      } else {
        last_val <- new_vec[idx]
      }
    }
    res_df[[col]] <- new_vec
    msg <- paste0("Forward filled missing values in '", col, "'.")
  } else if (method == "backward_fill") {
    new_vec <- vec
    next_val <- NA
    for (idx in rev(seq_along(new_vec))) {
      if (is_miss[idx]) {
        if (!is.na(next_val)) new_vec[idx] <- next_val
      } else {
        next_val <- new_vec[idx]
      }
    }
    res_df[[col]] <- new_vec
    msg <- paste0("Backward filled missing values in '", col, "'.")
  } else if (method == "grouped_median") {
    if (!is.numeric(vec) || is.null(group_col) || !(group_col %in% names(df))) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Grouped median requires a numeric column and a valid grouping variable."))
    }
    g_vec <- df[[group_col]]
    new_vec <- vec
    for (g in unique(g_vec)) {
      g_idx <- which(g_vec == g & !is.na(g_vec))
      g_clean <- vec[g_idx][!is.na(vec[g_idx])]
      if (length(g_clean) > 0) {
        g_med <- stats::median(g_clean)
        miss_in_g <- g_idx[is.na(new_vec[g_idx])]
        if (length(miss_in_g) > 0) new_vec[miss_in_g] <- g_med
      }
    }
    # Fallback for remaining NAs (groups with no non-NA)
    rem_miss <- is.na(new_vec)
    if (any(rem_miss)) {
      overall_med <- stats::median(vec[!is_miss], na.rm = TRUE)
      new_vec[rem_miss] <- overall_med
    }
    res_df[[col]] <- new_vec
    msg <- paste0("Imputed missing values in '", col, "' using grouped medians by '", group_col, "'.")
  } else if (method == "indicator_col") {
    ind_col_name <- paste0(col, "_is_missing")
    if (ind_col_name %in% names(res_df)) {
      ind_col_name <- make.unique(c(names(res_df), ind_col_name))[ncol(res_df) + 1]
    }
    res_df[[ind_col_name]] <- as.integer(is_miss)
    aff_cells <- nrow(res_df)
    msg <- paste0("Created missingness indicator column '", ind_col_name, "'. Original column left intact.")
  } else {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Unsupported missing value method: ", method)))
  }

  list(
    success = TRUE,
    data = res_df,
    affected_rows = aff_rows,
    affected_cells = aff_cells,
    message = msg
  )
}

# ---- 2. Duplicate Data Remediation ------------------------------------------

#' Deduplicate records exactly or based on key columns
#' @param df Data frame to deduplicate
#' @param key_cols Vector of column names for key-based deduplication (or NULL for all columns)
#' @param keep_rule 'first', 'last', or 'fewest_na'
#' @return List with success, data, affected_rows, affected_cells, message
transform_duplicates <- function(df, key_cols = NULL, keep_rule = "first") {
  if (is.null(df) || nrow(df) <= 1) {
    return(list(success = TRUE, data = df, affected_rows = 0, affected_cells = 0, message = "No duplicate removal needed on dataset."))
  }

  n_init <- nrow(df)
  sub_df <- if (!is.null(key_cols) && length(key_cols) > 0 && all(key_cols %in% names(df))) {
    df[, key_cols, drop = FALSE]
  } else {
    df
  }

  dup_flags <- duplicated(sub_df)
  if (!any(dup_flags)) {
    return(list(success = TRUE, data = df, affected_rows = 0, affected_cells = 0, message = "Zero duplicate rows found under the selected criteria."))
  }

  res_df <- df
  if (keep_rule == "first") {
    res_df <- df[!duplicated(sub_df), , drop = FALSE]
  } else if (keep_rule == "last") {
    res_df <- df[!duplicated(sub_df, fromLast = TRUE), , drop = FALSE]
  } else if (keep_rule == "fewest_na") {
    # Compute missing count per row
    na_per_row <- rowSums(is.na(df))
    # Order by key columns and then ascending missingness
    ord <- do.call(order, c(as.list(sub_df), list(na_per_row)))
    df_sorted <- df[ord, , drop = FALSE]
    sub_sorted <- sub_df[ord, , drop = FALSE]
    res_df <- df_sorted[!duplicated(sub_sorted), , drop = FALSE]
  } else {
    res_df <- df[!duplicated(sub_df), , drop = FALSE]
  }

  rownames(res_df) <- NULL
  n_removed <- n_init - nrow(res_df)

  list(
    success = TRUE,
    data = res_df,
    affected_rows = n_removed,
    affected_cells = n_removed * ncol(df),
    message = paste0("Removed ", n_removed, " duplicate record(s) keeping '", keep_rule, "' instances.")
  )
}

# ---- 3. Outlier and Anomaly Treatment ----------------------------------------

#' Detect bounds and treat statistical outliers in a numeric column
#' @param df Data frame
#' @param col Numeric column name
#' @param method 'iqr_1.5', 'iqr_3.0', 'zscore_3.0', 'custom_bounds'
#' @param action 'cap_bounds' (Winsorize), 'remove_rows', 'replace_median', 'replace_custom'
#' @param threshold Multiplier for IQR or Z-score (default 1.5)
#' @param custom_bounds Vector c(lower, upper) if method == 'custom_bounds'
#' @param custom_val Value for 'replace_custom'
#' @return List with success, data, affected_rows, affected_cells, message, bounds
transform_outliers <- function(df, col, method = "iqr_1.5", action = "cap_bounds",
                               threshold = 1.5, custom_bounds = NULL, custom_val = NULL) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Invalid input or column not found."))
  }

  vec <- df[[col]]
  if (!is.numeric(vec)) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Outlier treatment is only applicable to numeric columns."))
  }

  clean_vec <- vec[!is.na(vec) & is.finite(vec)]
  if (length(clean_vec) < 4) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Insufficient observations to evaluate outliers reliably."))
  }

  lower_b <- -Inf
  upper_b <- Inf

  if (method %in% c("iqr_1.5", "iqr_3.0")) {
    mult <- if (method == "iqr_3.0") 3.0 else as.numeric(threshold)
    qs <- stats::quantile(clean_vec, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr_val <- qs[2] - qs[1]
    lower_b <- as.numeric(qs[1] - mult * iqr_val)
    upper_b <- as.numeric(qs[2] + mult * iqr_val)
  } else if (method == "zscore_3.0") {
    m <- mean(clean_vec)
    s <- stats::sd(clean_vec)
    mult <- as.numeric(threshold)
    lower_b <- m - mult * s
    upper_b <- m + mult * s
  } else if (method == "custom_bounds") {
    if (is.null(custom_bounds) || length(custom_bounds) < 2) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Custom bounds must contain two numeric thresholds: c(lower, upper)."))
    }
    lower_b <- as.numeric(custom_bounds[1])
    upper_b <- as.numeric(custom_bounds[2])
  }

  out_idx <- which(!is.na(vec) & (vec < lower_b | vec > upper_b))
  n_out <- length(out_idx)

  if (n_out == 0) {
    return(list(
      success = TRUE, data = df, affected_rows = 0, affected_cells = 0,
      message = paste0("Zero observations in '", col, "' lie outside bounds [", round(lower_b, 3), ", ", round(upper_b, 3), "]."),
      bounds = c(lower = lower_b, upper = upper_b)
    ))
  }

  res_df <- df
  aff_rows <- n_out
  aff_cells <- n_out

  if (action == "cap_bounds") {
    new_vec <- vec
    new_vec[!is.na(new_vec) & new_vec < lower_b] <- lower_b
    new_vec[!is.na(new_vec) & new_vec > upper_b] <- upper_b
    res_df[[col]] <- new_vec
    msg <- paste0("Winsorized/capped ", n_out, " outlier(s) in '", col, "' to bounds [", round(lower_b, 3), ", ", round(upper_b, 3), "].")
  } else if (action == "remove_rows") {
    res_df <- df[-out_idx, , drop = FALSE]
    rownames(res_df) <- NULL
    msg <- paste0("Filtered ", n_out, " row(s) flagged as statistical outliers in '", col, "'.")
  } else if (action == "replace_median") {
    med_val <- stats::median(clean_vec)
    new_vec <- vec
    new_vec[out_idx] <- med_val
    res_df[[col]] <- new_vec
    msg <- paste0("Replaced ", n_out, " outlier observation(s) in '", col, "' with median (", round(med_val, 3), ").")
  } else if (action == "replace_custom") {
    c_val <- as.numeric(custom_val)
    new_vec <- vec
    new_vec[out_idx] <- c_val
    res_df[[col]] <- new_vec
    msg <- paste0("Replaced ", n_out, " outlier observation(s) in '", col, "' with custom value (", c_val, ").")
  } else {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Unsupported outlier action: ", action)))
  }

  list(
    success = TRUE,
    data = res_df,
    affected_rows = aff_rows,
    affected_cells = aff_cells,
    message = msg,
    bounds = c(lower = lower_b, upper = upper_b)
  )
}

# ---- 4. Data Type Conversion & Parsing ---------------------------------------

#' Safely convert column data types with failure inspection
#' @param df Data frame
#' @param col Column name
#' @param target_type 'numeric', 'integer', 'character', 'factor', 'date', 'logical'
#' @param date_format Format string for date parsing (e.g., '%Y-%m-%d', '%d/%m/%Y', '%m/%d/%Y')
#' @return List with success, data, affected_rows, affected_cells, n_failed, failed_rows, message
transform_types <- function(df, col, target_type = "numeric", date_format = "%Y-%m-%d") {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, n_failed = 0, failed_rows = integer(0), message = "Invalid input or column not found."))
  }

  vec <- df[[col]]
  non_na_idx <- which(!is.na(vec) & trimws(as.character(vec)) != "")
  n_valid <- length(non_na_idx)

  res_df <- df
  failed_idx <- integer(0)
  converted_vec <- vec

  if (target_type == "numeric") {
    suppressWarnings({
      conv <- as.numeric(as.character(vec))
    })
    failed_idx <- which(!is.na(vec) & is.na(conv) & trimws(as.character(vec)) != "")
    if (length(failed_idx) > 0) {
      return(list(
        success = FALSE, data = df, affected_rows = 0, affected_cells = 0,
        n_failed = length(failed_idx), failed_rows = failed_idx,
        message = paste0("Type conversion failed: ", length(failed_idx), " value(s) cannot be parsed as numeric. Please clean text or characters first.")
      ))
    }
    converted_vec <- conv
  } else if (target_type == "integer") {
    suppressWarnings({
      conv <- as.integer(as.character(vec))
    })
    failed_idx <- which(!is.na(vec) & is.na(conv) & trimws(as.character(vec)) != "")
    if (length(failed_idx) > 0) {
      return(list(
        success = FALSE, data = df, affected_rows = 0, affected_cells = 0,
        n_failed = length(failed_idx), failed_rows = failed_idx,
        message = paste0("Type conversion failed: ", length(failed_idx), " value(s) cannot be parsed as integer.")
      ))
    }
    converted_vec <- conv
  } else if (target_type == "character") {
    converted_vec <- as.character(vec)
  } else if (target_type == "factor") {
    converted_vec <- as.factor(vec)
  } else if (target_type == "logical") {
    char_v <- tolower(trimws(as.character(vec)))
    conv <- rep(NA, length(vec))
    conv[char_v %in% c("true", "t", "1", "yes", "y")] <- TRUE
    conv[char_v %in% c("false", "f", "0", "no", "n")] <- FALSE
    failed_idx <- which(!is.na(vec) & is.na(conv) & char_v != "")
    if (length(failed_idx) > 0) {
      return(list(
        success = FALSE, data = df, affected_rows = 0, affected_cells = 0,
        n_failed = length(failed_idx), failed_rows = failed_idx,
        message = paste0("Type conversion failed: ", length(failed_idx), " value(s) cannot be parsed as boolean/logical.")
      ))
    }
    converted_vec <- conv
  } else if (target_type == "date") {
    char_v <- as.character(vec)
    suppressWarnings({
      conv <- as.Date(char_v, format = date_format)
    })
    failed_idx <- which(!is.na(vec) & is.na(conv) & trimws(char_v) != "")
    if (length(failed_idx) > 0) {
      return(list(
        success = FALSE, data = df, affected_rows = 0, affected_cells = 0,
        n_failed = length(failed_idx), failed_rows = failed_idx,
        message = paste0("Date parsing failed: ", length(failed_idx), " value(s) do not match format '", date_format, "'.")
      ))
    }
    converted_vec <- conv
  } else {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, n_failed = 0, failed_rows = integer(0), message = paste0("Unsupported target type: ", target_type)))
  }

  res_df[[col]] <- converted_vec

  list(
    success = TRUE,
    data = res_df,
    affected_rows = n_valid,
    affected_cells = n_valid,
    n_failed = 0,
    failed_rows = integer(0),
    message = paste0("Converted column '", col, "' to type '", target_type, "'.")
  )
}

# ---- 5. Inconsistent Category & Text Standardization ------------------------

#' Standardize text categories, casing, whitespace, and label mappings
#' @param df Data frame
#' @param col Categorical/character column name
#' @param action 'trim_whitespace', 'case_lower', 'case_upper', 'case_title', 'merge_categories', 'group_rare_other'
#' @param mapping Named character vector where names are old labels and values are new labels
#' @param rare_threshold Frequency threshold (default 0.05 / 5%) for 'group_rare_other'
#' @return List with success, data, affected_rows, affected_cells, message
transform_categories <- function(df, col, action = "trim_whitespace", mapping = NULL, rare_threshold = 0.05) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Invalid input or column not found."))
  }

  vec <- as.character(df[[col]])
  res_df <- df
  aff_count <- 0

  if (action == "trim_whitespace") {
    new_vec <- trimws(vec)
    aff_count <- sum(new_vec != vec, na.rm = TRUE)
    res_df[[col]] <- new_vec
    msg <- paste0("Trimmed leading and trailing whitespace across ", aff_count, " cell(s) in '", col, "'.")
  } else if (action == "case_lower") {
    new_vec <- tolower(vec)
    aff_count <- sum(new_vec != vec, na.rm = TRUE)
    res_df[[col]] <- new_vec
    msg <- paste0("Normalized ", aff_count, " cell(s) in '", col, "' to lowercase.")
  } else if (action == "case_upper") {
    new_vec <- toupper(vec)
    aff_count <- sum(new_vec != vec, na.rm = TRUE)
    res_df[[col]] <- new_vec
    msg <- paste0("Normalized ", aff_count, " cell(s) in '", col, "' to UPPERCASE.")
  } else if (action == "case_title") {
    to_title <- function(s) {
      if (is.na(s)) return(NA_character_)
      s <- tolower(s)
      paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
    }
    new_vec <- vapply(vec, to_title, character(1))
    aff_count <- sum(new_vec != vec, na.rm = TRUE)
    res_df[[col]] <- new_vec
    msg <- paste0("Normalized ", aff_count, " cell(s) in '", col, "' to Title Case.")
  } else if (action == "merge_categories") {
    if (is.null(mapping) || length(mapping) == 0) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "No category mapping provided."))
    }
    new_vec <- vec
    for (old_val in names(mapping)) {
      new_val <- mapping[[old_val]]
      match_idx <- which(vec == old_val)
      if (length(match_idx) > 0) {
        new_vec[match_idx] <- new_val
        aff_count <- aff_count + length(match_idx)
      }
    }
    res_df[[col]] <- new_vec
    msg <- paste0("Mapped ", aff_count, " value(s) in '", col, "' according to replacement rules.")
  } else if (action == "group_rare_other") {
    tab <- prop.table(table(vec[!is.na(vec)]))
    rare_cats <- names(tab)[tab < rare_threshold]
    if (length(rare_cats) == 0) {
      return(list(success = TRUE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Zero rare categories found below ", rare_threshold * 100, "% in '", col, "'.")))
    }
    new_vec <- vec
    rare_idx <- which(vec %in% rare_cats)
    new_vec[rare_idx] <- "Other"
    aff_count <- length(rare_idx)
    res_df[[col]] <- new_vec
    msg <- paste0("Grouped ", length(rare_cats), " rare category/categories (", aff_count, " rows) into 'Other'.")
  } else {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Unsupported category action: ", action)))
  }

  list(
    success = TRUE,
    data = res_df,
    affected_rows = aff_count,
    affected_cells = aff_count,
    message = msg
  )
}

# ---- 6. Business Validation Rules -------------------------------------------

#' Apply custom business rule validation and remediation
#' @param df Data frame
#' @param col Column name
#' @param operator '>', '>=', '<', '<=', '==', '!=', 'between', 'regex_match', 'not_missing'
#' @param threshold Numeric, string, or vector c(min, max) for between
#' @param action 'filter_violations' (drops rows violating rule) or 'replace_violations'
#' @param replace_val Replacement value if action == 'replace_violations'
#' @return List with success, data, affected_rows, affected_cells, message
transform_business_rules <- function(df, col, operator = ">=", threshold = 0,
                                     action = "filter_violations", replace_val = NA) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Invalid input or column not found."))
  }

  vec <- df[[col]]
  n_rows <- nrow(df)
  
  # Evaluate condition: is_valid is TRUE if row complies with rule
  is_valid <- rep(TRUE, n_rows)

  if (operator == ">") {
    is_valid <- !is.na(vec) & (as.numeric(vec) > as.numeric(threshold))
  } else if (operator == ">=") {
    is_valid <- !is.na(vec) & (as.numeric(vec) >= as.numeric(threshold))
  } else if (operator == "<") {
    is_valid <- !is.na(vec) & (as.numeric(vec) < as.numeric(threshold))
  } else if (operator == "<=") {
    is_valid <- !is.na(vec) & (as.numeric(vec) <= as.numeric(threshold))
  } else if (operator == "==") {
    is_valid <- !is.na(vec) & (as.character(vec) == as.character(threshold))
  } else if (operator == "!=") {
    is_valid <- !is.na(vec) & (as.character(vec) != as.character(threshold))
  } else if (operator == "between") {
    b_vals <- as.numeric(threshold)
    is_valid <- !is.na(vec) & (as.numeric(vec) >= b_vals[1] & as.numeric(vec) <= b_vals[2])
  } else if (operator == "regex_match") {
    is_valid <- !is.na(vec) & grepl(as.character(threshold), as.character(vec))
  } else if (operator == "not_missing") {
    is_valid <- !is.na(vec) & trimws(as.character(vec)) != ""
  }

  viol_idx <- which(!is_valid)
  n_viol <- length(viol_idx)

  if (n_viol == 0) {
    return(list(success = TRUE, data = df, affected_rows = 0, affected_cells = 0, message = "Zero business rule violations found. All rows comply."))
  }

  res_df <- df
  if (action == "filter_violations") {
    res_df <- df[is_valid, , drop = FALSE]
    rownames(res_df) <- NULL
    msg <- paste0("Filtered ", n_viol, " row(s) violating constraint: '", col, " ", operator, " ", paste(threshold, collapse = " to "), "'.")
  } else if (action == "replace_violations") {
    res_df[[col]][viol_idx] <- replace_val
    msg <- paste0("Replaced ", n_viol, " violating value(s) in '", col, "' with specified replacement.")
  } else if (action == "flag_indicator") {
    ind_name <- paste0(col, "_violates_rule")
    if (ind_name %in% names(res_df)) ind_name <- make.unique(c(names(res_df), ind_name))[ncol(res_df) + 1]
    res_df[[ind_name]] <- as.integer(!is_valid)
    msg <- paste0("Created violation indicator column '", ind_name, "' marking ", n_viol, " non-compliant row(s).")
  } else {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Unsupported rule action: ", action)))
  }

  list(
    success = TRUE,
    data = res_df,
    affected_rows = n_viol,
    affected_cells = n_viol,
    message = msg
  )
}

# ---- 7. General Text Cleaning -----------------------------------------------

#' General text cleaning and character sanitization
#' @param df Data frame
#' @param col Character column name
#' @param action 'collapse_spaces', 'replace_pattern', 'remove_symbols'
#' @param pattern Regex pattern for 'replace_pattern'
#' @param replacement Replacement string
#' @return List with success, data, affected_rows, affected_cells, message
transform_text <- function(df, col, action = "collapse_spaces", pattern = NULL, replacement = "") {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Invalid input or column not found."))
  }

  vec <- as.character(df[[col]])
  res_df <- df
  aff_count <- 0

  if (action == "collapse_spaces") {
    new_vec <- gsub("[[:space:]]+", " ", vec)
    new_vec <- trimws(new_vec)
    aff_count <- sum(new_vec != vec, na.rm = TRUE)
    res_df[[col]] <- new_vec
    msg <- paste0("Collapsed multi-spaces across ", aff_count, " cell(s) in '", col, "'.")
  } else if (action == "replace_pattern") {
    if (is.null(pattern) || nchar(pattern) == 0) {
      return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Pattern string cannot be empty."))
    }
    new_vec <- gsub(pattern, replacement, vec)
    aff_count <- sum(new_vec != vec, na.rm = TRUE)
    res_df[[col]] <- new_vec
    msg <- paste0("Applied text replacement '", pattern, "' -> '", replacement, "' on ", aff_count, " cell(s).")
  } else if (action == "remove_symbols") {
    new_vec <- gsub("[^[:alnum:][:space:]]", "", vec)
    aff_count <- sum(new_vec != vec, na.rm = TRUE)
    res_df[[col]] <- new_vec
    msg <- paste0("Stripped punctuation and symbols from ", aff_count, " cell(s) in '", col, "'.")
  } else {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = paste0("Unsupported text action: ", action)))
  }

  list(
    success = TRUE,
    data = res_df,
    affected_rows = aff_count,
    affected_cells = aff_count,
    message = msg
  )
}

# ---- 8. Cell-Level Diff & Verification Engine -------------------------------

#' Compute comprehensive before-and-after diff between two dataset states
#' @param df_before Baseline data frame
#' @param df_after Transformed data frame
#' @param col Focus column name (optional)
#' @return List with dimensions, row_delta, col_delta, changed_cells_count, diff_preview_df
compute_transform_diff <- function(df_before, df_after, col = NULL) {
  if (is.null(df_before) || is.null(df_after)) {
    return(list(
      rows_before = 0, rows_after = 0, row_delta = 0,
      cols_before = 0, cols_after = 0, col_delta = 0,
      changed_cells = 0, diff_preview = data.frame()
    ))
  }

  r_before <- nrow(df_before)
  r_after <- nrow(df_after)
  c_before <- ncol(df_before)
  c_after <- ncol(df_after)

  row_delta <- r_after - r_before
  col_delta <- c_after - c_before

  # Case 1: Same row count -> calculate exact cell-level changes
  if (r_before == r_after && c_before == c_after) {
    check_cols <- if (!is.null(col) && col %in% names(df_before) && col %in% names(df_after)) col else intersect(names(df_before), names(df_after))
    diff_records <- list()
    total_changed <- 0

    for (cn in check_cols) {
      v_bef <- df_before[[cn]]
      v_aft <- df_after[[cn]]
      
      # Handle NA comparisons safely
      changed <- (is.na(v_bef) != is.na(v_aft)) | (!is.na(v_bef) & !is.na(v_aft) & as.character(v_bef) != as.character(v_aft))
      ch_idx <- which(changed)
      total_changed <- total_changed + length(ch_idx)

      if (length(ch_idx) > 0) {
        sample_idx <- head(ch_idx, 50)
        for (i in sample_idx) {
          diff_records[[length(diff_records) + 1]] <- list(
            Row = i,
            Column = cn,
            Original_Value = as.character(v_bef[i]),
            New_Value = as.character(v_aft[i])
          )
        }
      }
    }

    diff_df <- if (length(diff_records) > 0) {
      do.call(rbind, lapply(diff_records, as.data.frame, stringsAsFactors = FALSE))
    } else {
      data.frame(Row = integer(0), Column = character(0), Original_Value = character(0), New_Value = character(0), stringsAsFactors = FALSE)
    }

    return(list(
      rows_before = r_before,
      rows_after = r_after,
      row_delta = row_delta,
      cols_before = c_before,
      cols_after = c_after,
      col_delta = col_delta,
      changed_cells = total_changed,
      diff_preview = diff_df
    ))
  }

  # Case 2: Row count changed (e.g., duplicate removal or outlier row filtering)
  sample_rows <- min(10, r_after)
  diff_df <- data.frame(
    Change_Type = if (row_delta < 0) "Rows Filtered/Removed" else "Rows Added",
    Rows_Before = r_before,
    Rows_After = r_after,
    Row_Delta = row_delta,
    Columns = r_after,
    stringsAsFactors = FALSE
  )

  list(
    rows_before = r_before,
    rows_after = r_after,
    row_delta = row_delta,
    cols_before = c_before,
    cols_after = c_after,
    col_delta = col_delta,
    changed_cells = abs(row_delta) * c_before,
    diff_preview = diff_df
  )
}

# ---- 8. Intelligent Column Name Sanitization --------------------------------

#' Standardize and sanitize messy column names into clean, database-grade identifiers
#' @param df Data frame
#' @param case_style 'snake_case', 'lower_camel', or 'screaming_snake'
#' @return List with success, data, affected_rows, affected_cells, message, name_mapping
transform_clean_names <- function(df, case_style = "snake_case") {
  if (is.null(df) || ncol(df) == 0) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Empty dataset.", name_mapping = data.frame()))
  }

  old_names <- names(df)
  new_names <- old_names

  # Handle currency or percentage symbols in parentheses first e.g. "Income ($)" -> "income_usd"
  new_names <- gsub("\\(\\$\\)", "_usd", new_names)
  new_names <- gsub("\\(€\\)", "_eur", new_names)
  new_names <- gsub("\\(£\\)", "_gbp", new_names)
  new_names <- gsub("\\(%\\)", "_pct", new_names)
  new_names <- gsub("%", "_pct", new_names)

  # Replace punctuation and special characters with spaces
  new_names <- gsub("[^[:alnum:]]+", " ", new_names)
  new_names <- trimws(new_names)

  # Convert camelCase to space-separated words (e.g. annualIncome -> annual Income)
  new_names <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", new_names)

  if (case_style == "snake_case") {
    new_names <- tolower(gsub("[[:space:]]+", "_", new_names))
  } else if (case_style == "screaming_snake") {
    new_names <- toupper(gsub("[[:space:]]+", "_", new_names))
  } else {
    new_names <- tolower(gsub("[[:space:]]+", "_", new_names))
  }

  # Replace empty or invalid names with V_i or prefix numbers with x_
  for (i in seq_along(new_names)) {
    if (is.na(new_names[i]) || new_names[i] == "" || grepl("^[0-9]", new_names[i])) {
      if (grepl("^[0-9]", new_names[i])) {
        new_names[i] <- paste0("x_", new_names[i])
      } else {
        new_names[i] <- paste0("col_", i)
      }
    }
  }

  # Ensure uniqueness
  new_names <- make.unique(new_names, sep = "_")

  res_df <- df
  names(res_df) <- new_names

  changed_count <- sum(old_names != new_names)
  mapping_df <- data.frame(Original = old_names, Cleaned = new_names, Changed = (old_names != new_names), stringsAsFactors = FALSE)

  list(
    success = TRUE,
    data = res_df,
    affected_rows = nrow(df),
    affected_cells = changed_count,
    message = paste0("Sanitized column names: modified ", changed_count, " of ", ncol(df), " headers to clean snake_case."),
    name_mapping = mapping_df
  )
}

# ---- 9. Currency & Percentage Coercion --------------------------------------

#' Parse currency strings ($1,200.50) or percentage strings (45.2%) into clean numeric floats
#' @param df Data frame
#' @param col Column name
#' @return List with success, data, affected_rows, affected_cells, message
transform_currency_percent <- function(df, col) {
  if (is.null(df) || nrow(df) == 0 || !(col %in% names(df))) {
    return(list(success = FALSE, data = df, affected_rows = 0, affected_cells = 0, message = "Invalid input or column not found."))
  }

  vec <- as.character(df[[col]])
  # Strip $, €, £, ¥, %, commas, and surrounding whitespace
  cleaned_str <- gsub("[$€£¥%,]", "", trimws(vec))
  suppressWarnings({
    num_vec <- as.numeric(cleaned_str)
  })

  non_na_idx <- which(!is.na(vec) & trimws(vec) != "")
  failed_idx <- which(!is.na(vec) & trimws(vec) != "" & is.na(num_vec))

  if (length(failed_idx) > 0 && length(failed_idx) > length(non_na_idx) * 0.2) {
    return(list(
      success = FALSE, data = df, affected_rows = 0, affected_cells = 0,
      message = paste0("Failed to coerce '", col, "': ", length(failed_idx), " values contain non-numeric characters.")
    ))
  }

  res_df <- df
  res_df[[col]] <- num_vec
  aff_count <- sum(!is.na(num_vec))

  list(
    success = TRUE,
    data = res_df,
    affected_rows = nrow(df),
    affected_cells = aff_count,
    message = paste0("Successfully converted '", col, "' from currency/percentage text into numeric format (", aff_count, " values parsed).")
  )
}

