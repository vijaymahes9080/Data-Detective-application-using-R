# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/duplicate_analysis.R
# Description: Exact and potential duplicate row investigation
# ==============================================================================

#' Analyze duplicate records in a dataset
#' @param df Data frame to analyze
#' @param key_cols Optional vector of key column names to check for potential duplicates
#' @return A list with exact duplicate metrics, duplicate data frame, and potential duplicates
analyze_duplicates <- function(df, key_cols = NULL) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      total_rows = 0,
      unique_rows_count = 0,
      duplicate_rows_count = 0,
      duplicate_pct = 0,
      duplicate_rows = data.frame(),
      has_duplicates = FALSE,
      potential_duplicates = data.frame(),
      status = "No Data"
    ))
  }

  n_rows <- nrow(df)

  # Exact duplicates identification
  # duplicated(df) flags 2nd, 3rd, ... occurrences
  is_dup <- duplicated(df)
  # duplicated from both sides flags ALL copies of duplicated rows
  all_dup_indices <- which(duplicated(df) | duplicated(df, fromLast = TRUE))

  duplicate_rows_count <- sum(is_dup)
  unique_rows_count <- n_rows - duplicate_rows_count
  duplicate_pct <- round((duplicate_rows_count / n_rows) * 100, 2)

  duplicate_records <- if (length(all_dup_indices) > 0) {
    df[all_dup_indices, , drop = FALSE]
  } else {
    data.frame()
  }

  # Potential duplicates:
  # Check if ignoring first column (often an ID or row number) reveals duplicate business records
  potential_dups <- data.frame()
  if (ncol(df) >= 3) {
    cols_to_check <- if (!is.null(key_cols) && length(key_cols) > 0 && all(key_cols %in% names(df))) {
      key_cols
    } else {
      # Ignore ID-like or index columns (e.g. named id, index, row_num, etc.)
      id_regex <- "^(id|_id|uuid|index|row_id|no|num)$"
      non_id_cols <- names(df)[!grepl(id_regex, names(df), ignore.case = TRUE)]
      if (length(non_id_cols) >= 2) non_id_cols else names(df)
    }

    if (length(cols_to_check) < ncol(df)) {
      sub_df <- df[, cols_to_check, drop = FALSE]
      sub_dup_idx <- which(duplicated(sub_df) | duplicated(sub_df, fromLast = TRUE))
      # Potential duplicates that are not already exact duplicates
      sub_dup_only <- setdiff(sub_dup_idx, all_dup_indices)
      if (length(sub_dup_only) > 0) {
        potential_dups <- df[sub_dup_only, , drop = FALSE]
      }
    }
  }

  status <- if (duplicate_rows_count == 0) {
    "No Exact Duplicates Detected"
  } else {
    paste0(format_number(duplicate_rows_count), " Exact Duplicate Record(s) Detected (", duplicate_pct, "%)")
  }

  return(list(
    total_rows = n_rows,
    unique_rows_count = unique_rows_count,
    duplicate_rows_count = duplicate_rows_count,
    duplicate_pct = duplicate_pct,
    duplicate_rows = duplicate_records,
    has_duplicates = (duplicate_rows_count > 0),
    potential_duplicates = potential_dups,
    potential_count = nrow(potential_dups),
    status = status,
    note = "Exact duplicates are identical across all columns. Data Detective never deletes records automatically."
  ))
}
