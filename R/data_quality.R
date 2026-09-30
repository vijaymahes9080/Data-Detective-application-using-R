# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/data_quality.R
# Description: Transparent composite data-quality scoring and indicator engine
# ==============================================================================

#' Calculate Data Detective Composite Quality Indicator
#' @param profile Output from profile_dataset()
#' @param duplicate_info Output from analyze_duplicates()
#' @param outlier_info Output from analyze_outliers()
#' @param constant_info Output from analyze_constants()
#' @return A list with overall score, quality status, deductions, and explanations
calculate_quality_score <- function(profile, duplicate_info = NULL, outlier_info = NULL, constant_info = NULL) {
  if (is.null(profile) || profile$n_rows == 0 || profile$n_cols == 0) {
    return(list(
      score = 0,
      status = "No Data",
      status_color = "#6b7280",
      deductions = list(),
      explanation = "No data loaded."
    ))
  }

  base_score <- 100
  deductions <- list()

  # 1. Missingness Penalty
  # Missing cell proportion across entire dataset
  total_cells <- profile$total_cells
  total_missing <- sum(profile$columns_summary$missing_count, na.rm = TRUE)
  missing_pct <- ifelse(total_cells > 0, (total_missing / total_cells) * 100, 0)

  missing_penalty <- 0
  if (missing_pct > 0) {
    # Scale from 0 to 30 points maximum
    missing_penalty <- min(35, round(missing_pct * 0.7, 1))
    deductions$missingness <- list(
      name = "Missing Data Rate",
      points = missing_penalty,
      metric = paste0(round(missing_pct, 1), "% cells missing (", format_number(total_missing), "/", format_number(total_cells), ")"),
      description = "Deduction proportional to missing cell percentage across the dataset."
    )
  }

  # 2. Duplicate Rows Penalty
  dup_penalty <- 0
  if (!is.null(duplicate_info) && duplicate_info$duplicate_rows_count > 0) {
    dup_pct <- duplicate_info$duplicate_pct
    # Scale from 0 to 25 points maximum
    dup_penalty <- min(25, round(dup_pct * 0.8, 1))
    deductions$duplicates <- list(
      name = "Duplicate Records",
      points = dup_penalty,
      metric = paste0(round(dup_pct, 1), "% duplicate rows (", format_number(duplicate_info$duplicate_rows_count), " rows)"),
      description = "Deduction based on frequency of identical duplicate rows."
    )
  }

  # 3. Outlier Penalty
  outlier_penalty <- 0
  if (!is.null(outlier_info) && nrow(outlier_info$summary) > 0) {
    cols_with_outliers <- sum(outlier_info$summary$outlier_count > 0)
    total_num_cols <- nrow(outlier_info$summary)
    outlier_col_pct <- ifelse(total_num_cols > 0, (cols_with_outliers / total_num_cols) * 100, 0)
    # Scale from 0 to 15 points
    outlier_penalty <- min(15, round((outlier_col_pct / 100) * 15, 1))
    if (outlier_penalty > 0) {
      deductions$outliers <- list(
        name = "Unusual Observations (Outliers)",
        points = outlier_penalty,
        metric = paste0(cols_with_outliers, " of ", total_num_cols, " numeric variables contain IQR outliers"),
        description = "Deduction based on presence of statistical outliers requiring investigation."
      )
    }
  }

  # 4. Constant / Uninformative Columns Penalty
  const_penalty <- 0
  if (!is.null(constant_info) && nrow(constant_info$uninformative_columns) > 0) {
    n_const <- nrow(constant_info$uninformative_columns)
    # 5 points per uninformative column, up to 15
    const_penalty <- min(15, n_const * 5)
    deductions$constants <- list(
      name = "Uninformative Variables",
      points = const_penalty,
      metric = paste0(n_const, " constant or near-constant variable(s)"),
      description = "Deduction for columns with near-zero variability or single unique values."
    )
  }

  # 5. Completely Empty (All-NA) Columns Penalty
  all_na_count <- sum(profile$columns_summary$is_all_na, na.rm = TRUE)
  all_na_penalty <- 0
  if (all_na_count > 0) {
    all_na_penalty <- min(15, all_na_count * 5)
    deductions$all_na <- list(
      name = "All-Missing Columns",
      points = all_na_penalty,
      metric = paste0(all_na_count, " column(s) have 100% missing values"),
      description = "Deduction for columns containing zero usable observations."
    )
  }

  total_penalty <- missing_penalty + dup_penalty + outlier_penalty + const_penalty + all_na_penalty
  final_score <- max(0, min(100, round(base_score - total_penalty, 0)))

  # Status determination
  status <- if (final_score >= 90) {
    "Excellent"
  } else if (final_score >= 75) {
    "Good"
  } else if (final_score >= 50) {
    "Needs Attention"
  } else {
    "Poor"
  }

  status_color <- switch(
    status,
    "Excellent" = "#10b981",       # Emerald green
    "Good" = "#3b82f6",            # Blue
    "Needs Attention" = "#f59e0b", # Amber
    "Poor" = "#ef4444"             # Red
  )

  explanation <- paste0(
    "Data Detective Quality Indicator: Score is calculated starting from a baseline of 100, ",
    "subtracting heuristic penalties for missingness (-", missing_penalty, "), ",
    "duplicates (-", dup_penalty, "), outlier presence (-", outlier_penalty, "), ",
    "uninformative columns (-", const_penalty, "), and empty columns (-", all_na_penalty, "). ",
    "Status is based on the configurable checks used by Data Detective."
  )

  return(list(
    score = final_score,
    status = status,
    status_color = status_color,
    base_score = base_score,
    total_deductions = total_penalty,
    deductions = deductions,
    explanation = explanation,
    total_missing_cells = total_missing,
    missing_cell_pct = round(missing_pct, 2)
  ))
}
