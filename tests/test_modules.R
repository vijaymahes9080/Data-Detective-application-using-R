# ==============================================================================
# DATA DETECTIVE: Unit Tests
# File: tests/test_modules.R
# ==============================================================================

library(testthat)

source("../R/helper_functions.R")
source("../R/validation_functions.R")
source("../R/analysis_functions.R")

test_that("calculate_quality_indicator accurately deducts penalties", {
  df <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c(10, 20, 30, 40, 50)
  )

  prof <- inspect_dataset(df)
  miss <- detect_missing(df)
  dup <- detect_duplicates(df)
  out <- detect_outliers(df)
  const_df <- detect_constant_columns(df)

  qi <- calculate_quality_indicator(prof, miss, dup, out, const_df)
  expect_true(qi$score < 100)
  expect_true(qi$status %in% c("Excellent", "Good", "Needs Attention", "Poor"))
})

test_that("generate_findings populates structured findings", {
  df <- data.frame(
    id = 1:20,
    bad_col = rep("constant", 20),
    num = c(-100, 1:18, 500)
  )

  prof <- inspect_dataset(df)
  miss <- detect_missing(df)
  dup <- detect_duplicates(df)
  out <- detect_outliers(df)
  const_df <- detect_constant_columns(df)
  corr <- calculate_correlations(df)
  leak <- detect_leakage_indicators(df)
  bias <- detect_bias_indicators(df)

  findings <- generate_findings(prof, miss, dup, out, const_df, corr, leak, bias)
  expect_true(is.data.frame(findings))
  expect_true(nrow(findings) >= 2)
  expect_true(all(c("category", "severity", "column", "message", "evidence", "recommendation") %in% names(findings)))
})
