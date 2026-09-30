# ==============================================================================
# DATA DETECTIVE: Unit Tests
# File: tests/test_analysis.R
# ==============================================================================

library(testthat)

# Source required analytical engines
source("../R/utility_functions.R")
source("../R/validation_functions.R")
source("../R/analysis_functions.R")

test_that("detect_missing correctly computes metrics across columns", {
  df <- data.frame(
    a = c(1, 2, NA, 4, 5),
    b = c("x", "y", "z", "w", "v"),
    c = c(NA, NA, NA, NA, NA)
  )

  m <- detect_missing(df)
  expect_equal(m$total_missing, 6)
  expect_equal(m$columns_with_missing, 2)
  expect_equal(m$complete_cases, 0)
})

test_that("detect_duplicates accurately isolates identical rows", {
  df <- data.frame(
    id = c(1, 2, 3, 1),
    val = c("A", "B", "C", "A"),
    stringsAsFactors = FALSE
  )

  d <- detect_duplicates(df)
  expect_true(d$has_duplicates)
  expect_equal(d$duplicate_rows_count, 1)
  expect_equal(d$unique_rows, 3)
})

test_that("detect_outliers computes 1.5x IQR bounds properly", {
  vals <- c(-100, 10, 12, 14, 15, 16, 18, 20, 200)
  df <- data.frame(x = vals)

  o <- detect_outliers(df, iqr_multiplier = 1.5)
  expect_true(o$has_numeric)
  expect_equal(o$summary$outlier_count[1], 2)
})

test_that("calculate_correlations produces pairwise association matrix", {
  x <- 1:20
  y <- x * 3 + rnorm(20, 0, 0.1)
  df <- data.frame(x = x, y = y)

  c_res <- calculate_correlations(df)
  expect_true(c_res$has_correlations)
  expect_true(c_res$pairs$correlation[1] >= 0.90)
})

test_that("detect_constant_columns flags uninformative features", {
  df <- data.frame(
    const = rep("constant", 30),
    varying = 1:30
  )

  k <- detect_constant_columns(df)
  expect_equal(nrow(k), 1)
  expect_equal(k$column[1], "const")
})
