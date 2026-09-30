test_that("analyze_outliers accurately computes IQR bounds and flags unusual points", {
  # Synthetic data with clear outliers
  # 20 regular points between 10 and 20, plus two outliers: -50 and 150
  regular_vals <- seq(10, 20, length.out = 20)
  test_vals <- c(-50, regular_vals, 150)
  df <- data.frame(val = test_vals)

  res <- analyze_outliers(df, iqr_multiplier = 1.5)

  expect_true(res$has_numeric)
  expect_equal(nrow(res$summary), 1)

  row <- res$summary[1, ]
  expect_equal(row$outlier_count, 2)
  expect_true(row$lower_bound > -50)
  expect_true(row$upper_bound < 150)

  # Check details
  det <- res$details$val
  expect_equal(length(det$outlier_indices), 2)
  expect_equal(det$is_lower_count, 1)
  expect_equal(det$is_upper_count, 1)
})

test_that("analyze_correlations calculates Pearson r and flags strong associations", {
  x <- 1:30
  y_strong <- x * 2 + rnorm(30, 0, 0.5)     # Very strong positive correlation
  y_uncorr <- sample(x)                     # Low correlation

  df <- data.frame(X = x, Y_Strong = y_strong, Y_Uncorr = y_uncorr)

  corr_res <- analyze_correlations(df, strong_thresh = 0.70, very_strong_thresh = 0.90)

  expect_true(corr_res$has_correlations)
  expect_equal(nrow(corr_res$matrix), 3)

  # Pair X & Y_Strong should have r > 0.90
  strong_pair <- corr_res$pairs[
    (corr_res$pairs$variable_a == "X" & corr_res$pairs$variable_b == "Y_Strong") |
      (corr_res$pairs$variable_a == "Y_Strong" & corr_res$pairs$variable_b == "X"),
  ]
  expect_true(nrow(strong_pair) == 1)
  expect_true(strong_pair$abs_r >= 0.90)
  expect_true(strong_pair$is_very_strong)
})
