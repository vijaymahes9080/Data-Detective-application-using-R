test_that("analyze_missing calculates correct counts, percentages, and severity levels", {
  df <- data.frame(
    complete_v = 1:100,
    low_v = c(rep(NA, 4), 5:100),       # 4% missing -> Low
    mod_v = c(rep(NA, 15), 16:100),     # 15% missing -> Moderate
    high_v = c(rep(NA, 35), 36:100),    # 35% missing -> High
    crit_v = c(rep(NA, 60), 61:100)     # 60% missing -> Critical
  )

  res <- analyze_missing(df)

  expect_equal(res$total_missing, 4 + 15 + 35 + 60)
  expect_equal(res$columns_with_missing, 4)

  c_sum <- res$column_summary
  expect_equal(c_sum$severity[c_sum$column == "complete_v"], "Complete")
  expect_equal(c_sum$severity[c_sum$column == "low_v"], "Low")
  expect_equal(c_sum$severity[c_sum$column == "mod_v"], "Moderate")
  expect_equal(c_sum$severity[c_sum$column == "high_v"], "High")
  expect_equal(c_sum$severity[c_sum$column == "crit_v"], "Critical")
})

test_that("analyze_duplicates accurately identifies exact duplicates", {
  df <- data.frame(
    id = c(1, 2, 3, 2, 4, 1),
    name = c("A", "B", "C", "B", "D", "A"),
    stringsAsFactors = FALSE
  )

  dup <- analyze_duplicates(df)

  expect_equal(dup$total_rows, 6)
  expect_equal(dup$duplicate_rows_count, 2)
  expect_equal(dup$unique_rows_count, 4)
  expect_equal(round(dup$duplicate_pct, 1), 33.3)
  expect_true(dup$has_duplicates)
})

test_that("analyze_constants identifies strictly constant and near-constant columns", {
  df <- data.frame(
    const_col = rep("SingleVal", 50),
    near_const = c(rep("DomVal", 47), "Rare1", "Rare2", "Rare3"), # 47/50 = 94% -> near-constant
    varying_col = 1:50,
    stringsAsFactors = FALSE
  )

  res <- analyze_constants(df, concentration_threshold = 0.90)

  expect_equal(res$constant_count, 1)
  expect_equal(res$near_constant_count, 1)
  expect_equal(nrow(res$uninformative_columns), 2)
  expect_true("const_col" %in% res$uninformative_columns$column)
  expect_true("near_const" %in% res$uninformative_columns$column)
})
