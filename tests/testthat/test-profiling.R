test_that("profile_dataset handles empty datasets gracefully", {
  empty_df <- data.frame()
  prof <- profile_dataset(empty_df)
  expect_equal(prof$n_rows, 0)
  expect_equal(prof$n_cols, 0)
  expect_equal(prof$total_cells, 0)
  expect_equal(nrow(prof$columns_summary), 0)
})

test_that("profile_dataset computes correct dimensions, types, and summary statistics", {
  df <- data.frame(
    id = 1:10,
    val = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    grp = c(rep("A", 7), rep("B", 3)),
    missing_col = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10),
    stringsAsFactors = FALSE
  )

  prof <- profile_dataset(df)

  expect_equal(prof$n_rows, 10)
  expect_equal(prof$n_cols, 4)
  expect_equal(prof$total_cells, 40)
  expect_equal(prof$n_numeric, 3)
  expect_equal(prof$n_categorical, 1)

  # Check missing column stats
  miss_row <- prof$columns_summary[prof$columns_summary$column == "missing_col", ]
  expect_equal(miss_row$missing_count, 2)
  expect_equal(miss_row$missing_pct, 0.20)

  # Check numeric values
  val_row <- prof$columns_summary[prof$columns_summary$column == "val", ]
  expect_equal(val_row$mean, 55)
  expect_equal(val_row$median, 55)
  expect_equal(val_row$min, "10")
  expect_equal(val_row$max, "100")
})
