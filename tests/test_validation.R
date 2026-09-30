# ==============================================================================
# DATA DETECTIVE: Unit Tests
# File: tests/test_validation.R
# ==============================================================================

library(testthat)

source("../R/helper_functions.R")
source("../R/validation_functions.R")

test_that("validate_csv_file handles non-existent or empty paths gracefully", {
  res1 <- validate_csv_file(NULL)
  expect_false(res1$valid)

  res2 <- validate_csv_file("non_existent_file.csv")
  expect_false(res2$valid)
})

test_that("validate_dataset detects empty rows or columns", {
  res_empty <- validate_dataset(data.frame())
  expect_false(res_empty$valid)

  res_valid <- validate_dataset(data.frame(a = 1:5, b = letters[1:5]))
  expect_true(res_valid$valid)
})

test_that("detect_delimiter identifies standard commas and tabs", {
  tmp_comma <- tempfile(fileext = ".csv")
  writeLines(c("a,b,c", "1,2,3", "4,5,6"), tmp_comma)
  expect_equal(detect_delimiter(tmp_comma), ",")
  unlink(tmp_comma)

  tmp_semi <- tempfile(fileext = ".csv")
  writeLines(c("a;b;c", "1;2;3", "4;5;6"), tmp_semi)
  expect_equal(detect_delimiter(tmp_semi), ";")
  unlink(tmp_semi)
})
