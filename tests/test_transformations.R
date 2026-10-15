# ==============================================================================
# DATA DETECTIVE PRO: Automated Test Suite for Data Transformations & Feature Studio
# File: tests/test_transformations.R
# Description: Rigorous unit tests for all data quality remediation algorithms,
#              feature engineering, smart recipe synthesis, and reproducible exports.
# ==============================================================================

library(testthat)

# Source all analytical engines and feature libraries
if (file.exists("../R/utility_functions.R")) {
  source("../R/utility_functions.R")
  source("../R/validation_functions.R")
  source("../R/analysis_functions.R")
  source("../R/transformation_engine.R")
  source("../R/feature_engine.R")
  source("../R/recipe_engine.R")
  source("../R/query_engine.R")
} else if (file.exists("R/utility_functions.R")) {
  source("R/utility_functions.R")
  source("R/validation_functions.R")
  source("R/analysis_functions.R")
  source("R/transformation_engine.R")
  source("R/feature_engine.R")
  source("R/recipe_engine.R")
  source("R/query_engine.R")
}

# ---- 1. Missing Value Imputation Tests ---------------------------------------

test_that("transform_missing accurately performs median imputation", {
  df <- data.frame(
    id = 1:5,
    val = c(10, 20, NA, 40, 50)
  )
  res <- transform_missing(df, col = "val", method = "median")
  expect_true(res$success)
  expect_equal(res$affected_rows, 1)
  expect_equal(res$data$val[3], 30) # median of 10, 20, 40, 50 is 30
  # Ensure original df was not mutated
  expect_true(is.na(df$val[3]))
})

test_that("transform_missing handles mode and custom imputation for text", {
  df <- data.frame(
    city = c("NY", "London", "NY", NA, "Paris"),
    stringsAsFactors = FALSE
  )
  res_mode <- transform_missing(df, col = "city", method = "mode")
  expect_true(res_mode$success)
  expect_equal(res_mode$data$city[4], "NY")

  res_custom <- transform_missing(df, col = "city", method = "custom", custom_val = "Unknown")
  expect_true(res_custom$success)
  expect_equal(res_custom$data$city[4], "Unknown")
})

test_that("transform_missing handles all-missing columns gracefully", {
  df <- data.frame(empty_col = c(NA, NA, NA))
  res <- transform_missing(df, col = "empty_col", method = "median")
  expect_false(res$success)
  expect_match(res$message, "all values are missing")
})

test_that("transform_missing creates missingness indicator column", {
  df <- data.frame(
    x = c(1, NA, 3, NA, 5)
  )
  res <- transform_missing(df, col = "x", method = "indicator_col")
  expect_true(res$success)
  expect_true("x_is_missing" %in% names(res$data))
  expect_equal(res$data$x_is_missing, c(0, 1, 0, 1, 0))
  expect_equal(res$data$x, c(1, NA, 3, NA, 5)) # original column untouched
})

# ---- 2. Deduplication Tests -------------------------------------------------

test_that("transform_duplicates handles exact duplicates and keep rules", {
  df <- data.frame(
    id = c(1, 2, 2, 3),
    val = c("A", "B", "B", "C"),
    stringsAsFactors = FALSE
  )
  res <- transform_duplicates(df, keep_rule = "first")
  expect_true(res$success)
  expect_equal(nrow(res$data), 3)
  expect_equal(res$affected_rows, 1)
  expect_equal(res$data$id, c(1, 2, 3))
})

test_that("transform_duplicates handles key-based deduplication with fewest_na", {
  df <- data.frame(
    id = c(101, 101, 102),
    name = c("John", "John", "Jane"),
    phone = c("555-1234", NA, "555-9999"),
    stringsAsFactors = FALSE
  )
  res <- transform_duplicates(df, key_cols = c("id", "name"), keep_rule = "fewest_na")
  expect_true(res$success)
  expect_equal(nrow(res$data), 2)
  expect_equal(res$data$phone[1], "555-1234")
})

# ---- 3. Outlier Treatment Tests ---------------------------------------------

test_that("transform_outliers caps extreme outliers via Winsorizing", {
  vals <- c(10, 12, 14, 15, 16, 18, 20, 1000) # 1000 is severe outlier
  df <- data.frame(x = vals)

  res <- transform_outliers(df, col = "x", method = "iqr_1.5", action = "cap_bounds")
  expect_true(res$success)
  expect_equal(res$affected_rows, 1)
  expect_equal(nrow(res$data), length(vals)) # Winsorizing preserves row count
  expect_true(res$data$x[8] < 50) # Capped to upper boundary
})

test_that("transform_outliers replaces outliers with median", {
  vals <- c(10, 12, 14, 15, 16, 18, 20, 500)
  df <- data.frame(x = vals)

  res <- transform_outliers(df, col = "x", method = "iqr_1.5", action = "replace_median")
  expect_true(res$success)
  med_expected <- stats::median(vals)
  expect_equal(res$data$x[8], med_expected)
})

# ---- 4. Data Type Conversion Tests ------------------------------------------

test_that("transform_types accurately casts parseable character to numeric", {
  df <- data.frame(
    num_text = c("10.5", "20.2", "30.8", "40.0"),
    stringsAsFactors = FALSE
  )
  res <- transform_types(df, col = "num_text", target_type = "numeric")
  expect_true(res$success)
  expect_true(is.numeric(res$data$num_text))
  expect_equal(res$data$num_text[1], 10.5)
})

test_that("transform_types catches non-parseable values without corruption", {
  df <- data.frame(
    dirty_num = c("100", "200", "N/A_NOT_A_NUM", "400"),
    stringsAsFactors = FALSE
  )
  res <- transform_types(df, col = "dirty_num", target_type = "numeric")
  expect_false(res$success)
  expect_equal(res$n_failed, 1)
  expect_equal(res$failed_rows, 3)
})

# ---- 5. Inconsistent Category Tests -----------------------------------------

test_that("transform_categories trims whitespace and normalizes casing", {
  df <- data.frame(
    city = c("  Chennai ", "chennai", "CHENNAI", "Chennai"),
    stringsAsFactors = FALSE
  )
  res_trim <- transform_categories(df, col = "city", action = "trim_whitespace")
  expect_true(res_trim$success)
  expect_equal(res_trim$data$city[1], "Chennai")

  res_title <- transform_categories(res_trim$data, col = "city", action = "case_title")
  expect_true(res_title$success)
  expect_equal(unique(res_title$data$city), "Chennai")
})

# ---- 6. Business Validation Rules Tests -------------------------------------

test_that("transform_business_rules filters rows violating bounds", {
  df <- data.frame(
    age = c(25, 30, -5, 45, 150),
    stringsAsFactors = FALSE
  )
  res <- transform_business_rules(df, col = "age", operator = ">=", threshold = 0, action = "filter_violations")
  expect_true(res$success)
  expect_equal(nrow(res$data), 4) # -5 removed
  expect_false(-5 %in% res$data$age)
})

test_that("transform_business_rules flags violations in indicator column", {
  df <- data.frame(
    age = c(25, 30, -5, 45, 150),
    stringsAsFactors = FALSE
  )
  res <- transform_business_rules(df, col = "age", operator = ">=", threshold = 0, action = "flag_indicator")
  expect_true(res$success)
  expect_equal(nrow(res$data), 5) # rows preserved
  expect_true("age_violates_rule" %in% names(res$data))
  expect_equal(res$data$age_violates_rule, c(0, 0, 1, 0, 0))
})

# ---- 7. Diff & State Rollback Tests -----------------------------------------

test_that("compute_transform_diff identifies modified cell changes", {
  df_orig <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  df_mod  <- data.frame(a = c(1, 20, 3), b = c("x", "y", "Z_NEW"), stringsAsFactors = FALSE)

  diff_res <- compute_transform_diff(df_orig, df_mod)
  expect_equal(diff_res$rows_before, 3)
  expect_equal(diff_res$rows_after, 3)
  expect_equal(diff_res$row_delta, 0)
  expect_equal(diff_res$changed_cells, 2)
  expect_equal(nrow(diff_res$diff_preview), 2)
})

# ---- 8. Column Name Sanitization & Currency Tests ---------------------------

test_that("transform_clean_names standardizes messy headers into snake_case", {
  df <- data.frame(
    ` Annual  Income ($)` = c(50000, 60000),
    `User ID#` = c(1, 2),
    `is_active?` = c("Y", "N"),
    check.names = FALSE
  )
  res <- transform_clean_names(df, case_style = "snake_case")
  expect_true(res$success)
  expect_true("annual_income_usd" %in% names(res$data))
  expect_true("user_id" %in% names(res$data))
  expect_true("is_active" %in% names(res$data))
})

test_that("transform_currency_percent converts formatted strings to numeric", {
  df <- data.frame(
    cost = c("$1,200.50", "$3,450.00", "$50.25"),
    pct = c("45.2%", "12.0%", "99.5%"),
    stringsAsFactors = FALSE
  )
  res_cost <- transform_currency_percent(df, col = "cost")
  expect_true(res_cost$success)
  expect_equal(res_cost$data$cost[1], 1200.50)

  res_pct <- transform_currency_percent(df, col = "pct")
  expect_true(res_pct$success)
  expect_equal(res_pct$data$pct[1], 45.2)
})

# ---- 9. Temporal & Cyclical Feature Derivation Tests ------------------------

test_that("derive_date_features generates temporal indicators", {
  df <- data.frame(
    reg_date = c("2024-01-15", "2024-06-20", "2024-12-31"),
    stringsAsFactors = FALSE
  )
  res <- derive_date_features(df, col = "reg_date")
  expect_true(res$success)
  expect_true(all(c("reg_date_year", "reg_date_month", "reg_date_quarter", "reg_date_day_of_week", "reg_date_is_weekend") %in% names(res$data)))
  expect_equal(res$data$reg_date_year[1], 2024L)
  expect_equal(res$data$reg_date_quarter[2], 2L)
})

test_that("derive_cyclical_features computes bounded Sine and Cosine coordinates", {
  df <- data.frame(month_num = c(1, 6, 12))
  res <- derive_cyclical_features(df, col = "month_num", period = 12)
  expect_true(res$success)
  expect_true(all(c("month_num_sin", "month_num_cos") %in% names(res$data)))
  # All values must be bounded within [-1, 1]
  expect_true(all(res$data$month_num_sin >= -1 & res$data$month_num_sin <= 1))
  expect_true(all(res$data$month_num_cos >= -1 & res$data$month_num_cos <= 1))
})

# ---- 10. Log Normalization & Scaling Tests ----------------------------------

test_that("derive_log_transform stabilizes skewed numeric variables", {
  df <- data.frame(income = c(100, 500, 2000, 100000))
  res <- derive_log_transform(df, col = "income")
  expect_true(res$success)
  expect_true("income_log" %in% names(res$data))
  expect_equal(res$data$income_log[1], log1p(100))
})

test_that("derive_scaled_features standardizes Z-scores and Min-Max ranges", {
  df <- data.frame(score = c(10, 20, 30, 40, 50))
  
  res_z <- derive_scaled_features(df, col = "score", method = "standardize_z")
  expect_true(res_z$success)
  expect_equal(round(mean(res_z$data$score_zscore), 2), 0)
  expect_equal(round(sd(res_z$data$score_zscore), 2), 1)

  res_mm <- derive_scaled_features(df, col = "score", method = "min_max")
  expect_true(res_mm$success)
  expect_equal(min(res_mm$data$score_scaled), 0)
  expect_equal(max(res_mm$data$score_scaled), 1)
})

# ---- 11. Ratio & Quantile Binning Tests -------------------------------------

test_that("derive_ratio_features calculates safe ratios", {
  df <- data.frame(
    debt = c(500, 1000, 200),
    income = c(2000, 4000, 0) # includes zero denominator
  )
  res <- derive_ratio_features(df, num_col_a = "debt", num_col_b = "income", new_name = "dti")
  expect_true(res$success)
  expect_equal(res$data$dti[1], 0.25)
  expect_true(is.na(res$data$dti[3])) # division by zero handled safely
})

test_that("derive_binned_features discretizes continuous metrics into quantile tiers", {
  df <- data.frame(age = c(18, 22, 28, 35, 42, 50, 60, 75))
  res <- derive_binned_features(df, col = "age", n_bins = 4)
  expect_true(res$success)
  expect_true("age_tier" %in% names(res$data))
  expect_equal(length(unique(res$data$age_tier)), 4)
})

# ---- 12. One-Hot & Text Feature Extraction Tests ----------------------------

test_that("derive_one_hot_encoding creates binary indicator dummies", {
  df <- data.frame(
    dept = c("Sales", "Eng", "Sales", "HR", "Eng"),
    stringsAsFactors = FALSE
  )
  res <- derive_one_hot_encoding(df, col = "dept", max_levels = 3)
  expect_true(res$success)
  expect_true(all(c("is_dept_sales", "is_dept_eng", "is_dept_hr") %in% names(res$data)))
  expect_equal(res$data$is_dept_sales, c(1, 0, 1, 0, 0))
})

test_that("derive_text_features computes word count and character length", {
  df <- data.frame(
    notes = c("Hello world", "SingleWord", "A fast brown fox jumps"),
    stringsAsFactors = FALSE
  )
  res <- derive_text_features(df, col = "notes")
  expect_true(res$success)
  expect_true(all(c("notes_char_len", "notes_word_count") %in% names(res$data)))
  expect_equal(res$data$notes_word_count, c(2, 1, 5))
  expect_equal(res$data$notes_char_len[1], 11)
})

# ---- 13. Smart Recipe Synthesizer & Executor Tests --------------------------

test_that("synthesize_smart_recipe and execute_recipe transform messy data end-to-end", {
  # Build a synthetic messy dataset
  messy_df <- data.frame(
    ` User ID ` = c(1, 2, 2, 4), # duplicate row 2, messy header
    age = c(25, NA, 35, 45),     # missing value
    salary = c(50000, 60000, 60000, 5000000), # extreme outlier
    reg_date = c("2024-01-01", "2024-02-01", "2024-02-01", "2024-03-01"), # date
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  recipe <- synthesize_smart_recipe(messy_df)
  expect_true(length(recipe) >= 3)

  exec_res <- execute_recipe(messy_df, recipe)
  expect_true(exec_res$steps_completed >= 3)
  expect_equal(nrow(exec_res$data), 3) # duplicate removed
  expect_false(any(is.na(exec_res$data$age))) # missing age imputed
  expect_true(exec_res$data$salary[4] < 4000000) # outlier Winsorized
})

# ---- 14. Reproducible R Code Generator Tests --------------------------------

test_that("generate_reproducible_r_code produces standalone runnable Base R script", {
  sample_recipe <- list(
    list(id = "step_1", title = "Deduplicate", action = "deduplicate_exact", column = "All", params = list(), enabled = TRUE),
    list(id = "step_2", title = "Impute Age", action = "impute_median", column = "age", params = list(method = "median"), enabled = TRUE),
    list(id = "step_3", title = "Winsorize", action = "winsorize_iqr", column = "salary", params = list(threshold = 1.5), enabled = TRUE)
  )

  r_code <- generate_reproducible_r_code(sample_recipe, dataset_name = "test.csv")
  expect_true(is.character(r_code))
  expect_match(r_code, "read.csv")
  expect_match(r_code, "median")
  expect_match(r_code, "quantile")
  expect_match(r_code, "write.csv")
})

# ---- 15. Natural Language Query Engine Tests --------------------------------

test_that("answer_data_query returns accurate deterministic insights", {
  df <- data.frame(
    x = c(10, 20, 30, 40, 50),
    y = c(20, 40, 60, 80, 100),
    stringsAsFactors = FALSE
  )

  # Query 1: Key Drivers
  ans_drivers <- answer_data_query("What are the key drivers and correlations?", df)
  expect_equal(ans_drivers$intent, "correlation")
  expect_match(ans_drivers$headline, "correlation")

  # Query 2: Executive Summary
  ans_summary <- answer_data_query("Give me an executive summary", df)
  expect_equal(ans_summary$intent, "executive_summary")
  expect_match(ans_summary$headline, "Executive Dataset Dossier")
})
