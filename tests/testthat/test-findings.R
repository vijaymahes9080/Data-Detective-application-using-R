test_that("generate_findings dynamically creates structured findings and executive summary", {
  # Build synthetic dataset with specific defects
  df <- data.frame(
    id = 1:50,
    salary = c(rep(NA, 15), seq(50000, 90000, length.out = 33), 500000, 750000), # 15/50 = 30% missing (HIGH), 2 outliers
    status = rep("Active", 50), # Constant column (HIGH)
    stringsAsFactors = FALSE
  )

  prof <- profile_dataset(df)
  miss <- analyze_missing(df)
  dup <- analyze_duplicates(df)
  const <- analyze_constants(df)
  out <- analyze_outliers(df)
  corr <- analyze_correlations(df)

  findings_res <- generate_findings(
    profile = prof,
    missing_info = miss,
    duplicate_info = dup,
    constant_info = const,
    outlier_info = out,
    correlation_info = corr
  )

  expect_true(findings_res$total_findings >= 3)
  expect_true(findings_res$n_high >= 1)

  # Check that each finding has required fields
  f_df <- findings_res$findings
  expect_true(all(c("id", "category", "severity", "column", "description", "evidence", "why_investigate", "recommendation") %in% names(f_df)))

  # Check executive summary contains dynamic figures
  summary_text <- findings_res$executive_summary
  expect_true(grepl("50 rows", summary_text))
  expect_true(grepl("3 columns", summary_text))
  expect_true(nchar(summary_text) > 100)
})
