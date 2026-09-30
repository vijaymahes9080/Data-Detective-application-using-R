# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/finding_engine.R
# Description: Central rule-based finding engine, severity system, and executive summary
# ==============================================================================

#' Generate comprehensive, structured findings from all analytical engines
#' @param profile Dataset profile
#' @param missing_info Output from analyze_missing()
#' @param duplicate_info Output from analyze_duplicates()
#' @param constant_info Output from analyze_constants()
#' @param outlier_info Output from analyze_outliers()
#' @param correlation_info Output from analyze_correlations()
#' @param leakage_info Output from analyze_leakage_indicators()
#' @param representation_info Output from analyze_representation_indicators()
#' @return A list with structured findings data frame, severity counts, and executive summary
generate_findings <- function(profile,
                              missing_info = NULL,
                              duplicate_info = NULL,
                              constant_info = NULL,
                              outlier_info = NULL,
                              correlation_info = NULL,
                              leakage_info = NULL,
                              representation_info = NULL) {

  findings_list <- list()
  id_counter <- 1

  add_finding <- function(category, severity, column, description, evidence, why_investigate, recommendation) {
    prefix <- switch(
      category,
      "Missing Values" = "MISS",
      "Duplicate Records" = "DUP",
      "Unusual Observations" = "OUT",
      "Uninformative Variables" = "CONST",
      "Strong Correlations" = "CORR",
      "Leakage Indicators" = "LEAK",
      "Representation Indicators" = "REP",
      "Dataset Structure" = "STRUCT",
      "GEN"
    )
    fid <- sprintf("%s-%03d", prefix, id_counter)
    id_counter <<- id_counter + 1

    findings_list[[length(findings_list) + 1]] <<- list(
      id = fid,
      category = category,
      severity = toupper(severity),
      column = column,
      description = description,
      evidence = evidence,
      why_investigate = why_investigate,
      recommendation = recommendation
    )
  }

  # ---- 1. Missing Values Findings -------------------------------------------
  if (!is.null(missing_info) && nrow(missing_info$column_summary) > 0) {
    for (i in seq_len(nrow(missing_info$column_summary))) {
      row <- missing_info$column_summary[i, ]
      if (row$missing_count > 0) {
        sev <- if (row$missing_pct > missing_info$thresholds$critical) {
          "HIGH"
        } else if (row$missing_pct > missing_info$thresholds$moderate) {
          "WARNING"
        } else {
          "INFO"
        }

        add_finding(
          category = "Missing Values",
          severity = sev,
          column = row$column,
          description = paste0(row$missing_pct, "% of values are missing in column '", row$column, "'."),
          evidence = paste0(format_number(row$missing_count), " of ", format_number(profile$n_rows), " records are missing (Severity: ", row$severity, ")."),
          why_investigate = "Missingness can introduce bias, reduce statistical power, and trigger silent failures in downstream models.",
          recommendation = paste0("Investigate the collection mechanism for '", row$column, "'. Determine whether missingness is completely at random (MCAR) or systematic.")
        )
      }
    }
  }

  # ---- 2. Duplicate Records Findings ----------------------------------------
  if (!is.null(duplicate_info) && duplicate_info$duplicate_rows_count > 0) {
    sev <- if (duplicate_info$duplicate_pct >= 5.0) "HIGH" else if (duplicate_info$duplicate_pct >= 1.0) "WARNING" else "INFO"
    add_finding(
      category = "Duplicate Records",
      severity = sev,
      column = "All Columns",
      description = paste0(duplicate_info$duplicate_pct, "% of records are exact duplicate rows."),
      evidence = paste0(format_number(duplicate_info$duplicate_rows_count), " duplicate records detected out of ", format_number(duplicate_info$total_rows), " total rows."),
      why_investigate = "Exact duplicates artificially inflate sample size and can distort summary statistics and model weights.",
      recommendation = "Review duplicate rows in the Duplicates tab to ascertain whether they represent distinct repeated events or data logging errors."
    )
  }

  # ---- 3. Uninformative / Constant Columns Findings -------------------------
  if (!is.null(constant_info) && nrow(constant_info$uninformative_columns) > 0) {
    for (i in seq_len(nrow(constant_info$uninformative_columns))) {
      row <- constant_info$uninformative_columns[i, ]
      sev <- if (row$issue_type == "Strictly Constant" || row$issue_type == "All Missing") "HIGH" else "WARNING"
      add_finding(
        category = "Uninformative Variables",
        severity = sev,
        column = row$column,
        description = row$finding,
        evidence = paste0(row$issue_type, ": Dominant value '", row$dominant_value, "' covers ", row$dominant_pct, "% of records."),
        why_investigate = "Variables with zero or negligible variance offer no discriminatory power and consume computational resources.",
        recommendation = paste0("Examine '", row$column, "'. If confirmed redundant for your specific domain objective, consider excluding it from predictive modeling.")
      )
    }
  }

  # ---- 4. Outliers Findings -------------------------------------------------
  if (!is.null(outlier_info) && nrow(outlier_info$summary) > 0) {
    for (i in seq_len(nrow(outlier_info$summary))) {
      row <- outlier_info$summary[i, ]
      if (row$outlier_count > 0) {
        sev <- if (row$outlier_pct >= 5.0) "WARNING" else "INFO"
        add_finding(
          category = "Unusual Observations",
          severity = sev,
          column = row$column,
          description = paste0(row$outlier_pct, "% statistically unusual observations detected (beyond 1.5 \u00d7 IQR)."),
          evidence = paste0(format_number(row$outlier_count), " observations fall outside the calculated interval [", row$lower_bound, ", ", row$upper_bound, "]."),
          why_investigate = "Extreme observations can heavily leverage means, variances, and regression coefficients.",
          recommendation = paste0("Inspect values for '", row$column, "' in the Outliers tab. Verify whether they stem from measurement anomalies, data entry, or valid heavy-tailed behavior.")
        )
      }
    }
  }

  # ---- 5. Correlation & Multicollinearity Findings --------------------------
  if (!is.null(correlation_info) && nrow(correlation_info$multicollinear_pairs) > 0) {
    for (i in seq_len(nrow(correlation_info$multicollinear_pairs))) {
      row <- correlation_info$multicollinear_pairs[i, ]
      sev <- if (row$is_very_strong) "HIGH" else "WARNING"
      add_finding(
        category = "Strong Correlations",
        severity = sev,
        column = paste0(row$variable_a, " & ", row$variable_b),
        description = paste0(row$strength, " (r = ", row$pearson_r, ")."),
        evidence = paste0("Pearson correlation coefficient is ", row$pearson_r, " (|r| = ", row$abs_r, ")."),
        why_investigate = "Severe multicollinearity inflates parameter estimate variances and complicates feature attribution.",
        recommendation = paste0("Assess relationship between '", row$variable_a, "' and '", row$variable_b, "'. Consider dimension reduction or selecting the more reliably measured feature.")
      )
    }
  }

  # ---- 6. Leakage Indicators Findings ---------------------------------------
  if (!is.null(leakage_info) && leakage_info$target_specified && nrow(leakage_info$indicators) > 0) {
    for (i in seq_len(nrow(leakage_info$indicators))) {
      row <- leakage_info$indicators[i, ]
      add_finding(
        category = "Leakage Indicators",
        severity = row$severity,
        column = row$column,
        description = row$finding,
        evidence = row$evidence,
        why_investigate = "Features that inadvertently embed information from the target variable result in deceptively high training accuracy and test-time failure.",
        recommendation = paste0("Verify the timing of when '", row$column, "' is recorded relative to '", row$target, "'. If created after the target event, exclude it.")
      )
    }
  }

  # ---- 7. Representation Indicators Findings --------------------------------
  if (!is.null(representation_info) && nrow(representation_info$indicators) > 0) {
    for (i in seq_len(nrow(representation_info$indicators))) {
      row <- representation_info$indicators[i, ]
      add_finding(
        category = "Representation Indicators",
        severity = row$severity,
        column = row$column,
        description = row$finding,
        evidence = row$evidence,
        why_investigate = "Skewed category representation or unequal missingness across groups may lead to poor model generalizability for minority cohorts.",
        recommendation = paste0("Examine data collection protocols across sub-populations for '", row$column, "'. Ensure sample coverage aligns with intended domain scope.")
      )
    }
  }

  # Convert findings list to structured data frame
  findings_df <- if (length(findings_list) > 0) {
    do.call(rbind, lapply(findings_list, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(
      id = character(0),
      category = character(0),
      severity = character(0),
      column = character(0),
      description = character(0),
      evidence = character(0),
      why_investigate = character(0),
      recommendation = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Severity counts
  n_high <- sum(findings_df$severity == "HIGH")
  n_warn <- sum(findings_df$severity == "WARNING")
  n_info <- sum(findings_df$severity == "INFO")
  n_total <- nrow(findings_df)

  # Dynamic Executive Summary Generator (Section 28)
  summary_text <- generate_executive_summary_text(
    profile = profile,
    missing_info = missing_info,
    duplicate_info = duplicate_info,
    outlier_info = outlier_info,
    constant_info = constant_info,
    correlation_info = correlation_info,
    representation_info = representation_info,
    findings_df = findings_df
  )

  return(list(
    findings = findings_df,
    total_findings = n_total,
    n_high = n_high,
    n_warning = n_warn,
    n_info = n_info,
    executive_summary = summary_text
  ))
}

#' Generate dynamic executive summary narrative strictly from calculated metrics
generate_executive_summary_text <- function(profile,
                                            missing_info,
                                            duplicate_info,
                                            outlier_info,
                                            constant_info,
                                            correlation_info,
                                            representation_info,
                                            findings_df) {
  if (is.null(profile) || profile$n_rows == 0) {
    return("No dataset currently analyzed.")
  }

  sentences <- character(0)

  # Sentence 1: Dimensions
  sentences <- c(sentences, sprintf(
    "The dataset contains %s rows and %s columns (%s total data cells, occupying approximately %s in memory).",
    format_number(profile$n_rows),
    format_number(profile$n_cols),
    format_number(profile$total_cells),
    profile$memory_formatted
  ))

  # Sentence 2: Variable types
  sentences <- c(sentences, sprintf(
    "%d numeric variable(s), %d categorical variable(s), %d date/time variable(s), and %d logical variable(s) were identified.",
    profile$n_numeric,
    profile$n_categorical,
    profile$n_date,
    profile$n_logical
  ))

  # Sentence 3: Missing values
  if (!is.null(missing_info)) {
    if (missing_info$total_missing == 0) {
      sentences <- c(sentences, "No missing values were detected; the dataset is 100% complete across all columns.")
    } else {
      sentences <- c(sentences, sprintf(
        "%d column(s) contain missing values, representing %s missing cells (%s%% of total dataset volume).",
        missing_info$columns_with_missing,
        format_number(missing_info$total_missing),
        missing_info$missing_pct
      ))
    }
  }

  # Sentence 4: Duplicates
  if (!is.null(duplicate_info)) {
    if (duplicate_info$duplicate_rows_count == 0) {
      sentences <- c(sentences, "No exact duplicate records were detected.")
    } else {
      sentences <- c(sentences, sprintf(
        "%s exact duplicate record(s) (%s%%) were detected across all columns.",
        format_number(duplicate_info$duplicate_rows_count),
        duplicate_info$duplicate_pct
      ))
    }
  }

  # Sentence 5: Uninformative columns
  if (!is.null(constant_info) && nrow(constant_info$uninformative_columns) > 0) {
    sentences <- c(sentences, sprintf(
      "%d variable(s) were flagged as potentially uninformative due to constant values or near-zero variance (%s).",
      nrow(constant_info$uninformative_columns),
      paste(head(constant_info$uninformative_columns$column, 3), collapse = ", ")
    ))
  }

  # Sentence 6: Outliers
  if (!is.null(outlier_info) && outlier_info$total_outlier_variables > 0) {
    sentences <- c(sentences, sprintf(
      "Statistically unusual observations (beyond 1.5 \u00d7 IQR bounds) were identified in %d numeric variable(s).",
      outlier_info$total_outlier_variables
    ))
  }

  # Sentence 7: Correlations
  if (!is.null(correlation_info) && nrow(correlation_info$multicollinear_pairs) > 0) {
    top_pair <- correlation_info$multicollinear_pairs[1, ]
    sentences <- c(sentences, sprintf(
      "A strong correlation was identified between '%s' and '%s' (r = %s).",
      top_pair$variable_a,
      top_pair$variable_b,
      top_pair$pearson_r
    ))
  }

  # Sentence 8: Representation issues
  if (!is.null(representation_info) && nrow(representation_info$indicators) > 0) {
    sentences <- c(sentences, sprintf(
      "Potential representation patterns were detected in %d variable check(s), requiring domain review.",
      nrow(representation_info$indicators)
    ))
  }

  # Summary of findings count
  sentences <- c(sentences, sprintf(
    "Overall, Data Detective generated %d total investigation finding(s): %d High, %d Warning, and %d Info.",
    nrow(findings_df),
    sum(findings_df$severity == "HIGH"),
    sum(findings_df$severity == "WARNING"),
    sum(findings_df$severity == "INFO")
  ))

  paste(sentences, collapse = " ")
}
