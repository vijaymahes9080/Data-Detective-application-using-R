# ==============================================================================
# DATA DETECTIVE PRO: Natural Language Query & Insights Engine
# File: R/query_engine.R
# Description: Deterministic natural-language data question answering & intelligence
# Shinylive / WebAssembly compatible (Pure Base R)
# ==============================================================================

#' Answer natural language user inquiries deterministically using active dataset evidence
#' @param query User's string query
#' @param df Active working dataset
#' @param profile Output of inspect_dataset(df)
#' @param findings Output of generate_findings(...)
#' @return List with intent, headline, details, recommendation, chart_type, chart_col
answer_data_query <- function(query, df, profile = NULL, findings = NULL) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      intent = "empty",
      headline = "No active dataset loaded.",
      details = "Please upload a dataset or select a benchmark sample to ask questions.",
      recommendation = "Upload a CSV, TSV, or RDS file.",
      chart_type = "none",
      chart_col = NULL
    ))
  }

  if (is.null(profile)) profile <- inspect_dataset(df)
  q_clean <- tolower(trimws(query))
  cols <- names(df)

  # Check if a specific column is mentioned in the query
  matched_col <- NULL
  for (cn in cols) {
    if (grepl(tolower(cn), q_clean, fixed = TRUE)) {
      matched_col <- cn
      break
    }
  }

  # ---- Intent 1: Outliers / Anomalies ----------------------------------------
  if (grepl("outlier|anomal|unusual|extreme|leverage", q_clean)) {
    out_res <- detect_outliers(df)
    if (!out_res$has_numeric || nrow(out_res$summary) == 0) {
      return(list(
        intent = "outliers",
        headline = "No numeric variables available for outlier analysis.",
        details = "This dataset does not contain numeric columns.",
        recommendation = "Explore categorical distributions in Column Studio.",
        chart_type = "none",
        chart_col = NULL
      ))
    }

    target_c <- if (!is.null(matched_col) && matched_col %in% out_res$numeric_columns) matched_col else out_res$summary$column[which.max(out_res$summary$outlier_count)]
    r <- out_res$summary[out_res$summary$column == target_c, ][1, ]

    return(list(
      intent = "outliers",
      headline = paste0("Detected ", r$outlier_count, " outlier(s) in '", target_c, "' (", r$outlier_pct, "% of observations)."),
      details = paste0("Values lie outside Tukey 1.5 \u00d7 IQR bounds [", r$lower_bound, ", ", r$upper_bound, "]. Valid count is ", r$valid_count, "."),
      recommendation = "Use the Quality Workbench Outlier Treatment tab to Winsorize/cap these values to prevent variance distortion.",
      chart_type = "boxplot",
      chart_col = target_c
    ))
  }

  # ---- Intent 2: Missing Values ----------------------------------------------
  if (grepl("miss|na|nan|null|empty|incomplete", q_clean)) {
    miss_res <- detect_missing(df)
    m_cols <- miss_res$column_summary[miss_res$column_summary$missing_count > 0, ]

    if (nrow(m_cols) == 0) {
      return(list(
        intent = "missing",
        headline = "Zero missing values detected across the entire active dataset!",
        details = paste0("All ", format_number(profile$n_rows), " rows and ", profile$n_cols, " columns have 100% completeness."),
        recommendation = "Dataset is fully complete for analytical modeling.",
        chart_type = "none",
        chart_col = NULL
      ))
    }

    top_m <- m_cols[1, ]
    return(list(
      intent = "missing",
      headline = paste0("A total of ", format_number(miss_res$total_missing), " cells (", miss_res$missing_pct, "%) are missing across ", miss_res$columns_with_missing, " column(s)."),
      details = paste0("Column with highest missingness is '", top_m$column, "' with ", top_m$missing_pct, "% missing (", format_number(top_m$missing_count), " rows)."),
      recommendation = "Visit Quality Workbench to impute missing numbers with median or text with mode.",
      chart_type = "missing_bar",
      chart_col = top_m$column
    ))
  }

  # ---- Intent 3: Correlations & Key Drivers ---------------------------------
  if (grepl("correlat|driver|relationship|predict|influenc|associat", q_clean)) {
    corr_res <- calculate_correlations(df)
    if (!corr_res$has_correlations || nrow(corr_res$pairs) == 0) {
      return(list(
        intent = "correlation",
        headline = "Insufficient numeric variables to compute correlation matrix.",
        details = "At least two numeric columns are required to evaluate correlation coefficients.",
        recommendation = "Cast numeric string columns to numeric in the Workbench.",
        chart_type = "none",
        chart_col = NULL
      ))
    }

    top_pair <- corr_res$pairs[1, ]
    return(list(
      intent = "correlation",
      headline = paste0("Strongest correlation: '", top_pair$variable_a, "' and '", top_pair$variable_b, "' (r = ", top_pair$correlation, ", ", top_pair$strength, ")."),
      details = paste0("Total collinear pairs with |r| \u2265 0.70: ", nrow(corr_res$multicollinear_pairs), " pair(s)."),
      recommendation = "Check the Relationships tab to view bivariate scatter plots and trendlines.",
      chart_type = "scatter",
      chart_col = c(top_pair$variable_a, top_pair$variable_b)
    ))
  }

  # ---- Intent 4: Executive Summary / Dataset Health --------------------------
  if (grepl("summary|executive|overview|explain|story|tell me|health", q_clean)) {
    miss_res <- detect_missing(df)
    dup_res <- detect_duplicates(df)
    out_res <- detect_outliers(df)

    n_out_tot <- if (out_res$has_numeric && nrow(out_res$summary) > 0) sum(out_res$summary$outlier_count) else 0

    return(list(
      intent = "executive_summary",
      headline = paste0("Executive Dataset Dossier: ", format_number(profile$n_rows), " rows across ", profile$n_cols, " attributes."),
      details = paste0("Completeness: ", 100 - miss_res$missing_pct, "% | Exact Duplicates: ", dup_res$duplicate_rows_count, " | Flagged Outliers: ", n_out_tot, "."),
      recommendation = "Use the One-Click Raw-to-Useful Smart Recipe in Workbench to generate a clean, enriched data product.",
      chart_type = "none",
      chart_col = NULL
    ))
  }

  # ---- Intent 5: Specific Column Distribution --------------------------------
  if (!is.null(matched_col)) {
    v <- df[[matched_col]]
    if (is.numeric(v)) {
      clean_v <- v[!is.na(v)]
      m <- mean(clean_v)
      med <- stats::median(clean_v)
      sk <- safe_skewness(clean_v)
      return(list(
        intent = "column_drilldown",
        headline = paste0("Column '", matched_col, "' (Numeric): Mean = ", round(m, 2), ", Median = ", round(med, 2), "."),
        details = paste0("Sample skewness is ", round(sk, 2), ". Min = ", round(min(clean_v), 2), ", Max = ", round(max(clean_v), 2), "."),
        recommendation = "Open Column Studio for comprehensive quantile plots, ECDFs, and violin spreads.",
        chart_type = "histogram",
        chart_col = matched_col
      ))
    } else {
      tab <- sort(table(as.character(v[!is.na(v)])), decreasing = TRUE)
      return(list(
        intent = "column_drilldown",
        headline = paste0("Column '", matched_col, "' (Categorical): ", length(tab), " distinct classes."),
        details = paste0("Dominant category is '", names(tab)[1], "' with ", tab[1], " observation(s)."),
        recommendation = "Standardize category casing or group rare labels in Workbench.",
        chart_type = "bar",
        chart_col = matched_col
      ))
    }
  }

  # ---- Fallback General Intelligence ----------------------------------------
  list(
    intent = "general",
    headline = paste0("Dataset holds ", format_number(profile$n_rows), " observations and ", profile$n_cols, " variables."),
    details = "Try asking: 'What are the key drivers?', 'Show outliers in income', 'Which columns have missing data?', or 'Give me an executive summary'.",
    recommendation = "Click any of the suggested query chips above for instant insights.",
    chart_type = "none",
    chart_col = NULL
  )
}
