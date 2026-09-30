# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/anomaly_analysis.R
# Description: Data leakage indicators, representation indicators, target & group analysis
# ==============================================================================

# ---- Section 19 & 20: Target & Leakage Analysis -----------------------------

#' Analyze potential data leakage indicators relative to a target variable
#' @param df Data frame
#' @param target_col Target column name selected by user (optional)
#' @return A list with leakage indicators, target profile, and explanations
analyze_leakage_indicators <- function(df, target_col = NULL) {
  if (is.null(target_col) || !nzchar(trimws(target_col)) || !(target_col %in% names(df))) {
    return(list(
      target_specified = FALSE,
      indicators = data.frame(),
      message = "Target variable not specified. Leakage analysis is limited.",
      wording_note = "Potential leakage indicators are investigation aids, not proof of leakage."
    ))
  }

  target_vals <- df[[target_col]]
  indicators_list <- list()
  other_cols <- setdiff(names(df), target_col)

  # 1. Target Name Similarity Check
  clean_target_name <- tolower(gsub("[^a-z0-9]", "", target_col))
  for (col in other_cols) {
    clean_col <- tolower(gsub("[^a-z0-9]", "", col))
    if (nchar(clean_target_name) >= 3 && grepl(clean_target_name, clean_col, fixed = TRUE)) {
      indicators_list[[length(indicators_list) + 1]] <- data.frame(
        column = col,
        target = target_col,
        indicator_type = "Name Resemblance",
        severity = "WARNING",
        evidence = paste0("Column name '", col, "' embeds target name '", target_col, "'."),
        finding = "Potential leakage indicator detected: Feature name may denote post-target information or direct derivations.",
        stringsAsFactors = FALSE
      )
    }
  }

  # 2. Near-Identity / Exact Duplication with Target
  for (col in other_cols) {
    col_vals <- df[[col]]
    # Check if identical (excluding NAs)
    valid_idx <- which(!is.na(col_vals) & !is.na(target_vals))
    if (length(valid_idx) >= 10) {
      match_pct <- sum(col_vals[valid_idx] == target_vals[valid_idx]) / length(valid_idx)
      if (match_pct >= 0.99) {
        indicators_list[[length(indicators_list) + 1]] <- data.frame(
          column = col,
          target = target_col,
          indicator_type = "Near Identity",
          severity = "HIGH",
          evidence = paste0(round(match_pct * 100, 1), "% identical non-missing values with target."),
          finding = "Potential leakage indicator detected: Feature is virtually identical to target variable.",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # 3. Extreme Numeric Correlation with Target
  if (is.numeric(target_vals)) {
    num_clean_t <- target_vals[!is.na(target_vals) & is.finite(target_vals)]
    if (length(num_clean_t) >= 10 && stats::sd(num_clean_t) > 0) {
      for (col in other_cols) {
        if (is.numeric(df[[col]])) {
          valid_both <- which(!is.na(df[[col]]) & !is.na(target_vals) & is.finite(df[[col]]) & is.finite(target_vals))
          if (length(valid_both) >= 10) {
            s_col <- stats::sd(df[[col]][valid_both])
            if (!is.na(s_col) && s_col > 0) {
              r <- suppressWarnings(stats::cor(df[[col]][valid_both], target_vals[valid_both]))
              if (!is.na(r) && abs(r) >= 0.95) {
                indicators_list[[length(indicators_list) + 1]] <- data.frame(
                  column = col,
                  target = target_col,
                  indicator_type = "Extreme Linear Association",
                  severity = "HIGH",
                  evidence = paste0("Pearson correlation |r| = ", round(abs(r), 3)),
                  finding = "Potential leakage indicator detected: Variable exhibits near-perfect linear association with target.",
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
    }
  }

  indicators_df <- if (length(indicators_list) > 0) {
    do.call(rbind, indicators_list)
  } else {
    data.frame(
      column = character(0),
      target = character(0),
      indicator_type = character(0),
      severity = character(0),
      evidence = character(0),
      finding = character(0),
      stringsAsFactors = FALSE
    )
  }

  return(list(
    target_specified = TRUE,
    target_column = target_col,
    indicators = indicators_df,
    has_leakage_flags = (nrow(indicators_df) > 0),
    message = if (nrow(indicators_df) > 0) {
      paste0(nrow(indicators_df), " potential leakage indicator(s) identified.")
    } else {
      "No overt leakage indicators detected based on standard heuristics."
    },
    wording_note = "Potential leakage indicators are exploratory signals, never proof of leakage."
  ))
}

# ---- Section 21: Potential Data Representation Indicators -------------------

#' Analyze potential data representation issues and category-level missingness
#' @param df Data frame
#' @return A list with representation indicators and differential missingness findings
analyze_representation_indicators <- function(df) {
  indicators <- list()
  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]

  if (length(cat_cols) == 0 || nrow(df) == 0) {
    return(list(
      has_representation_issues = FALSE,
      indicators = data.frame(),
      message = "No categorical variables available for representation analysis."
    ))
  }

  n_rows <- nrow(df)

  for (col in cat_cols) {
    vals <- df[[col]]
    non_na <- vals[!is.na(vals) & trimws(as.character(vals)) != ""]
    if (length(non_na) < 10) next

    tab <- table(non_na)
    dom_count <- max(tab)
    dom_cat <- names(tab)[which.max(tab)]
    dom_pct <- round((dom_count / length(non_na)) * 100, 1)

    # 1. Severe Category Imbalance
    if (dom_pct >= 90.0 && length(tab) > 1) {
      indicators[[length(indicators) + 1]] <- data.frame(
        column = col,
        category = dom_cat,
        indicator_type = "Category Imbalance",
        severity = "WARNING",
        evidence = paste0("Dominant category '", dom_cat, "' comprises ", dom_pct, "% (", dom_count, "/", length(non_na), ") of observations."),
        finding = "Potential representation issue requiring further investigation: Heavy concentration in a single category.",
        stringsAsFactors = FALSE
      )
    }

    # 2. Check for differential missingness across categories in other columns
    # Find columns with moderate missingness
    other_cols <- setdiff(names(df), col)
    for (ocol in other_cols) {
      if (sum(is.na(df[[ocol]])) > 10 && sum(!is.na(df[[ocol]])) > 10) {
        # Cross-tab missingness by category
        split_miss <- tapply(is.na(df[[ocol]]), vals, function(m) c(missing = sum(m), total = length(m)))
        rates <- vapply(split_miss, function(k) if (!is.null(k) && k["total"] >= 5) k["missing"] / k["total"] else NA_real_, numeric(1))
        valid_rates <- rates[!is.na(rates)]
        if (length(valid_rates) >= 2) {
          rate_diff <- max(valid_rates) - min(valid_rates)
          if (rate_diff >= 0.30) {
            # At least 30% difference in missingness rate across groups
            max_grp <- names(valid_rates)[which.max(valid_rates)]
            min_grp <- names(valid_rates)[which.min(valid_rates)]
            indicators[[length(indicators) + 1]] <- data.frame(
              column = col,
              category = paste0(ocol, " by ", col),
              indicator_type = "Differential Missingness",
              severity = "WARNING",
              evidence = paste0("Missingness of '", ocol, "' is ", round(max(valid_rates) * 100, 1), "% in group '", max_grp, "' vs ", round(min(valid_rates) * 100, 1), "% in '", min_grp, "'."),
              finding = "Potential representation issue requiring further investigation: Missing data is disproportionately concentrated in certain sub-groups.",
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  indicators_df <- if (length(indicators) > 0) {
    do.call(rbind, indicators)
  } else {
    data.frame(
      column = character(0),
      category = character(0),
      indicator_type = character(0),
      severity = character(0),
      evidence = character(0),
      finding = character(0),
      stringsAsFactors = FALSE
    )
  }

  return(list(
    has_representation_issues = (nrow(indicators_df) > 0),
    indicators = indicators_df,
    wording_note = "Potential representation issues reflect observed statistical distributions only. They do not infer discrimination, unfairness, intent, or causality."
  ))
}

# ---- Section 22: Group Comparison ------------------------------------------

#' Compare a numeric metric across groups of a categorical variable
#' @param df Dataset
#' @param group_col Categorical column name
#' @param num_col Numeric column name
#' @return Data frame comparing group count, mean, median, missing %, outlier %
compare_groups <- function(df, group_col, num_col) {
  if (is.null(df[[group_col]]) || is.null(df[[num_col]])) return(data.frame())

  g_vals <- as.character(df[[group_col]])
  n_vals <- df[[num_col]]

  # Remove NAs in grouping
  valid_g <- !is.na(g_vals) & trimws(g_vals) != ""
  if (sum(valid_g) < 2) return(data.frame())

  g_factor <- factor(g_vals[valid_g])
  n_sub <- n_vals[valid_g]

  # Calculate global IQR bounds for outlier detection
  clean_all <- n_sub[!is.na(n_sub) & is.finite(n_sub)]
  global_outlier_count <- 0
  lower_b <- -Inf
  upper_b <- Inf
  if (length(clean_all) >= 4) {
    q <- stats::quantile(clean_all, probs = c(0.25, 0.75))
    iqr_v <- q[2] - q[1]
    lower_b <- q[1] - 1.5 * iqr_v
    upper_b <- q[2] + 1.5 * iqr_v
  }

  groups <- levels(g_factor)
  rows <- list()

  for (grp in groups) {
    idx <- which(g_factor == grp)
    grp_vals <- n_sub[idx]
    n_total <- length(grp_vals)
    n_miss <- sum(is.na(grp_vals))
    grp_clean <- grp_vals[!is.na(grp_vals) & is.finite(grp_vals)]

    m_val <- safe_mean(grp_clean)
    med_val <- safe_median(grp_clean)
    sd_val <- safe_sd(grp_clean)

    n_out <- if (length(grp_clean) > 0) sum(grp_clean < lower_b | grp_clean > upper_b) else 0

    rows[[length(rows) + 1]] <- data.frame(
      Group = grp,
      Total_Count = n_total,
      Valid_Count = length(grp_clean),
      Missing_Pct = round((n_miss / n_total) * 100, 1),
      Mean = round(m_val, 2),
      Median = round(med_val, 2),
      SD = round(sd_val, 2),
      Outliers_Count = n_out,
      Outliers_Pct = ifelse(length(grp_clean) > 0, round((n_out / length(grp_clean)) * 100, 1), 0),
      stringsAsFactors = FALSE
    )
  }

  res_df <- do.call(rbind, rows)
  # Sort descending by Total_Count
  res_df <- res_df[order(-res_df$Total_Count), ]
  rownames(res_df) <- NULL

  return(res_df)
}

# ---- Section 23: Date / Time Analysis ---------------------------------------

#' Analyze date and temporal columns in dataset
#' @param df Data frame
#' @return A list with date columns detected, date ranges, and time distribution metrics
analyze_dates <- function(df) {
  date_cols <- character(0)
  for (col in names(df)) {
    if (inherits(df[[col]], "Date") || inherits(df[[col]], "POSIXt")) {
      date_cols <- c(date_cols, col)
    }
  }

  if (length(date_cols) == 0) {
    return(list(
      has_dates = FALSE,
      date_columns = character(0),
      summary = data.frame()
    ))
  }

  summary_rows <- list()
  for (col in date_cols) {
    v <- df[[col]]
    valid_dates <- v[!is.na(v)]
    n_valid <- length(valid_dates)
    n_miss <- sum(is.na(v))
    min_d <- if (n_valid > 0) min(valid_dates) else NA
    max_d <- if (n_valid > 0) max(valid_dates) else NA
    days_range <- if (n_valid > 0) as.numeric(difftime(max_d, min_d, units = "days")) else NA_real_

    # Duplicates in dates
    dup_dates <- if (n_valid > 0) sum(duplicated(valid_dates)) else 0

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      column = col,
      valid_count = n_valid,
      missing_count = n_miss,
      missing_pct = round((n_miss / length(v)) * 100, 1),
      min_date = as.character(min_d),
      max_date = as.character(max_d),
      span_days = days_range,
      duplicate_date_records = dup_dates,
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, summary_rows)

  return(list(
    has_dates = TRUE,
    date_columns = date_cols,
    summary = summary_df,
    wording_note = "Do not assume time series meaning without domain confirmation."
  ))
}
