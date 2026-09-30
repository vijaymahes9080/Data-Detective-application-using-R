# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/analysis_functions.R
# Description: Centralized analytical engines and statistical algorithms
# Shinylive / WebAssembly compatible (Base R & stats priority)
# ==============================================================================

# ---- 1. Dataset Inspection & Column Summaries -------------------------------

#' Profile entire dataset dimensions, memory, and per-column metrics
inspect_dataset <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      n_rows = 0,
      n_cols = 0,
      total_cells = 0,
      memory_bytes = 0,
      memory_formatted = "0 B",
      n_numeric = 0,
      n_categorical = 0,
      n_logical = 0,
      n_date = 0,
      columns_summary = data.frame()
    ))
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  total_cells <- n_rows * n_cols
  mem_bytes <- as.numeric(utils::object.size(df))

  col_names <- names(df)
  col_types <- vapply(df, detect_column_type, character(1))

  rows_list <- vector("list", n_cols)
  for (i in seq_len(n_cols)) {
    cname <- col_names[i]
    cdata <- df[[i]]
    ctype <- col_types[i]

    if (is.character(cdata)) {
      n_miss <- sum(is.na(cdata) | trimws(cdata) == "")
    } else if (is.numeric(cdata)) {
      n_miss <- sum(is.na(cdata) | is.nan(cdata))
    } else {
      n_miss <- sum(is.na(cdata))
    }
    miss_pct <- ifelse(n_rows > 0, (n_miss / n_rows) * 100, 0)

    clean_data <- cdata[!is.na(cdata)]
    if (is.character(clean_data)) clean_data <- clean_data[trimws(clean_data) != ""]
    n_unique <- length(unique(clean_data))
    unique_pct <- ifelse(n_rows > 0, (n_unique / n_rows) * 100, 0)

    val_min <- NA_character_
    val_max <- NA_character_
    val_mean <- NA_real_
    val_median <- NA_real_
    val_sd <- NA_real_
    val_skew <- NA_real_

    if (ctype %in% c("numeric", "integer")) {
      num_clean <- cdata[!is.na(cdata) & is.finite(cdata)]
      if (length(num_clean) > 0) {
        val_min <- as.character(round(min(num_clean), 3))
        val_max <- as.character(round(max(num_clean), 3))
        val_mean <- round(safe_mean(num_clean), 3)
        val_median <- round(safe_median(num_clean), 3)
        val_sd <- round(safe_sd(num_clean), 3)
        val_skew <- round(safe_skewness(num_clean), 3)
      }
    } else if (ctype %in% c("date", "datetime")) {
      d_clean <- cdata[!is.na(cdata)]
      if (length(d_clean) > 0) {
        val_min <- as.character(min(d_clean))
        val_max <- as.character(max(d_clean))
      }
    }

    # Example value
    example_val <- if (length(clean_data) > 0) as.character(clean_data[1]) else "-"

    rows_list[[i]] <- data.frame(
      column = cname,
      data_type = ctype,
      missing_count = n_miss,
      missing_pct = round(miss_pct, 2),
      non_missing_count = n_rows - n_miss,
      unique_count = n_unique,
      unique_pct = round(unique_pct, 2),
      min = val_min,
      max = val_max,
      mean = val_mean,
      median = val_median,
      sd = val_sd,
      skewness = val_skew,
      example = example_val,
      is_constant = (n_unique <= 1 && n_miss == 0),
      is_all_na = (n_miss == n_rows),
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, rows_list)

  return(list(
    n_rows = n_rows,
    n_cols = n_cols,
    total_cells = total_cells,
    memory_bytes = mem_bytes,
    memory_formatted = format_bytes(mem_bytes),
    n_numeric = sum(col_types %in% c("numeric", "integer")),
    n_categorical = sum(col_types %in% c("character", "factor")),
    n_logical = sum(col_types == "logical"),
    n_date = sum(col_types %in% c("date", "datetime", "date_candidate")),
    columns_summary = summary_df
  ))
}

# ---- 2. Missing Value Analysis ----------------------------------------------

#' Detect and classify missing values
detect_missing <- function(df, thresholds = list(low = 5, moderate = 20, high = 50)) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      total_missing = 0,
      total_cells = 0,
      missing_pct = 0,
      columns_with_missing = 0,
      complete_cases = 0,
      incomplete_cases = 0,
      column_summary = data.frame()
    ))
  }

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  total_cells <- n_rows * n_cols

  col_res <- lapply(names(df), function(col) {
    cdata <- df[[col]]
    if (is.character(cdata)) {
      n_miss <- sum(is.na(cdata) | trimws(cdata) == "")
    } else if (is.numeric(cdata)) {
      n_miss <- sum(is.na(cdata) | is.nan(cdata))
    } else {
      n_miss <- sum(is.na(cdata))
    }
    pct <- (n_miss / n_rows) * 100

    sev <- if (n_miss == 0) {
      "Complete"
    } else if (pct <= thresholds$low) {
      "Low"
    } else if (pct <= thresholds$moderate) {
      "Moderate"
    } else if (pct <= thresholds$high) {
      "High"
    } else {
      "Critical"
    }

    data.frame(
      column = col,
      missing_count = n_miss,
      missing_pct = round(pct, 2),
      non_missing_count = n_rows - n_miss,
      severity = sev,
      stringsAsFactors = FALSE
    )
  })

  col_df <- do.call(rbind, col_res)
  col_df <- col_df[order(-col_df$missing_count), ]
  rownames(col_df) <- NULL

  # Row missingness
  complete_idx <- stats::complete.cases(df)
  complete_count <- sum(complete_idx)

  return(list(
    total_missing = sum(col_df$missing_count),
    total_cells = total_cells,
    missing_pct = round((sum(col_df$missing_count) / total_cells) * 100, 2),
    columns_with_missing = sum(col_df$missing_count > 0),
    complete_cases = complete_count,
    incomplete_cases = n_rows - complete_count,
    complete_cases_pct = round((complete_count / n_rows) * 100, 2),
    column_summary = col_df,
    thresholds = thresholds
  ))
}

# ---- 3. Duplicate Records Detection -----------------------------------------

#' Detect exact and candidate duplicate records
detect_duplicates <- function(df, key_cols = NULL) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    return(list(
      total_rows = 0,
      unique_rows = 0,
      duplicate_rows_count = 0,
      duplicate_pct = 0,
      duplicate_records = data.frame(),
      has_duplicates = FALSE,
      potential_duplicates = data.frame()
    ))
  }

  n_rows <- nrow(df)
  dup_logical <- duplicated(df)
  all_dup_idx <- which(duplicated(df) | duplicated(df, fromLast = TRUE))

  n_dups <- sum(dup_logical)
  dup_pct <- round((n_dups / n_rows) * 100, 2)

  dup_records <- if (length(all_dup_idx) > 0) df[all_dup_idx, , drop = FALSE] else data.frame()

  # Near duplicates check (ignoring likely row IDs)
  pot_dups <- data.frame()
  if (ncol(df) >= 3) {
    id_regex <- "^(id|_id|uuid|index|row_id|no|num)$"
    non_id_cols <- names(df)[!grepl(id_regex, names(df), ignore.case = TRUE)]
    if (length(non_id_cols) >= 2 && length(non_id_cols) < ncol(df)) {
      sub_df <- df[, non_id_cols, drop = FALSE]
      sub_dup_idx <- which(duplicated(sub_df) | duplicated(sub_df, fromLast = TRUE))
      pot_idx <- setdiff(sub_dup_idx, all_dup_idx)
      if (length(pot_idx) > 0) pot_dups <- df[pot_idx, , drop = FALSE]
    }
  }

  return(list(
    total_rows = n_rows,
    unique_rows = n_rows - n_dups,
    duplicate_rows_count = n_dups,
    duplicate_pct = dup_pct,
    duplicate_records = dup_records,
    has_duplicates = (n_dups > 0),
    potential_duplicates = pot_dups,
    potential_count = nrow(pot_dups)
  ))
}

# ---- 4. Outlier Analysis (IQR & Z-Score) -------------------------------------

#' Detect statistical outliers on numeric features
detect_outliers <- function(df, iqr_multiplier = 1.5, z_threshold = 3.0) {
  num_cols <- names(df)[vapply(df, function(x) is.numeric(x) && !is.logical(x), logical(1))]

  if (length(num_cols) == 0 || nrow(df) == 0) {
    return(list(
      summary = data.frame(),
      has_numeric = FALSE,
      numeric_columns = character(0),
      total_outlier_variables = 0
    ))
  }

  summary_rows <- list()
  details_map <- list()

  for (col in num_cols) {
    vals <- df[[col]]
    clean_idx <- which(!is.na(vals) & is.finite(vals))
    clean_vals <- vals[clean_idx]
    n_clean <- length(clean_vals)

    if (n_clean < 4) next

    # IQR Method
    q <- stats::quantile(clean_vals, probs = c(0.25, 0.75), na.rm = TRUE)
    q1 <- as.numeric(q[1])
    q3 <- as.numeric(q[2])
    iqr_val <- q3 - q1
    lower_b <- q1 - (iqr_multiplier * iqr_val)
    upper_b <- q3 + (iqr_multiplier * iqr_val)

    is_out <- (clean_vals < lower_b) | (clean_vals > upper_b)
    n_iqr_outliers <- sum(is_out)
    iqr_pct <- round((n_iqr_outliers / n_clean) * 100, 2)

    # Z-Score Method
    m <- mean(clean_vals)
    s <- stats::sd(clean_vals)
    n_z_outliers <- 0
    z_pct <- 0
    if (!is.na(s) && s > 0) {
      z_sc <- abs((clean_vals - m) / s)
      n_z_outliers <- sum(z_sc > z_threshold)
      z_pct <- round((n_z_outliers / n_clean) * 100, 2)
    }

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      column = col,
      valid_count = n_clean,
      q1 = round(q1, 3),
      q3 = round(q3, 3),
      iqr = round(iqr_val, 3),
      lower_bound = round(lower_b, 3),
      upper_bound = round(upper_b, 3),
      outlier_count = n_iqr_outliers,
      outlier_pct = iqr_pct,
      z_outlier_count = n_z_outliers,
      z_outlier_pct = z_pct,
      stringsAsFactors = FALSE
    )

    out_row_ids <- clean_idx[is_out]
    details_map[[col]] <- list(
      q1 = q1,
      q3 = q3,
      iqr = iqr_val,
      lower_bound = lower_b,
      upper_bound = upper_b,
      outlier_indices = out_row_ids,
      outlier_values = vals[out_row_ids]
    )
  }

  summary_df <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
  if (nrow(summary_df) > 0) {
    summary_df <- summary_df[order(-summary_df$outlier_count), ]
    rownames(summary_df) <- NULL
  }

  return(list(
    summary = summary_df,
    details = details_map,
    has_numeric = TRUE,
    numeric_columns = num_cols,
    total_outlier_variables = sum(summary_df$outlier_count > 0),
    iqr_multiplier = iqr_multiplier
  ))
}

# ---- 5. Correlation & Multicollinearity -------------------------------------

#' Compute correlation matrix and flag multicollinear pairs
calculate_correlations <- function(df, method = "pearson", strong_thresh = 0.70) {
  num_cols <- names(df)[vapply(df, function(x) is.numeric(x) && !is.logical(x), logical(1))]

  valid_cols <- character(0)
  for (col in num_cols) {
    v <- df[[col]][!is.na(df[[col]]) & is.finite(df[[col]])]
    if (length(v) >= 4) {
      variance <- stats::var(v)
      if (!is.na(variance) && variance > 1e-9) valid_cols <- c(valid_cols, col)
    }
  }

  if (length(valid_cols) < 2) {
    return(list(
      has_correlations = FALSE,
      matrix = matrix(numeric(0)),
      pairs = data.frame(),
      multicollinear_pairs = data.frame(),
      numeric_columns = valid_cols
    ))
  }

  sub_df <- df[, valid_cols, drop = FALSE]
  c_mat <- suppressWarnings(stats::cor(sub_df, use = "pairwise.complete.obs", method = method))
  c_mat_round <- round(c_mat, 3)

  n <- length(valid_cols)
  pair_rows <- list()

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      r_val <- c_mat[i, j]
      if (is.na(r_val) || is.nan(r_val)) next

      abs_r <- abs(r_val)
      strength <- if (abs_r >= 0.90) {
        "Very Strong Association"
      } else if (abs_r >= strong_thresh) {
        "Strong Association"
      } else if (abs_r >= 0.40) {
        "Moderate Association"
      } else {
        "Weak / Negligible"
      }

      pair_rows[[length(pair_rows) + 1]] <- data.frame(
        variable_a = valid_cols[i],
        variable_b = valid_cols[j],
        correlation = round(r_val, 3),
        abs_r = round(abs_r, 3),
        strength = strength,
        is_strong = (abs_r >= strong_thresh),
        is_very_strong = (abs_r >= 0.90),
        stringsAsFactors = FALSE
      )
    }
  }

  pairs_df <- if (length(pair_rows) > 0) do.call(rbind, pair_rows) else data.frame()
  if (nrow(pairs_df) > 0) {
    pairs_df <- pairs_df[order(-pairs_df$abs_r), ]
    rownames(pairs_df) <- NULL
  }

  multi_df <- if (nrow(pairs_df) > 0) pairs_df[pairs_df$abs_r >= strong_thresh, , drop = FALSE] else data.frame()

  return(list(
    has_correlations = TRUE,
    matrix = c_mat_round,
    pairs = pairs_df,
    multicollinear_pairs = multi_df,
    numeric_columns = valid_cols,
    method = method,
    strong_thresh = strong_thresh
  ))
}

# ---- 6. Constant Columns & High Cardinality ---------------------------------

#' Detect constant and uninformative columns
detect_constant_columns <- function(df, concentration_threshold = 0.90) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(data.frame())

  n_rows <- nrow(df)
  res_list <- list()

  for (col in names(df)) {
    vals <- df[[col]]
    non_na <- vals[!is.na(vals)]
    if (length(non_na) == 0) {
      res_list[[length(res_list) + 1]] <- data.frame(
        column = col,
        unique_count = 0,
        dominant_value = "ALL NA",
        dominant_pct = 100.0,
        issue_type = "All Missing",
        finding = "Column contains 100% missing values.",
        stringsAsFactors = FALSE
      )
      next
    }

    n_uniq <- length(unique(non_na))
    if (n_uniq == 1) {
      res_list[[length(res_list) + 1]] <- data.frame(
        column = col,
        unique_count = 1,
        dominant_value = as.character(non_na[1]),
        dominant_pct = round((length(non_na) / n_rows) * 100, 2),
        issue_type = "Strictly Constant",
        finding = paste0("Contains only one unique value: '", non_na[1], "'."),
        stringsAsFactors = FALSE
      )
      next
    }

    freq_tab <- table(non_na)
    dom_count <- max(freq_tab)
    dom_val <- names(freq_tab)[which.max(freq_tab)]
    dom_pct <- round((dom_count / n_rows) * 100, 2)

    if (dom_pct >= (concentration_threshold * 100)) {
      res_list[[length(res_list) + 1]] <- data.frame(
        column = col,
        unique_count = n_uniq,
        dominant_value = dom_val,
        dominant_pct = dom_pct,
        issue_type = "Near-Constant",
        finding = paste0(dom_pct, "% of records share identical value: '", dom_val, "'."),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(res_list) > 0) do.call(rbind, res_list) else data.frame()
}

#' Detect high-cardinality categorical variables
detect_high_cardinality <- function(df, threshold_pct = 0.80) {
  n_rows <- nrow(df)
  if (n_rows < 10) return(data.frame())

  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
  res <- list()

  for (col in cat_cols) {
    n_uniq <- length(unique(df[[col]][!is.na(df[[col]])]))
    uniq_ratio <- n_uniq / n_rows
    if (uniq_ratio >= threshold_pct) {
      res[[length(res) + 1]] <- data.frame(
        column = col,
        unique_count = n_uniq,
        unique_ratio = round(uniq_ratio * 100, 1),
        finding = "High cardinality: nearly every row has a distinct value (potential unique ID).",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(res) > 0) do.call(rbind, res) else data.frame()
}

# ---- 7. Data Leakage & Representation Indicators ----------------------------

#' Detect potential data leakage indicators
detect_leakage_indicators <- function(df, target_col = NULL) {
  if (is.null(target_col) || !nzchar(trimws(target_col)) || !(target_col %in% names(df))) {
    return(list(
      target_specified = FALSE,
      indicators = data.frame()
    ))
  }

  target_vals <- df[[target_col]]
  indicators <- list()
  other_cols <- setdiff(names(df), target_col)

  clean_target <- tolower(gsub("[^a-z0-9]", "", target_col))

  for (col in other_cols) {
    cvals <- df[[col]]

    # 1. Name resemblance
    clean_col <- tolower(gsub("[^a-z0-9]", "", col))
    if (nchar(clean_target) >= 3 && grepl(clean_target, clean_col, fixed = TRUE)) {
      indicators[[length(indicators) + 1]] <- data.frame(
        column = col,
        target = target_col,
        indicator_type = "Name Resemblance",
        severity = "MEDIUM",
        evidence = paste0("Column name '", col, "' embeds target name '", target_col, "'."),
        reason = "Feature name may denote post-target derivation or leaky data pipeline.",
        stringsAsFactors = FALSE
      )
    }

    # 2. Near-Identity
    valid_idx <- which(!is.na(cvals) & !is.na(target_vals))
    if (length(valid_idx) >= 10) {
      match_pct <- sum(cvals[valid_idx] == target_vals[valid_idx]) / length(valid_idx)
      if (match_pct >= 0.99) {
        indicators[[length(indicators) + 1]] <- data.frame(
          column = col,
          target = target_col,
          indicator_type = "Near Identity",
          severity = "HIGH",
          evidence = paste0(round(match_pct * 100, 1), "% identical non-missing values."),
          reason = "Feature is virtually identical to target variable.",
          stringsAsFactors = FALSE
        )
      }
    }

    # 3. Extreme Correlation
    if (is.numeric(cvals) && is.numeric(target_vals)) {
      clean_both <- which(!is.na(cvals) & !is.na(target_vals) & is.finite(cvals) & is.finite(target_vals))
      if (length(clean_both) >= 10) {
        r <- suppressWarnings(stats::cor(cvals[clean_both], target_vals[clean_both]))
        if (!is.na(r) && abs(r) >= 0.95) {
          indicators[[length(indicators) + 1]] <- data.frame(
            column = col,
            target = target_col,
            indicator_type = "Extreme Correlation",
            severity = "HIGH",
            evidence = paste0("Pearson |r| = ", round(abs(r), 3)),
            reason = "Near-perfect linear co-movement with target.",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  ind_df <- if (length(indicators) > 0) do.call(rbind, indicators) else data.frame()

  return(list(
    target_specified = TRUE,
    target_column = target_col,
    indicators = ind_df
  ))
}

#' Detect potential bias and representation indicators
detect_bias_indicators <- function(df, group_col = NULL, target_col = NULL) {
  indicators <- list()
  cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]

  if (length(cat_cols) == 0 || nrow(df) == 0) {
    return(list(has_indicators = FALSE, indicators = data.frame()))
  }

  for (col in cat_cols) {
    vals <- df[[col]]
    non_na <- vals[!is.na(vals) & trimws(as.character(vals)) != ""]
    if (length(non_na) < 10) next

    tab <- table(non_na)
    dom_count <- max(tab)
    dom_cat <- names(tab)[which.max(tab)]
    dom_pct <- round((dom_count / length(non_na)) * 100, 1)

    if (dom_pct >= 90.0 && length(tab) > 1) {
      indicators[[length(indicators) + 1]] <- data.frame(
        column = col,
        indicator_type = "Class Imbalance",
        severity = "MEDIUM",
        evidence = paste0("Dominant category '", dom_cat, "' represents ", dom_pct, "% of records."),
        reason = "Severe category concentration in sub-population.",
        stringsAsFactors = FALSE
      )
    }
  }

  ind_df <- if (length(indicators) > 0) do.call(rbind, indicators) else data.frame()

  return(list(
    has_indicators = (nrow(ind_df) > 0),
    indicators = ind_df
  ))
}

# ---- 8. Data Quality Indicator Scoring --------------------------------------

#' Calculate composite Data Quality Indicator (0-100) with transparent deductions
calculate_quality_indicator <- function(profile, missing_res, dup_res, out_res, const_df) {
  if (is.null(profile) || profile$n_rows == 0) {
    return(list(score = 0, status = "No Data", deductions = list()))
  }

  base_score <- 100
  deductions <- list()

  # Missing deduction (max 35)
  m_pct <- missing_res$missing_pct
  m_points <- min(35, round(m_pct * 0.7, 1))
  if (m_points > 0) {
    deductions$missing <- list(name = "Missing Data", points = m_points, metric = paste0(m_pct, "% missing cells"))
  }

  # Duplicates deduction (max 25)
  d_pct <- dup_res$duplicate_pct
  d_points <- min(25, round(d_pct * 0.8, 1))
  if (d_points > 0) {
    deductions$duplicates <- list(name = "Duplicate Rows", points = d_points, metric = paste0(d_pct, "% duplicate rows"))
  }

  # Outliers deduction (max 15)
  if (out_res$has_numeric && nrow(out_res$summary) > 0) {
    n_out_cols <- sum(out_res$summary$outlier_count > 0)
    out_points <- min(15, round((n_out_cols / nrow(out_res$summary)) * 15, 1))
    if (out_points > 0) {
      deductions$outliers <- list(name = "Statistical Outliers", points = out_points, metric = paste0(n_out_cols, " variable(s) contain outliers"))
    }
  }

  # Constant columns deduction (max 15)
  if (nrow(const_df) > 0) {
    c_points <- min(15, nrow(const_df) * 5)
    deductions$constants <- list(name = "Uninformative Variables", points = c_points, metric = paste0(nrow(const_df), " constant/near-constant columns"))
  }

  total_penalty <- sum(vapply(deductions, function(d) d$points, numeric(1)))
  final_score <- max(0, min(100, round(base_score - total_penalty, 0)))

  status <- if (final_score >= 90) "Excellent" else if (final_score >= 75) "Good" else if (final_score >= 50) "Needs Attention" else "Poor"
  status_color <- switch(status, "Excellent" = "#10b981", "Good" = "#3b82f6", "Needs Attention" = "#f59e0b", "Poor" = "#ef4444")

  return(list(
    score = final_score,
    status = status,
    status_color = status_color,
    total_deductions = total_penalty,
    deductions = deductions
  ))
}

# ---- 9. Unified Finding Engine ----------------------------------------------

#' Generate unified findings dossier (category, severity, column, message, evidence, recommendation)
generate_findings <- function(profile, missing_res, dup_res, out_res, const_df, corr_res, leak_res, bias_res) {
  f_list <- list()

  add_f <- function(category, severity, column, message, evidence, recommendation) {
    f_list[[length(f_list) + 1]] <<- list(
      category = category,
      severity = toupper(severity),
      column = column,
      message = message,
      evidence = evidence,
      recommendation = recommendation
    )
  }

  # Missing values
  if (nrow(missing_res$column_summary) > 0) {
    for (i in seq_len(nrow(missing_res$column_summary))) {
      r <- missing_res$column_summary[i, ]
      if (r$missing_count > 0) {
        sev <- if (r$missing_pct > 50) "HIGH" else if (r$missing_pct > 20) "HIGH" else if (r$missing_pct > 5) "MEDIUM" else "LOW"
        add_f(
          category = "Missing Values",
          severity = sev,
          column = r$column,
          message = paste0("Column '", r$column, "' contains ", r$missing_pct, "% missing values."),
          evidence = paste0(format_number(r$missing_count), " of ", format_number(profile$n_rows), " rows missing."),
          recommendation = "Investigate the missingness mechanism before modeling."
        )
      }
    }
  }

  # Duplicates
  if (dup_res$duplicate_rows_count > 0) {
    sev <- if (dup_res$duplicate_pct >= 5) "HIGH" else if (dup_res$duplicate_pct >= 1) "MEDIUM" else "LOW"
    add_f(
      category = "Duplicate Records",
      severity = sev,
      column = "All Columns",
      message = paste0(dup_res$duplicate_pct, "% of rows are exact duplicate copies."),
      evidence = paste0(format_number(dup_res$duplicate_rows_count), " duplicate records detected."),
      recommendation = "Inspect duplicate rows to determine if they stem from logging redundancy."
    )
  }

  # Constants
  if (nrow(const_df) > 0) {
    for (i in seq_len(nrow(const_df))) {
      r <- const_df[i, ]
      sev <- if (r$issue_type == "Strictly Constant") "HIGH" else "MEDIUM"
      add_f(
        category = "Uninformative Variables",
        severity = sev,
        column = r$column,
        message = r$finding,
        evidence = paste0(r$issue_type, ": Dominant value represents ", r$dominant_pct, "%."),
        recommendation = "Consider excluding uninformative features from predictive models."
      )
    }
  }

  # Outliers
  if (out_res$has_numeric && nrow(out_res$summary) > 0) {
    for (i in seq_len(nrow(out_res$summary))) {
      r <- out_res$summary[i, ]
      if (r$outlier_count > 0) {
        sev <- if (r$outlier_pct >= 10) "MEDIUM" else "LOW"
        add_f(
          category = "Outliers",
          severity = sev,
          column = r$column,
          message = paste0(r$outlier_pct, "% statistically unusual observations detected (beyond 1.5 \u00d7 IQR)."),
          evidence = paste0(format_number(r$outlier_count), " observations lie outside bounds [", r$lower_bound, ", ", r$upper_bound, "]."),
          recommendation = "Inspect flagged records to verify data entry accuracy."
        )
      }
    }
  }

  # Multicollinearity
  if (corr_res$has_correlations && nrow(corr_res$multicollinear_pairs) > 0) {
    for (i in seq_len(nrow(corr_res$multicollinear_pairs))) {
      r <- corr_res$multicollinear_pairs[i, ]
      sev <- if (r$is_very_strong) "HIGH" else "MEDIUM"
      add_f(
        category = "Correlations",
        severity = sev,
        column = paste0(r$variable_a, " & ", r$variable_b),
        message = paste0(r$strength, " (r = ", r$correlation, ")."),
        evidence = paste0("Pearson correlation coefficient is ", r$correlation),
        recommendation = "Assess linear redundancy before training regression models."
      )
    }
  }

  # Leakage
  if (!is.null(leak_res) && leak_res$target_specified && nrow(leak_res$indicators) > 0) {
    for (i in seq_len(nrow(leak_res$indicators))) {
      r <- leak_res$indicators[i, ]
      add_f(
        category = "Leakage Indicators",
        severity = r$severity,
        column = r$column,
        message = r$reason,
        evidence = r$evidence,
        recommendation = "Verify if this feature is recorded post-event relative to the target."
      )
    }
  }

  # Bias
  if (!is.null(bias_res) && bias_res$has_indicators && nrow(bias_res$indicators) > 0) {
    for (i in seq_len(nrow(bias_res$indicators))) {
      r <- bias_res$indicators[i, ]
      add_f(
        category = "Bias Indicators",
        severity = r$severity,
        column = r$column,
        message = r$reason,
        evidence = r$evidence,
        recommendation = "Verify whether representation coverage aligns with intended domain scope."
      )
    }
  }

  f_df <- if (length(f_list) > 0) {
    do.call(rbind, lapply(f_list, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(
      category = character(0), severity = character(0),
      column = character(0), message = character(0),
      evidence = character(0), recommendation = character(0),
      stringsAsFactors = FALSE
    )
  }

  return(f_df)
}

# ---- Backward Compatibility Aliases -----------------------------------------
profile_dataset <- inspect_dataset
analyze_missing <- detect_missing
analyze_duplicates <- detect_duplicates
analyze_constants <- detect_constant_columns

