# ==============================================================================
# DATA DETECTIVE PRO: Autonomous Smart Recipe Engine
# File: R/recipe_engine.R
# Description: Synthesizes, executes, and exports one-click raw-to-useful recipes
# Shinylive / WebAssembly compatible (Pure Base R)
# ==============================================================================

# ---- 1. Smart Recipe Synthesizer --------------------------------------------

#' Automatically inspect dataset defects and synthesize an ordered raw-to-useful recipe
#' @param df Data frame to inspect
#' @param profile Output of inspect_dataset(df)
#' @param findings Output of generate_findings(...)
#' @return List of recipe step objects
synthesize_smart_recipe <- function(df, profile = NULL, findings = NULL) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(list())

  if (is.null(profile)) profile <- inspect_dataset(df)
  recipe <- list()
  step_counter <- 1

  add_step <- function(category, title, description, col, action, params = list()) {
    recipe[[length(recipe) + 1]] <<- list(
      id = paste0("step_", step_counter),
      category = category,
      title = title,
      description = description,
      column = col,
      action = action,
      params = params,
      enabled = TRUE
    )
    step_counter <<- step_counter + 1
  }

  # Step 0: Column Header Sanitization
  raw_names <- names(df)
  needs_name_clean <- any(grepl("[[:space:]%#$€£¥/\\(\\)-]", raw_names)) || any(grepl("^[0-9]", raw_names))
  if (needs_name_clean) {
    add_step(
      category = "Header Standardization",
      title = "Sanitize Column Headers to snake_case",
      description = "Normalize column names by eliminating whitespace, illegal punctuation, and ensuring unique database-grade snake_case identifiers.",
      col = "All Headers",
      action = "clean_names",
      params = list(case_style = "snake_case")
    )
  }

  # Step 1: Duplicate Rows
  dup_rows <- sum(duplicated(df))
  if (dup_rows > 0) {
    add_step(
      category = "Deduplication",
      title = "Eliminate Exact Duplicate Rows",
      description = paste0("Remove ", dup_rows, " redundant duplicate row(s) to restore genuine sample weights."),
      col = "All Columns",
      action = "deduplicate_exact",
      params = list(keep_rule = "first")
    )
  }

  # Step 2: Currency & Percentage String Coercion
  for (cn in names(df)) {
    v <- df[[cn]]
    if (is.character(v)) {
      non_na_v <- v[!is.na(v) & trimws(v) != ""]
      if (length(non_na_v) > 0) {
        has_curr <- any(grepl("^\\s*[$€£¥]", non_na_v))
        has_pct <- any(grepl("%\\s*$", non_na_v))
        if (has_curr || has_pct) {
          add_step(
            category = "Type Casting",
            title = paste0("Coerce '", cn, "' to Numeric (Strip Currency/%)"),
            description = paste0("Parse monetary or percentage text into clean continuous numeric floats."),
            col = cn,
            action = "currency_percent",
            params = list()
          )
        }
      }
    }
  }

  # Step 3: Inconsistent Whitespace & Text
  cats_incons <- tryCatch(detect_category_inconsistencies(df), error = function(e) data.frame())
  if (nrow(cats_incons) > 0) {
    ws_cols <- cats_incons$column[cats_incons$issue_type == "Whitespace Padding"]
    for (cn in unique(ws_cols)) {
      add_step(
        category = "Sanitization",
        title = paste0("Trim Whitespace in '", cn, "'"),
        description = "Remove rogue leading and trailing spaces.",
        col = cn,
        action = "trim_whitespace",
        params = list()
      )
    }

    casing_cols <- cats_incons$column[cats_incons$issue_type == "Inconsistent Casing"]
    for (cn in unique(casing_cols)) {
      add_step(
        category = "Normalization",
        title = paste0("Normalize Casing in '", cn, "'"),
        description = "Standardize varied category spellings into Title Case.",
        col = cn,
        action = "case_title",
        params = list()
      )
    }
  }

  # Step 4: Type Mismatches
  type_mismatches <- tryCatch(detect_type_mismatches(df), error = function(e) data.frame())
  if (nrow(type_mismatches) > 0) {
    for (i in seq_len(nrow(type_mismatches))) {
      r <- type_mismatches[i, ]
      add_step(
        category = "Type Casting",
        title = paste0("Cast '", r$column, "' to ", toupper(r$suggested_type)),
        description = r$reason,
        col = r$column,
        action = paste0("cast_", r$suggested_type),
        params = list(target_type = r$suggested_type)
      )
    }
  }

  # Step 5: Missing Values
  miss_res <- detect_missing(df)
  if (nrow(miss_res$column_summary) > 0) {
    for (i in seq_len(nrow(miss_res$column_summary))) {
      r <- miss_res$column_summary[i, ]
      if (r$missing_count > 0) {
        matched <- profile$columns_summary[profile$columns_summary$column == r$column, ]
        is_num <- if (nrow(matched) > 0) matched$data_type[1] %in% c("numeric", "integer") else is.numeric(df[[r$column]])
        
        meth <- if (is_num) "median" else "mode"
        add_step(
          category = "Imputation",
          title = paste0("Impute Missing in '", r$column, "' (", toupper(meth), ")"),
          description = paste0("Fill ", r$missing_count, " missing observation(s) with statistical ", meth, "."),
          col = r$column,
          action = paste0("impute_", meth),
          params = list(method = meth)
        )
      }
    }
  }

  # Step 6: Extreme Outlier Winsorization
  out_res <- detect_outliers(df)
  if (out_res$has_numeric && nrow(out_res$summary) > 0) {
    for (i in seq_len(nrow(out_res$summary))) {
      r <- out_res$summary[i, ]
      if (r$outlier_count > 0) {
        add_step(
          category = "Outlier Winsorization",
          title = paste0("Winsorize Outliers in '", r$column, "'"),
          description = paste0("Cap ", r$outlier_count, " extreme value(s) outside [", r$lower_bound, ", ", r$upper_bound, "] to preserve rows without variance distortion."),
          col = r$column,
          action = "winsorize_iqr",
          params = list(threshold = 1.5)
        )
      }
    }
  }

  # Step 7: Feature Enrichment (Dates, Cyclical, & Highly Skewed Features)
  for (cn in names(df)) {
    v <- df[[cn]]
    ctype <- detect_column_type(v)
    if (ctype %in% c("date", "datetime", "date_candidate")) {
      add_step(
        category = "Feature Enrichment",
        title = paste0("Extract Date Features from '", cn, "'"),
        description = "Derive Year, Month, Quarter, DayOfWeek, and IsWeekend indicator columns.",
        col = cn,
        action = "enrich_date",
        params = list()
      )
    } else if (ctype %in% c("numeric", "integer")) {
      clean_v <- v[!is.na(v) & is.finite(v)]
      if (length(clean_v) > 10) {
        sk <- safe_skewness(clean_v)
        if (!is.na(sk) && sk > 1.5) {
          add_step(
            category = "Feature Enrichment",
            title = paste0("Create Log-Normalized Feature for '", cn, "'"),
            description = paste0("Sample skewness is high (", round(sk, 2), "). Add log1p(x) transformation for linear models."),
            col = cn,
            action = "enrich_log",
            params = list()
          )
        }
      }
    }
  }

  return(recipe)
}

# ---- 2. Recipe Executor -----------------------------------------------------

#' Execute an ordered list of synthesized recipe steps
#' @param df Data frame to transform
#' @param recipe List of recipe step objects
#' @return List with data, steps_completed, affected_rows, affected_cells, log
execute_recipe <- function(df, recipe) {
  if (is.null(df) || length(recipe) == 0) {
    return(list(data = df, steps_completed = 0, affected_rows = 0, affected_cells = 0, log = character(0)))
  }

  curr_df <- df
  n_done <- 0
  tot_rows <- 0
  tot_cells <- 0
  exec_log <- character(0)

  for (step in recipe) {
    if (!isTRUE(step$enabled)) next

    res <- NULL
    act <- step$action
    cn <- step$column

    tryCatch({
      if (act == "clean_names") {
        res <- transform_clean_names(curr_df)
      } else if (act == "currency_percent") {
        res <- transform_currency_percent(curr_df, col = cn)
      } else if (act == "deduplicate_exact") {
        res <- transform_duplicates(curr_df, keep_rule = "first")
      } else if (act == "trim_whitespace") {
        res <- transform_categories(curr_df, col = cn, action = "trim_whitespace")
      } else if (act == "case_title") {
        res <- transform_categories(curr_df, col = cn, action = "case_title")
      } else if (grepl("^cast_", act)) {
        t_type <- step$params$target_type
        res <- transform_types(curr_df, col = cn, target_type = t_type)
      } else if (grepl("^impute_", act)) {
        meth <- step$params$method
        res <- transform_missing(curr_df, col = cn, method = meth)
      } else if (act == "winsorize_iqr") {
        res <- transform_outliers(curr_df, col = cn, method = "iqr_1.5", action = "cap_bounds")
      } else if (act == "enrich_date") {
        f_res <- derive_date_features(curr_df, col = cn)
        if (f_res$success) {
          res <- list(success = TRUE, data = f_res$data, affected_rows = nrow(curr_df), affected_cells = length(f_res$new_columns) * nrow(curr_df), message = f_res$message)
        }
      } else if (act == "enrich_cyclical") {
        p_val <- step$params$period
        if (is.null(p_val)) p_val <- 12
        f_res <- derive_cyclical_features(curr_df, col = cn, period = p_val)
        if (f_res$success) {
          res <- list(success = TRUE, data = f_res$data, affected_rows = nrow(curr_df), affected_cells = length(f_res$new_columns) * nrow(curr_df), message = f_res$message)
        }
      } else if (act == "enrich_log") {
        f_res <- derive_log_transform(curr_df, col = cn)
        if (f_res$success) {
          res <- list(success = TRUE, data = f_res$data, affected_rows = nrow(curr_df), affected_cells = nrow(curr_df), message = f_res$message)
        }
      }
    }, error = function(e) {
      res <<- NULL
    })

    if (!is.null(res) && isTRUE(res$success)) {
      curr_df <- res$data
      n_done <- n_done + 1
      tot_rows <- tot_rows + res$affected_rows
      tot_cells <- tot_cells + res$affected_cells
      exec_log <- c(exec_log, paste0("[Step ", n_done, "] ", step$title, ": ", res$message))
    }
  }

  list(
    data = curr_df,
    steps_completed = n_done,
    affected_rows = tot_rows,
    affected_cells = tot_cells,
    log = exec_log
  )
}

# ---- 3. Reproducible R Code Generator ---------------------------------------

#' Generate standalone, runnable R script reproducing the exact transformation pipeline
#' @param recipe List of recipe step objects
#' @param dataset_name Name of source file
#' @return String containing formatted R code
generate_reproducible_r_code <- function(recipe, dataset_name = "dataset.csv") {
  lines <- c(
    "# ==============================================================================",
    "# DATA DETECTIVE PRO: Automated Preprocessing & Enrichment Script",
    paste0("# Generated for: ", dataset_name),
    paste0("# Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "# Compatible with Base R (>= 4.0.0)",
    "# ==============================================================================",
    "",
    "# 1. Load Raw Dataset",
    paste0("raw_path <- '", dataset_name, "'"),
    "df <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE)",
    "cat(sprintf('Loaded raw dataset with %d rows and %d columns.\\n', nrow(df), ncol(df)))",
    "",
    "# 2. Transformation & Enrichment Pipeline"
  )

  step_idx <- 1
  for (step in recipe) {
    if (!isTRUE(step$enabled)) next

    act <- step$action
    cn <- step$column
    lines <- c(lines, paste0("# Step ", step_idx, ": ", step$title))

    if (act == "clean_names") {
      lines <- c(
        lines,
        "# Sanitize and standardize column names into snake_case",
        "clean_names <- tolower(gsub('[^[:alnum:]]+', '_', trimws(names(df))))",
        "clean_names <- gsub('^_|_$', '', clean_names)",
        "names(df) <- make.unique(clean_names, sep = '_')",
        "cat('Standardized column headers to clean snake_case.\\n')"
      )
    } else if (act == "currency_percent") {
      lines <- c(
        lines,
        paste0("df[['", cn, "']] <- as.numeric(gsub('[$€£¥%,]', '', trimws(df[['", cn, "']])))"),
        paste0("cat(sprintf('Parsed currency/percent in %s to numeric.\\n', '", cn, "'))")
      )
    } else if (act == "deduplicate_exact") {
      lines <- c(
        lines,
        "n_before <- nrow(df)",
        "df <- df[!duplicated(df), , drop = FALSE]",
        "cat(sprintf('Removed %d duplicate rows.\\n', n_before - nrow(df)))"
      )
    } else if (act == "trim_whitespace") {
      lines <- c(
        lines,
        paste0("df[['", cn, "']] <- trimws(df[['", cn, "']])")
      )
    } else if (act == "case_title") {
      lines <- c(
        lines,
        paste0("df[['", cn, "']] <- vapply(df[['", cn, "']], function(s) {"),
        "  if (is.na(s)) return(NA_character_)",
        "  s <- tolower(s)",
        "  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))",
        "}, character(1))"
      )
    } else if (grepl("^cast_", act)) {
      t_type <- step$params$target_type
      lines <- c(lines, paste0("df[['", cn, "']] <- as.", t_type, "(df[['", cn, "']])"))
    } else if (grepl("^impute_", act)) {
      meth <- step$params$method
      if (meth == "median") {
        lines <- c(
          lines,
          paste0("med_val <- stats::median(df[['", cn, "']], na.rm = TRUE)"),
          paste0("df[['", cn, "']][is.na(df[['", cn, "']])] <- med_val")
        )
      } else if (meth == "mean") {
        lines <- c(
          lines,
          paste0("mean_val <- mean(df[['", cn, "']], na.rm = TRUE)"),
          paste0("df[['", cn, "']][is.na(df[['", cn, "']])] <- mean_val")
        )
      } else if (meth == "mode") {
        lines <- c(
          lines,
          paste0("tab <- table(df[['", cn, "']][!is.na(df[['", cn, "']])])"),
          "mode_val <- names(tab)[which.max(tab)]",
          paste0("df[['", cn, "']][is.na(df[['", cn, "']])] <- mode_val")
        )
      }
    } else if (act == "winsorize_iqr") {
      lines <- c(
        lines,
        paste0("qs <- quantile(df[['", cn, "']], probs = c(0.25, 0.75), na.rm = TRUE)"),
        "iqr_val <- qs[2] - qs[1]",
        "lower_b <- qs[1] - 1.5 * iqr_val",
        "upper_b <- qs[2] + 1.5 * iqr_val",
        paste0("df[['", cn, "']][!is.na(df[['", cn, "']]) & df[['", cn, "']] < lower_b] <- lower_b"),
        paste0("df[['", cn, "']][!is.na(df[['", cn, "']]) & df[['", cn, "']] > upper_b] <- upper_b")
      )
    } else if (act == "enrich_date") {
      lines <- c(
        lines,
        paste0("d_parsed <- as.Date(df[['", cn, "']])"),
        paste0("df[['", cn, "_year']] <- as.integer(format(d_parsed, '%Y'))"),
        paste0("df[['", cn, "_month']] <- as.integer(format(d_parsed, '%m'))"),
        paste0("df[['", cn, "_quarter']] <- as.integer((as.integer(format(d_parsed, '%m')) - 1) %/% 3 + 1)"),
        paste0("df[['", cn, "_day_of_week']] <- weekdays(d_parsed)"),
        paste0("df[['", cn, "_is_weekend']] <- as.integer(weekdays(d_parsed) %in% c('Saturday', 'Sunday'))")
      )
    } else if (act == "enrich_cyclical") {
      p_val <- step$params$period
      if (is.null(p_val)) p_val <- 12
      lines <- c(
        lines,
        paste0("df[['", cn, "_sin']] <- round(sin(2 * pi * df[['", cn, "']] / ", p_val, "), 4)"),
        paste0("df[['", cn, "_cos']] <- round(cos(2 * pi * df[['", cn, "']] / ", p_val, "), 4)")
      )
    } else if (act == "enrich_log") {
      lines <- c(
        lines,
        paste0("df[['", cn, "_log']] <- log1p(df[['", cn, "']])")
      )
    }

    lines <- c(lines, "")
    step_idx <- step_idx + 1

  }

  lines <- c(
    lines,
    "# 3. Save Final Useful Dataset",
    "output_path <- 'cleaned_useful_dataset.csv'",
    "write.csv(df, output_path, row.names = FALSE)",
    "cat(sprintf('Successfully exported %d rows and %d columns to %s\\n', nrow(df), ncol(df), output_path))"
  )

  paste(lines, collapse = "\n")
}
