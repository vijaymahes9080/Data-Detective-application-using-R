# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/distribution_analysis.R
# Description: Numeric distribution metrics, density/histogram profiling & observations
# ==============================================================================

#' Analyze numeric distribution of a single vector
#' @param x Numeric vector
#' @param var_name Variable name for contextual observations
#' @return A list containing summary metrics and factual observations
analyze_distribution <- function(x, var_name = "Variable") {
  clean_x <- x[!is.na(x) & is.finite(x)]
  n <- length(clean_x)

  if (n < 3) {
    return(list(
      valid = FALSE,
      message = "Insufficient observations for distribution analysis (minimum 3 required).",
      metrics = list(),
      observations = character(0)
    ))
  }

  q <- stats::quantile(clean_x, probs = c(0, 0.25, 0.5, 0.75, 1))
  v_min <- as.numeric(q[1])
  v_q1 <- as.numeric(q[2])
  v_med <- as.numeric(q[3])
  v_q3 <- as.numeric(q[4])
  v_max <- as.numeric(q[5])
  v_mean <- mean(clean_x)
  v_sd <- stats::sd(clean_x)
  v_var <- stats::var(clean_x)
  v_iqr <- v_q3 - v_q1
  v_skew <- safe_skewness(clean_x)
  v_kurt <- safe_kurtosis(clean_x)
  n_uniq <- length(unique(clean_x))

  # Generate Factual Observations (Never claiming significance without tests)
  obs <- character(0)

  # Mean vs Median comparison
  mean_med_diff <- abs(v_mean - v_med)
  threshold_skew <- ifelse(v_sd > 0, 0.1 * v_sd, 0.01)

  if (mean_med_diff < threshold_skew) {
    obs <- c(obs, "Mean and median are approximately aligned, suggesting relative central balance.")
  } else if (v_mean > v_med) {
    obs <- c(obs, paste0("Mean (", round(v_mean, 2), ") is greater than median (", round(v_med, 2), "), indicating positive (right) asymmetry."))
  } else {
    obs <- c(obs, paste0("Mean (", round(v_mean, 2), ") is less than median (", round(v_med, 2), "), indicating negative (left) asymmetry."))
  }

  # Skewness classification
  if (!is.na(v_skew)) {
    if (abs(v_skew) < 0.5) {
      obs <- c(obs, paste0("Sample skewness is ", round(v_skew, 2), " (fairly symmetrical)."))
    } else if (abs(v_skew) < 1.0) {
      obs <- c(obs, paste0("Sample skewness is ", round(v_skew, 2), " (moderately skewed)."))
    } else {
      obs <- c(obs, paste0("Sample skewness is ", round(v_skew, 2), " (pronounced skewness observed)."))
    }
  }

  # IQR Bounds observations
  lower_bound <- v_q1 - 1.5 * v_iqr
  upper_bound <- v_q3 + 1.5 * v_iqr
  n_outliers <- sum(clean_x < lower_bound | clean_x > upper_bound)

  if (n_outliers > 0) {
    pct_out <- round((n_outliers / n) * 100, 1)
    obs <- c(obs, paste0("The distribution contains ", n_outliers, " observations (", pct_out, "%) beyond the 1.5 \u00d7 IQR bounds."))
  } else {
    obs <- c(obs, "All observed values lie within standard 1.5 \u00d7 IQR bounds.")
  }

  # Concentration
  central_count <- sum(clean_x >= v_q1 & clean_x <= v_q3)
  obs <- c(obs, paste0("The middle 50% of observations fall between ", round(v_q1, 2), " and ", round(v_q3, 2), " (IQR = ", round(v_iqr, 2), ")."))

  # Distinct value count
  if (n_uniq <= 10) {
    obs <- c(obs, paste0("Variable has only ", n_uniq, " unique values; consider whether it represents discrete or ordinal categories."))
  }

  metrics <- list(
    n = n,
    n_unique = n_uniq,
    min = v_min,
    q1 = v_q1,
    median = v_med,
    mean = v_mean,
    q3 = v_q3,
    max = v_max,
    sd = v_sd,
    variance = v_var,
    iqr = v_iqr,
    skewness = v_skew,
    kurtosis = v_kurt,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    outliers_count = n_outliers
  )

  return(list(
    valid = TRUE,
    var_name = var_name,
    metrics = metrics,
    observations = obs
  ))
}
