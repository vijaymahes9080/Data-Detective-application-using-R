# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/helper_functions.R
# Description: General formatting, safe statistical moments, and type detectors
# Shinylive / WebAssembly compatible (Base R priority)
# ==============================================================================

#' Format integer and float values with comma thousands separators
format_number <- function(x) {
  if (is.null(x) || is.na(x) || length(x) == 0) return("0")
  formatC(as.numeric(x), format = "d", big.mark = ",")
}

#' Format decimal percentages
format_pct <- function(x, digits = 1) {
  if (is.null(x) || is.na(x) || length(x) == 0) return("0.0%")
  paste0(formatC(as.numeric(x) * 100, format = "f", digits = digits), "%")
}

#' Format bytes into human-readable memory sizes
format_bytes <- function(bytes) {
  if (is.null(bytes) || is.na(bytes) || bytes <= 0) return("0 B")
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- min(floor(log(bytes, base = 1024)), length(units) - 1)
  val <- bytes / (1024 ^ power)
  paste0(round(val, 2), " ", units[power + 1])
}

#' Safe arithmetic mean
safe_mean <- function(x) {
  clean <- x[!is.na(x) & is.finite(x)]
  if (length(clean) == 0) return(NA_real_)
  mean(clean)
}

#' Safe median
safe_median <- function(x) {
  clean <- x[!is.na(x) & is.finite(x)]
  if (length(clean) == 0) return(NA_real_)
  stats::median(clean)
}

#' Safe sample standard deviation
safe_sd <- function(x) {
  clean <- x[!is.na(x) & is.finite(x)]
  if (length(clean) <= 1) return(NA_real_)
  stats::sd(clean)
}

#' Safe Interquartile Range (IQR)
safe_iqr <- function(x) {
  clean <- x[!is.na(x) & is.finite(x)]
  if (length(clean) < 4) return(NA_real_)
  stats::IQR(clean)
}

#' Safe sample skewness (Fisher-Pearson moment coefficient)
safe_skewness <- function(x) {
  clean <- x[!is.na(x) & is.finite(x)]
  n <- length(clean)
  if (n < 3) return(NA_real_)
  m <- mean(clean)
  s <- stats::sd(clean)
  if (is.na(s) || s == 0) return(0)
  m3 <- sum((clean - m)^3) / n
  skew <- m3 / (s^3)
  (sqrt(n * (n - 1)) / (n - 2)) * skew
}

#' Detect variable data type accurately
detect_column_type <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return("datetime")
  } else if (inherits(x, "Date")) {
    return("date")
  } else if (is.integer(x)) {
    return("integer")
  } else if (is.numeric(x)) {
    return("numeric")
  } else if (is.logical(x)) {
    return("logical")
  } else if (is.factor(x)) {
    return("factor")
  } else if (is.character(x)) {
    # Check if character strings represent ISO dates
    non_na <- x[!is.na(x) & nzchar(trimws(x))]
    if (length(non_na) >= 5) {
      sample_vals <- head(non_na, 25)
      parsed <- suppressWarnings(as.Date(sample_vals, optional = TRUE))
      if (all(!is.na(parsed)) && length(parsed) > 0) {
        return("date_candidate")
      }
    }
    return("character")
  } else {
    return("other")
  }
}

#' Generate an HTML badge for severity levels (INFO, LOW, MEDIUM, HIGH)
severity_badge <- function(severity) {
  sev <- toupper(as.character(severity))
  bg_color <- switch(
    sev,
    "HIGH" = "#ef4444",
    "MEDIUM" = "#f59e0b",
    "WARNING" = "#f59e0b",
    "LOW" = "#3b82f6",
    "INFO" = "#64748b",
    "#64748b"
  )
  sprintf(
    '<span style="background-color: %s; color: #ffffff; padding: 3px 8px; border-radius: 4px; font-weight: 600; font-size: 0.75rem; letter-spacing: 0.05em; display: inline-block;">%s</span>',
    bg_color, sev
  )
}
