# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/utility_functions.R
# Description: Helper functions for formatting, safe calculations, and types
# ==============================================================================

# ---- Formatting Helpers -----------------------------------------------------

#' Format large numbers with commas
format_number <- function(x) {
  if (is.null(x) || is.na(x)) return("0")
  formatC(as.numeric(x), format = "d", big.mark = ",")
}

#' Format percentages
format_pct <- function(x, digits = 1) {
  if (is.null(x) || is.na(x)) return("0.0%")
  paste0(formatC(as.numeric(x) * 100, format = "f", digits = digits), "%")
}

#' Format byte sizes
format_bytes <- function(bytes) {
  if (is.null(bytes) || is.na(bytes) || bytes <= 0) return("0 B")
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- min(floor(log(bytes, base = 1024)), length(units) - 1)
  val <- bytes / (1024 ^ power)
  paste0(round(val, 2), " ", units[power + 1])
}

# ---- Safe Statistical Helpers -----------------------------------------------

#' Safe mean calculation
safe_mean <- function(x) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  if (length(x_clean) == 0) return(NA_real_)
  mean(x_clean)
}

#' Safe median calculation
safe_median <- function(x) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  if (length(x_clean) == 0) return(NA_real_)
  stats::median(x_clean)
}

#' Safe standard deviation
safe_sd <- function(x) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  if (length(x_clean) <= 1) return(NA_real_)
  stats::sd(x_clean)
}

#' Safe IQR calculation
safe_iqr <- function(x) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  if (length(x_clean) < 4) return(NA_real_)
  stats::IQR(x_clean)
}

#' Safe skewness calculation (Fisher-Pearson standardized moment)
safe_skewness <- function(x) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  n <- length(x_clean)
  if (n < 3) return(NA_real_)
  m <- mean(x_clean)
  s <- stats::sd(x_clean)
  if (is.na(s) || s == 0) return(0)
  m3 <- sum((x_clean - m)^3) / n
  skew <- m3 / (s^3)
  # Sample correction
  (sqrt(n * (n - 1)) / (n - 2)) * skew
}

#' Safe kurtosis calculation (excess kurtosis)
safe_kurtosis <- function(x) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  n <- length(x_clean)
  if (n < 4) return(NA_real_)
  m <- mean(x_clean)
  s <- stats::sd(x_clean)
  if (is.na(s) || s == 0) return(0)
  m4 <- sum((x_clean - m)^4) / n
  (m4 / (s^4)) - 3
}

# ---- Data Type Detection ----------------------------------------------------

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
    # Check if character might actually be dates
    sample_vals <- x[!is.na(x) & nzchar(trimws(x))]
    if (length(sample_vals) > 0) {
      sample_subset <- head(sample_vals, 30)
      parsed <- suppressWarnings(as.Date(sample_subset, optional = TRUE))
      if (all(!is.na(parsed)) && length(parsed) > 0) {
        return("date_candidate")
      }
    }
    return("character")
  } else {
    return("other")
  }
}

# ---- HTML Severity Badges ---------------------------------------------------

#' Generate an HTML badge for severity level
severity_badge <- function(severity) {
  sev <- toupper(as.character(severity))
  bg_color <- switch(
    sev,
    "HIGH" = "#ef4444",
    "WARNING" = "#f59e0b",
    "INFO" = "#3b82f6",
    "#6b7280"
  )
  text_color <- "#ffffff"
  sprintf(
    '<span style="background-color: %s; color: %s; padding: 2px 8px; border-radius: 4px; font-weight: 600; font-size: 0.75rem; letter-spacing: 0.05em; display: inline-block;">%s</span>',
    bg_color, text_color, sev
  )
}
