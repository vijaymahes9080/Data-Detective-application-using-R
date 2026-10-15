# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/utility_functions.R
# Description: Helper functions for formatting, safe calculations, and types
# ==============================================================================

# ---- Formatting Helpers -----------------------------------------------------

#' Format large numbers with commas (vectorized-safe for R 4.3+)
format_number <- function(x) {
  if (is.null(x) || length(x) == 0) return("0")
  vapply(x, function(val) {
    if (is.na(val)) return("0")
    formatC(as.numeric(val), format = "d", big.mark = ",")
  }, character(1))
}

#' Format percentages (safe for R 4.3+)
format_pct <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0) return("0.0%")
  vapply(x, function(val) {
    if (is.na(val)) return("0.0%")
    paste0(formatC(as.numeric(val) * 100, format = "f", digits = digits), "%")
  }, character(1))
}

#' Format byte sizes (safe for R 4.3+)
format_bytes <- function(bytes) {
  if (is.null(bytes) || length(bytes) == 0 || is.na(bytes[1]) || bytes[1] <= 0) return("0 B")
  b <- bytes[1]
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- min(floor(log(b, base = 1024)), length(units) - 1)
  val <- b / (1024 ^ power)
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

# ---- High-Craft Data Visualization Theme & Tokens ---------------------------

#' Cohesive Chart Color Tokens
detective_colors <- function() {
  c(
    primary      = "#2563eb",
    primary_dark = "#1e40af",
    secondary    = "#0284c7",
    accent       = "#6366f1",
    teal         = "#0d9488",
    success      = "#10b981",
    warning      = "#f59e0b",
    danger       = "#ef4444",
    muted        = "#94a3b8",
    border       = "#e2e8f0",
    grid         = "#f1f5f9",
    text_dark    = "#0f172a",
    text_muted   = "#64748b"
  )
}

#' Unified Data Detective ggplot2 Theme
theme_detective <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size + 2, color = "#0f172a",
        margin = ggplot2::margin(b = 6), hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 1, color = "#64748b",
        margin = ggplot2::margin(b = 10), hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        size = base_size - 2, color = "#94a3b8",
        margin = ggplot2::margin(t = 8)
      ),
      panel.grid.major = ggplot2::element_line(color = "#f1f5f9", linewidth = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        size = base_size - 1, fontface = "bold", color = "#475569"
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.text = ggplot2::element_text(size = base_size - 2, color = "#64748b"),
      axis.line = ggplot2::element_line(color = "#cbd5e1", linewidth = 0.4),
      axis.ticks = ggplot2::element_line(color = "#cbd5e1", linewidth = 0.4),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size - 1, fontface = "bold", color = "#334155"),
      legend.text = ggplot2::element_text(size = base_size - 2, color = "#64748b"),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      panel.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

# ---- 2D / 3D Visual Projection Engine (Pure R & Shinylive Safe) --------------

#' Project 3D Points to 2D Screen Plane with Depth Ordering & Floor Grids
#'
#' @param df Data frame containing the coordinates
#' @param var_x Numeric variable for X axis
#' @param var_y Numeric variable for Y axis
#' @param var_z Numeric variable for Z axis (Elevation/Depth)
#' @param theta Horizontal azimuth rotation angle in degrees (0 - 360)
#' @param phi Vertical elevation angle in degrees (10 - 80)
#' @return A ggplot2 visualization displaying a 3D isometric feature space
project_3d_scatter <- function(df, var_x, var_y, var_z, color_var = NULL, theta = 45, phi = 30) {
  req(df, var_x, var_y, var_z)
  if (!(var_x %in% names(df) && var_y %in% names(df) && var_z %in% names(df))) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }

  vx <- as.numeric(df[[var_x]])
  vy <- as.numeric(df[[var_y]])
  vz <- as.numeric(df[[var_z]])

  valid_idx <- which(!is.na(vx) & !is.na(vy) & !is.na(vz))
  if (length(valid_idx) < 3) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 1, y = 1, label = "Insufficient non-missing 3D data.", color = "#94a3b8") +
        ggplot2::theme_void()
    )
  }

  vx <- vx[valid_idx]
  vy <- vy[valid_idx]
  vz <- vz[valid_idx]

  # Normalize coordinates into [0, 1] cube for stable rotation
  norm_coord <- function(v) {
    rng <- range(v, na.rm = TRUE)
    if (rng[2] == rng[1]) return(rep(0.5, length(v)))
    (v - rng[1]) / (rng[2] - rng[1])
  }

  nx <- norm_coord(vx)
  ny <- norm_coord(vy)
  nz <- norm_coord(vz)

  # Convert angles to radians
  rad_theta <- (theta * pi) / 180
  rad_phi   <- (phi * pi) / 180

  # Isometric / perspective rotation projection
  # X' = -x * sin(theta) + y * cos(theta)
  # Y' = -x * cos(theta) * sin(phi) - y * sin(theta) * sin(phi) + z * cos(phi)
  # Depth = x * cos(theta) * cos(phi) + y * sin(theta) * cos(phi) + z * sin(phi)
  px <- -nx * sin(rad_theta) + ny * cos(rad_theta)
  py <- -nx * cos(rad_theta) * sin(rad_phi) - ny * sin(rad_theta) * sin(rad_phi) + nz * cos(rad_phi)
  depth <- nx * cos(rad_theta) * cos(rad_phi) + ny * sin(rad_theta) * cos(rad_phi) + nz * sin(rad_phi)

  # Floor projection (z = 0) for drop lines
  floor_px <- px
  floor_py <- -nx * cos(rad_theta) * sin(rad_phi) - ny * sin(rad_theta) * sin(rad_phi)

  plot_data <- data.frame(
    px = px,
    py = py,
    floor_px = floor_px,
    floor_py = floor_py,
    depth = depth,
    orig_z = vz,
    stringsAsFactors = FALSE
  )

  # Sort by depth so closer points render on top
  plot_data <- plot_data[order(plot_data$depth), ]

  # Build Wireframe Grid Floor (5x5 grid)
  grid_lines <- list()
  grid_steps <- seq(0, 1, length.out = 5)
  for (gx in grid_steps) {
    g_px <- -gx * sin(rad_theta) + grid_steps * cos(rad_theta)
    g_py <- -gx * cos(rad_theta) * sin(rad_phi) - grid_steps * sin(rad_theta) * sin(rad_phi)
    grid_lines[[length(grid_lines) + 1]] <- data.frame(gx = g_px, gy = g_py, grp = paste0("x_", gx))
  }
  for (gy in grid_steps) {
    g_px <- -grid_steps * sin(rad_theta) + gy * cos(rad_theta)
    g_py <- -grid_steps * cos(rad_theta) * sin(rad_phi) - gy * sin(rad_theta) * sin(rad_phi)
    grid_lines[[length(grid_lines) + 1]] <- data.frame(gx = g_px, gy = g_py, grp = paste0("y_", gy))
  }
  grid_df <- do.call(rbind, grid_lines)

  # Color by Z or group if supplied
  p <- ggplot2::ggplot() +
    # Floor grid
    ggplot2::geom_path(
      data = grid_df,
      ggplot2::aes(x = gx, y = gy, group = grp),
      color = "#e2e8f0", linewidth = 0.4
    ) +
    # Drop lines from point to floor
    ggplot2::geom_segment(
      data = plot_data,
      ggplot2::aes(x = px, y = py, xend = floor_px, yend = floor_py),
      color = "#cbd5e1", linewidth = 0.35, linetype = "dotted"
    ) +
    # 3D points shaded by depth/Z
    ggplot2::geom_point(
      data = plot_data,
      ggplot2::aes(x = px, y = py, color = orig_z, size = depth),
      alpha = 0.85
    ) +
    ggplot2::scale_color_gradientn(
      colors = c("#2563eb", "#06b6d4", "#10b981", "#f59e0b", "#ef4444"),
      name = paste0("Z: ", var_z)
    ) +
    ggplot2::scale_size_continuous(range = c(2.2, 5.0), guide = "none") +
    ggplot2::labs(
      title = paste0("3D Feature Space: ", var_x, " \u00d7 ", var_y, " \u00d7 ", var_z),
      subtitle = paste0("Azimuth: ", theta, "\u00b0 | Elevation: ", phi, "\u00b0 | Depth-Ordered Projection")
    ) +
    theme_detective() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  p
}
