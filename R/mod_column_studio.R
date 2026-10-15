# ==============================================================================
# DATA DETECTIVE PRO: Column Studio Module
# File: R/mod_column_studio.R
# Description: Deep univariate investigation, distribution visualizations, and statistical summaries
# Shinylive / WebAssembly compatible (Base R + ggplot2)
# ==============================================================================

mod_column_studio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row",
      # Left Sidebar: Column Selector & Metadata Profile
      div(
        class = "col-lg-4 col-md-12 mb-3",
        div(
          class = "panel-box mb-3",
          h5(class = "panel-title", icon("columns"), " Select Variable"),
          selectInput(ns("selected_column"), label = NULL, choices = NULL, width = "100%"),
          uiOutput(ns("column_badge_summary"))
        ),
        div(
          class = "panel-box",
          h6(class = "panel-title", icon("circle-info"), " Column Characteristics"),
          uiOutput(ns("column_quick_meta"))
        )
      ),

      # Right Workspace: Deep Univariate Diagnostics & Dynamic Visualizations
      div(
        class = "col-lg-8 col-md-12 mb-3",
        div(
          class = "panel-box mb-3",
          div(
            class = "d-flex justify-content-between align-items-center mb-2 flex-wrap gap-2",
            h5(class = "panel-title mb-0", icon("chart-line"), " Variable Visualization & Diagnostics"),
            uiOutput(ns("chart_controls_ui"))
          ),
          plotOutput(ns("column_chart"), height = "360px")
        ),
        # Statistics & Plain-Language Interpretation
        div(
          class = "panel-box mb-3",
          h5(class = "panel-title", icon("calculator"), " Detailed Univariate Summary"),
          uiOutput(ns("column_stats_content"))
        ),
        # Interpretation Callout
        div(
          class = "panel-box",
          h5(class = "panel-title", icon("comment-dots"), " Plain-Language Analytical Interpretation"),
          uiOutput(ns("column_interpretation_text"))
        )
      )
    )
  )
}

mod_column_studio_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update available columns
    observe({
      df <- data_r()
      req(df)
      cols <- names(df)
      current <- input$selected_column
      sel <- if (!is.null(current) && current %in% cols) current else cols[1]
      updateSelectInput(session, "selected_column", choices = cols, selected = sel)
    })

    # Active column vector and type
    col_data <- reactive({
      df <- data_r()
      col <- input$selected_column
      req(df, col, col %in% names(df))
      df[[col]]
    })

    col_type <- reactive({
      v <- col_data()
      req(v)
      detect_column_type(v)
    })

    # Column Badge Summary (Left)
    output$column_badge_summary <- renderUI({
      df <- data_r()
      col <- input$selected_column
      req(df, col, col %in% names(df))
      v <- col_data()
      ctype <- col_type()

      n_total <- length(v)
      n_miss <- sum(is.na(v) | (is.character(v) & trimws(v) == ""))
      miss_pct <- round((n_miss / n_total) * 100, 1)
      n_unique <- length(unique(v[!is.na(v)]))

      type_badge_class <- switch(
        ctype,
        "numeric" = "bg-primary",
        "integer" = "bg-info text-white",
        "character" = "bg-secondary",
        "factor" = "bg-dark",
        "date" = "bg-success",
        "datetime" = "bg-success",
        "logical" = "bg-warning text-dark",
        "bg-secondary"
      )

      div(
        class = "mt-2",
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          span(class = paste0("badge ", type_badge_class, " fs-6 text-uppercase"), ctype),
          span(class = "small text-muted", paste0(n_unique, " unique values"))
        ),
        div(
          class = "progress", style = "height: 8px;",
          div(
            class = "progress-bar bg-success",
            role = "progressbar",
            style = paste0("width: ", 100 - miss_pct, "%;"),
            title = paste0(100 - miss_pct, "% Complete")
          ),
          div(
            class = "progress-bar bg-danger",
            role = "progressbar",
            style = paste0("width: ", miss_pct, "%;"),
            title = paste0(miss_pct, "% Missing")
          )
        ),
        div(
          class = "d-flex justify-content-between small text-muted mt-1",
          span(paste0("Valid: ", format_number(n_total - n_miss), " (", 100 - miss_pct, "%)")),
          span(paste0("Missing: ", format_number(n_miss), " (", miss_pct, "%)"))
        )
      )
    })

    # Quick Meta Table (Left)
    output$column_quick_meta <- renderUI({
      v <- col_data()
      req(v)
      ctype <- col_type()
      n_total <- length(v)
      clean_v <- v[!is.na(v)]

      sample_text <- if (length(clean_v) > 0) paste(utils::head(clean_v, 3), collapse = ", ") else "None"
      
      tagList(
        tags$table(
          class = "table table-sm table-borderless small mb-0",
          tags$tr(tags$td(class = "text-muted", "Total Cells:"), tags$td(strong(format_number(n_total)))),
          tags$tr(tags$td(class = "text-muted", "Inferred Type:"), tags$td(strong(ctype))),
          tags$tr(tags$td(class = "text-muted", "Is Identifier:"), tags$td(if (length(unique(clean_v)) == length(clean_v) && length(clean_v) > 10) span(class = "badge bg-info text-white", "Candidate Primary Key") else "No")),
          tags$tr(tags$td(class = "text-muted", "Constant:"), tags$td(if (length(unique(clean_v)) <= 1) span(class = "badge bg-danger", "Strictly Constant") else "Varying")),
          tags$tr(tags$td(class = "text-muted", "Preview:"), tags$td(class = "text-truncate", style = "max-width: 140px;", sample_text))
        )
      )
    })

    # Dynamic Chart Controls based on column type
    output$chart_controls_ui <- renderUI({
      ctype <- col_type()
      req(ctype)

      if (ctype %in% c("numeric", "integer")) {
        div(
          class = "d-flex align-items-center gap-2",
          selectInput(
            ns("num_plot_type"),
            label = NULL,
            choices = c(
              "Histogram + Density" = "hist_density",
              "Box Plot (Outliers)" = "box",
              "Violin Plot" = "violin",
              "ECDF (Cumulative)" = "ecdf",
              "QQ-Norm Plot" = "qq"
            ),
            selected = "hist_density",
            width = "180px"
          ),
          sliderInput(ns("num_bins"), label = NULL, min = 10, max = 60, value = 25, step = 5, width = "100px")
        )
      } else if (ctype %in% c("character", "factor", "logical")) {
        div(
          class = "d-flex align-items-center gap-2",
          selectInput(
            ns("cat_plot_type"),
            label = NULL,
            choices = c(
              "Vertical Bar Chart" = "bar_v",
              "Horizontal Bar Chart" = "bar_h",
              "Top 10 Categories" = "top10",
              "Pareto Cumulative Curve" = "pareto"
            ),
            selected = "bar_v",
            width = "180px"
          )
        )
      } else {
        div(class = "small text-muted", "Standard Timeline Display")
      }
    })

    # Column Chart Render
    output$column_chart <- renderPlot({
      v <- col_data()
      col <- input$selected_column
      ctype <- col_type()
      req(v, col, ctype)

      clean_v <- v[!is.na(v)]
      if (length(clean_v) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 1, y = 1, label = "All observations in this variable are missing.", size = 5, color = "#ef4444") +
                 ggplot2::theme_void())
      }

      if (ctype %in% c("numeric", "integer")) {
        clean_num <- as.numeric(clean_v[is.finite(clean_v)])
        p_type <- if (!is.null(input$num_plot_type)) input$num_plot_type else "hist_density"
        bins <- if (!is.null(input$num_bins)) as.numeric(input$num_bins) else 25
        plot_df <- data.frame(val = clean_num)

        if (p_type == "hist_density") {
          ggplot2::ggplot(plot_df, ggplot2::aes(x = val)) +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = bins, fill = "#2563eb", color = "#1e40af", alpha = 0.7) +
            ggplot2::geom_density(color = "#ef4444", linewidth = 1.0) +
            ggplot2::labs(title = paste0("Histogram & Empirical Density: ", col), x = col, y = "Density") +
            theme_detective()
        } else if (p_type == "box") {
          ggplot2::ggplot(plot_df, ggplot2::aes(x = "", y = val)) +
            ggplot2::geom_boxplot(fill = "#eff6ff", color = "#1e40af", outlier.color = "#ef4444", outlier.size = 2.2) +
            ggplot2::geom_jitter(width = 0.15, alpha = 0.25, color = "#64748b") +
            ggplot2::coord_flip() +
            ggplot2::labs(title = paste0("Box Plot & Outlier Spread: ", col), y = col, x = "") +
            theme_detective()
        } else if (p_type == "violin") {
          ggplot2::ggplot(plot_df, ggplot2::aes(x = "", y = val)) +
            ggplot2::geom_violin(fill = "#eff6ff", color = "#2563eb", trim = FALSE, alpha = 0.8) +
            ggplot2::geom_boxplot(width = 0.15, fill = "#ffffff", color = "#1e40af", outlier.color = "#ef4444") +
            ggplot2::coord_flip() +
            ggplot2::labs(title = paste0("Violin Kernel Density Spread: ", col), y = col, x = "") +
            theme_detective()
        } else if (p_type == "ecdf") {
          ggplot2::ggplot(plot_df, ggplot2::aes(x = val)) +
            ggplot2::stat_ecdf(geom = "step", color = "#10b981", linewidth = 1.0) +
            ggplot2::labs(title = paste0("Empirical Cumulative Distribution Function (ECDF): ", col), x = col, y = "Cumulative Probability F(x)") +
            theme_detective()
        } else if (p_type == "qq") {
          ggplot2::ggplot(plot_df, ggplot2::aes(sample = val)) +
            ggplot2::stat_qq(color = "#2563eb", alpha = 0.7, size = 1.8) +
            ggplot2::stat_qq_line(color = "#ef4444", linewidth = 0.9) +
            ggplot2::labs(title = paste0("Normal Q-Q Plot: ", col), x = "Theoretical Quantiles", y = "Sample Quantiles") +
            theme_detective()
        }
      } else if (ctype %in% c("character", "factor", "logical")) {
        char_v <- as.character(clean_v)
        tab <- sort(table(char_v), decreasing = TRUE)
        p_type <- if (!is.null(input$cat_plot_type)) input$cat_plot_type else "bar_v"

        if (p_type == "top10") {
          tab <- head(tab, 10)
        }

        plot_df <- data.frame(
          Category = names(tab),
          Count = as.numeric(tab),
          stringsAsFactors = FALSE
        )
        plot_df$Category <- factor(plot_df$Category, levels = if (p_type == "bar_h") rev(plot_df$Category) else plot_df$Category)

        if (p_type == "bar_h") {
          ggplot2::ggplot(plot_df, ggplot2::aes(x = Category, y = Count)) +
            ggplot2::geom_col(fill = "#2563eb", width = 0.6, alpha = 0.85) +
            ggplot2::geom_text(ggplot2::aes(label = Count), hjust = -0.15, size = 3.6, color = "#0f172a") +
            ggplot2::coord_flip() +
            ggplot2::labs(title = paste0("Category Frequencies: ", col), x = "", y = "Observation Count") +
            theme_detective()
        } else {
          ggplot2::ggplot(plot_df, ggplot2::aes(x = Category, y = Count)) +
            ggplot2::geom_col(fill = "#2563eb", width = 0.6, alpha = 0.85) +
            ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.4, size = 3.4, color = "#0f172a") +
            ggplot2::labs(title = paste0("Category Frequencies: ", col), x = col, y = "Observation Count") +
            theme_detective() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
        }
      } else if (ctype %in% c("date", "datetime")) {
        d_clean <- as.Date(clean_v)
        tab_dates <- sort(table(d_clean))
        plot_df <- data.frame(Date = as.Date(names(tab_dates)), Count = as.numeric(tab_dates))
        ggplot2::ggplot(plot_df, ggplot2::aes(x = Date, y = Count)) +
          ggplot2::geom_line(color = "#2563eb", linewidth = 0.9) +
          ggplot2::geom_point(color = "#1d4ed8", size = 2) +
          ggplot2::labs(title = paste0("Observation Timeline Frequency: ", col), x = "Date", y = "Count") +
          theme_detective()
      }
    })

    # Column Stats Grid
    output$column_stats_content <- renderUI({
      v <- col_data()
      ctype <- col_type()
      req(v, ctype)
      clean_v <- v[!is.na(v)]

      if (ctype %in% c("numeric", "integer")) {
        num_clean <- as.numeric(clean_v[is.finite(clean_v)])
        if (length(num_clean) == 0) return(div("No finite observations."))

        qs <- stats::quantile(num_clean, probs = c(0.25, 0.5, 0.75))
        iqr_v <- qs[3] - qs[1]
        out_v <- sum(num_clean < (qs[1] - 1.5 * iqr_v) | num_clean > (qs[3] + 1.5 * iqr_v))

        div(
          class = "metrics-grid mb-2",
          div(class = "metric-card", div(class = "metric-title", "VALID COUNT"), div(class = "metric-value", format_number(length(num_clean)))),
          div(class = "metric-card", div(class = "metric-title", "MEAN"), div(class = "metric-value", round(mean(num_clean), 3))),
          div(class = "metric-card", div(class = "metric-title", "MEDIAN"), div(class = "metric-value", round(stats::median(num_clean), 3))),
          div(class = "metric-card", div(class = "metric-title", "STD DEV"), div(class = "metric-value", round(stats::sd(num_clean), 3))),
          div(class = "metric-card", div(class = "metric-title", "MINIMUM"), div(class = "metric-value", round(min(num_clean), 3))),
          div(class = "metric-card", div(class = "metric-title", "MAXIMUM"), div(class = "metric-value", round(max(num_clean), 3))),
          div(class = "metric-card", div(class = "metric-title", "IQR (SPREAD)"), div(class = "metric-value", round(iqr_v, 3))),
          div(class = "metric-card", div(class = "metric-title", "SKEWNESS"), div(class = "metric-value", round(safe_skewness(num_clean), 3))),
          div(class = "metric-card card-alert", div(class = "metric-title", "OUTLIERS (1.5x)"), div(class = "metric-value", out_v))
        )
      } else {
        tab <- sort(table(as.character(clean_v)), decreasing = TRUE)
        top_cat <- if (length(tab) > 0) names(tab)[1] else "None"
        top_pct <- if (length(clean_v) > 0) round((tab[1] / length(clean_v)) * 100, 1) else 0

        div(
          class = "metrics-grid mb-2",
          div(class = "metric-card", div(class = "metric-title", "OBSERVATIONS"), div(class = "metric-value", format_number(length(clean_v)))),
          div(class = "metric-card", div(class = "metric-title", "DISTINCT CLASSES"), div(class = "metric-value", length(tab))),
          div(class = "metric-card", div(class = "metric-title", "DOMINANT CLASS"), div(class = "metric-value fs-6", top_cat)),
          div(class = "metric-card", div(class = "metric-title", "TOP CLASS SHARE"), div(class = "metric-value", paste0(top_pct, "%")))
        )
      }
    })

    # Plain Language Interpretation
    output$column_interpretation_text <- renderUI({
      v <- col_data()
      col <- input$selected_column
      ctype <- col_type()
      req(v, col, ctype)
      clean_v <- v[!is.na(v)]

      if (ctype %in% c("numeric", "integer")) {
        num_clean <- as.numeric(clean_v[is.finite(clean_v)])
        if (length(num_clean) < 3) return(p("Insufficient numeric sample size for interpretation."))
        m <- mean(num_clean)
        med <- stats::median(num_clean)
        skew <- safe_skewness(num_clean)

        skew_desc <- if (abs(skew) < 0.5) {
          "exhibits an approximately symmetric distribution"
        } else if (skew > 0.5) {
          "exhibits positive (right-skewed) spread where extreme high values pull the mean above the median"
        } else {
          "exhibits negative (left-skewed) spread where low values drag down the mean"
        }

        div(
          class = "p-3 bg-light border rounded small",
          p(
            strong("Distribution Shape: "),
            paste0("Variable '", col, "' ", skew_desc, " (Sample Skewness = ", round(skew, 3), ").")
          ),
          p(
            strong("Central Tendency Contrast: "),
            paste0("The arithmetic mean is ", round(m, 2), " compared to the median of ", round(med, 2), ". ",
                   if (abs(m - med) > 0.1 * stats::sd(num_clean)) "The divergence indicates skewness or outlier leverage; median is recommended for robust central reporting." else "The close agreement between mean and median indicates symmetrical central concentration.")
          )
        )
      } else {
        tab <- sort(table(as.character(clean_v)), decreasing = TRUE)
        top_name <- if (length(tab) > 0) names(tab)[1] else "None"
        top_pct <- if (length(clean_v) > 0) round((tab[1] / length(clean_v)) * 100, 1) else 0

        div(
          class = "p-3 bg-light border rounded small",
          p(
            strong("Categorical Balance: "),
            paste0("Variable '", col, "' contains ", length(tab), " unique categorical levels. The dominant category is '",
                   top_name, "' accounting for ", top_pct, "% of non-missing records.")
          ),
          if (top_pct > 70) {
            p(class = "text-warning fw-bold", icon("triangle-exclamation"), " High category concentration detected. This variable may have limited variance for statistical modeling.")
          } else {
            p(class = "text-success", icon("check"), " Category distribution shows balanced class spread.")
          }
        )
      }
    })

  })
}
