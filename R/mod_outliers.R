# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_outliers.R
# Description: Module 6 — Outlier Analysis (IQR Method & Z-Score Screening)
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_outliers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "panel-box mb-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
        h5(class = "panel-title mb-0", icon("chart-simple"), " Outlier Summary Across Numeric Variables"),
        span(class = "badge bg-info text-white", "Tukey 1.5 \u00d7 IQR Method")
      ),
      p(class = "small text-muted mb-3",
        strong("Statistical Notice: "),
        "An outlier is a statistically unusual observation, not automatically a data entry error. ",
        "Extreme observations may reflect legitimate heavy-tailed behavior, natural skewness, or sensor anomalies."
      ),
      DT::dataTableOutput(ns("table_outliers"))
    ),

    # Drill-down Variable Selector & Charts
    div(
      class = "panel-box",
      h5(class = "panel-title", icon("magnifying-glass"), " Outlier Visual Diagnostics"),
      div(
        class = "row align-items-center mb-3",
        div(
          class = "col-md-5",
          selectInput(ns("num_var"), "Select Numeric Variable to Drill Down:", choices = NULL, width = "100%")
        ),
        div(
          class = "col-md-7",
          uiOutput(ns("bounds_callout"))
        )
      ),
      div(
        class = "row",
        div(
          class = "col-md-6",
          div(class = "plot-container", plotOutput(ns("plot_box"), height = "320px"))
        ),
        div(
          class = "col-md-6",
          div(class = "plot-container", plotOutput(ns("plot_hist"), height = "320px"))
        )
      ),
      div(
        class = "mt-3",
        h6("Flagged Unusual Observations in Selected Variable:"),
        DT::dataTableOutput(ns("table_flagged_rows"))
      )
    )
  )
}

mod_outliers_server <- function(id, data_r, outlier_r) {
  moduleServer(id, function(input, output, session) {

    # Update variable choices
    observe({
      req(outlier_r())
      if (outlier_r()$has_numeric && length(outlier_r()$numeric_columns) > 0) {
        cols <- outlier_r()$numeric_columns
        current <- input$num_var
        selected <- if (!is.null(current) && current %in% cols) current else cols[1]
        updateSelectInput(session, "num_var", choices = cols, selected = selected)
      } else {
        updateSelectInput(session, "num_var", choices = character(0))
      }
    })

    # Summary table
    output$table_outliers <- DT::renderDataTable({
      req(outlier_r())
      s_df <- outlier_r()$summary

      if (nrow(s_df) == 0) {
        return(DT::datatable(
          data.frame(Status = "No numeric variables available for outlier analysis."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Column = s_df$column,
        Valid_Count = format_number(s_df$valid_count),
        Q1 = s_df$q1,
        Q3 = s_df$q3,
        IQR = s_df$iqr,
        Lower_Bound = s_df$lower_bound,
        Upper_Bound = s_df$upper_bound,
        IQR_Outliers = paste0(format_number(s_df$outlier_count), " (", s_df$outlier_pct, "%)"),
        Z_Score_Outliers = paste0(format_number(s_df$z_outlier_count), " (", s_df$z_outlier_pct, "%)"),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 8, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Bounds info callout
    output$bounds_callout <- renderUI({
      req(input$num_var, outlier_r())
      det <- outlier_r()$details[[input$num_var]]
      req(det)

      div(
        class = "alert alert-secondary p-2 small mb-0",
        div(strong("IQR Bounds [Q1 \u2212 1.5\u00d7IQR, Q3 + 1.5\u00d7IQR]: "), paste0("[", round(det$lower_bound, 2), ", ", round(det$upper_bound, 2), "]")),
        div(class = "text-danger", paste0("Potential Outliers Detected: ", length(det$outlier_indices), " observation(s)"))
      )
    })

    # Boxplot
    output$plot_box <- renderPlot({
      req(input$num_var, data_r())
      v <- data_r()[[input$num_var]]
      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 2)

      plot_df <- data.frame(val = clean_v, var = input$num_var)

      ggplot2::ggplot(plot_df, ggplot2::aes(x = var, y = val)) +
        ggplot2::geom_boxplot(fill = "#dbeafe", color = "#1e40af", outlier.color = "#ef4444", outlier.size = 2.5) +
        ggplot2::labs(title = paste0("Boxplot: ", input$num_var), x = "", y = input$num_var) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Histogram
    output$plot_hist <- renderPlot({
      req(input$num_var, data_r(), outlier_r())
      v <- data_r()[[input$num_var]]
      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 2)

      det <- outlier_r()$details[[input$num_var]]
      plot_df <- data.frame(val = clean_v)

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = val)) +
        ggplot2::geom_histogram(bins = 30, fill = "#3b82f6", color = "#ffffff", alpha = 0.8) +
        ggplot2::labs(title = paste0("Histogram & Bounds: ", input$num_var), x = input$num_var, y = "Frequency") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

      if (!is.null(det)) {
        p <- p +
          ggplot2::geom_vline(xintercept = det$lower_bound, color = "#ef4444", linetype = "dashed", linewidth = 1) +
          ggplot2::geom_vline(xintercept = det$upper_bound, color = "#ef4444", linetype = "dashed", linewidth = 1)
      }
      p
    })

    # Flagged rows
    output$table_flagged_rows <- DT::renderDataTable({
      req(input$num_var, data_r(), outlier_r())
      det <- outlier_r()$details[[input$num_var]]
      if (is.null(det) || length(det$outlier_indices) == 0) {
        return(DT::datatable(
          data.frame(Status = "No observations outside IQR bounds for this variable."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      ids <- det$outlier_indices
      sub_df <- data_r()[ids, , drop = FALSE]
      sub_df$Row_Index <- ids
      sub_df$Unusual_Value <- data_r()[[input$num_var]][ids]
      cols <- c("Row_Index", "Unusual_Value", setdiff(names(sub_df), c("Row_Index", "Unusual_Value")))

      DT::datatable(
        utils::head(sub_df[, cols, drop = FALSE], 50),
        options = list(pageLength = 8, scrollX = TRUE, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
