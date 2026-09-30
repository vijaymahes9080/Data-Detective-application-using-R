# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_outliers.R
# Description: Outlier investigation, IQR bounds, boxplots & unusual records
# ==============================================================================

mod_outliers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "p-3",
      # Outliers Summary Panel
      div(
        class = "panel-box mb-3",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap mb-2",
          h5(class = "panel-title mb-0", icon("chart-simple"), " Outlier Summary Across Numeric Variables"),
          span(class = "badge bg-info text-white", "IQR Method: 1.5 \u00d7 IQR")
        ),
        p(class = "small text-muted mb-3",
          strong("Important Principle: "),
          "An outlier is a statistically unusual observation, not automatically an error. ",
          "Outliers should be investigated to determine whether they represent data entry errors, measurement artifacts, or valid heavy-tailed behavior."
        ),
        DT::dataTableOutput(ns("table_outliers_summary"))
      ),

      # Variable Drilldown Section
      div(
        class = "panel-box mt-3",
        h5(class = "panel-title", icon("magnifying-glass"), " Detailed Variable Outlier Drilldown"),
        div(
          class = "row align-items-center mb-3",
          div(
            class = "col-md-5",
            selectInput(ns("select_var"), "Select Numeric Variable to Inspect:", choices = NULL, width = "100%")
          ),
          div(
            class = "col-md-7",
            uiOutput(ns("var_bounds_info"))
          )
        ),
        div(
          class = "row",
          div(
            class = "col-md-6",
            div(class = "plot-container", plotOutput(ns("plot_boxplot"), height = "320px"))
          ),
          div(
            class = "col-md-6",
            div(class = "plot-container", plotOutput(ns("plot_histogram"), height = "320px"))
          )
        ),
        div(
          class = "mt-4",
          h6("Flagged Unusual Observations in Selected Variable:"),
          DT::dataTableOutput(ns("table_unusual_records"))
        )
      )
    )
  )
}

mod_outliers_server <- function(id, data_r, outlier_r) {
  moduleServer(id, function(input, output, session) {

    # Update variable choices when outlier_r changes
    observe({
      req(outlier_r())
      if (outlier_r()$has_numeric && length(outlier_r()$numeric_columns) > 0) {
        current_sel <- input$select_var
        choices <- outlier_r()$numeric_columns
        selected <- if (!is.null(current_sel) && current_sel %in% choices) current_sel else choices[1]
        updateSelectInput(session, "select_var", choices = choices, selected = selected)
      } else {
        updateSelectInput(session, "select_var", choices = character(0))
      }
    })

    # Summary Table
    output$table_outliers_summary <- DT::renderDataTable({
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
        Valid_Count = format_number(s_df$n_valid),
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
        options = list(pageLength = 10, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Variable Bounds Info UI
    output$var_bounds_info <- renderUI({
      req(input$select_var, outlier_r())
      det <- outlier_r()$details[[input$select_var]]
      req(det)

      div(
        class = "alert alert-secondary mb-0 p-2 small",
        strong("Calculated Thresholds: "),
        paste0("Q1 = ", round(det$q1, 2), " | Q3 = ", round(det$q3, 2), " | IQR = ", round(det$iqr, 2)),
        br(),
        strong("IQR Bounds [Q1 \u2212 1.5\u00d7IQR, Q3 + 1.5\u00d7IQR]: "),
        paste0("[", round(det$lower_bound, 2), ", ", round(det$upper_bound, 2), "]"),
        br(),
        span(class = "text-danger", paste0("Outliers detected: ", length(det$outlier_indices), " (Lower: ", det$is_lower_count, ", Upper: ", det$is_upper_count, ")"))
      )
    })

    # Boxplot
    output$plot_boxplot <- renderPlot({
      req(input$select_var, data_r())
      v <- data_r()[[input$select_var]]
      req(is.numeric(v))

      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 0)

      plot_df <- data.frame(val = clean_v, var = input$select_var)

      ggplot2::ggplot(plot_df, ggplot2::aes(x = var, y = val)) +
        ggplot2::geom_boxplot(fill = "#e0e7ff", color = "#4338ca", outlier.color = "#ef4444", outlier.size = 2.5, alpha = 0.8) +
        ggplot2::labs(
          title = paste0("Boxplot with Outliers: ", input$select_var),
          x = "",
          y = input$select_var
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Histogram with Bounds
    output$plot_histogram <- renderPlot({
      req(input$select_var, data_r(), outlier_r())
      v <- data_r()[[input$select_var]]
      req(is.numeric(v))

      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 0)

      det <- outlier_r()$details[[input$select_var]]
      plot_df <- data.frame(val = clean_v)

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = val)) +
        ggplot2::geom_histogram(bins = 30, fill = "#3b82f6", color = "#ffffff", alpha = 0.8) +
        ggplot2::labs(
          title = paste0("Distribution & IQR Thresholds: ", input$select_var),
          x = input$select_var,
          y = "Frequency"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

      if (!is.null(det)) {
        p <- p +
          ggplot2::geom_vline(xintercept = det$lower_bound, color = "#ef4444", linetype = "dashed", linewidth = 1) +
          ggplot2::geom_vline(xintercept = det$upper_bound, color = "#ef4444", linetype = "dashed", linewidth = 1)
      }

      p
    })

    # Flagged records table
    output$table_unusual_records <- DT::renderDataTable({
      req(input$select_var, data_r(), outlier_r())
      out_df <- get_column_outliers(data_r(), outlier_r(), input$select_var)

      if (nrow(out_df) == 0) {
        return(DT::datatable(
          data.frame(Status = "No observations outside IQR bounds for this variable."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        head(out_df, 50),
        options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
