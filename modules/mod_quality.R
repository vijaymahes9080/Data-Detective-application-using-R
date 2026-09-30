# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_quality.R
# Description: Data Quality Score Breakdown, Missing Values, Duplicates & Constants
# ==============================================================================

mod_quality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("quality_subtabs"),

      # ---- Tab 1: Missing Values --------------------------------------------
      tabPanel(
        title = tagList(icon("magnifying-glass-chart"), " Missing Value Diagnostics"),
        div(
          class = "p-3",
          div(
            class = "row",
            div(
              class = "col-lg-4 col-md-12",
              div(
                class = "panel-box mb-3",
                h5(class = "panel-title", "Missingness Severity Heuristics"),
                p(class = "small text-muted", "Severity classifications are application heuristics (configurable in Settings):"),
                tags$ul(
                  class = "list-unstyled small",
                  tags$li(span(class = "badge bg-success me-2", "Complete"), "0% missing values"),
                  tags$li(span(class = "badge bg-info me-2", "Low"), "> 0% and \u2264 5% missing"),
                  tags$li(span(class = "badge bg-warning me-2", "Moderate"), "> 5% and \u2264 20% missing"),
                  tags$li(span(class = "badge bg-danger me-2", "High"), "> 20% and \u2264 50% missing"),
                  tags$li(span(class = "badge bg-dark me-2", "Critical"), "> 50% missing (unusable)")
                ),
                hr(),
                h6("Row-Level Missingness Summary:"),
                uiOutput(ns("row_missing_stats"))
              )
            ),
            div(
              class = "col-lg-8 col-md-12",
              div(
                class = "panel-box mb-3",
                h5(class = "panel-title", icon("chart-bar"), " Missing Percentage by Column"),
                plotOutput(ns("plot_missing_bar"), height = "320px")
              )
            )
          ),
          div(
            class = "panel-box mt-3",
            h5(class = "panel-title", "Column-by-Column Missingness Details"),
            DT::dataTableOutput(ns("table_missing"))
          )
        )
      ),

      # ---- Tab 2: Duplicate Investigation -----------------------------------
      tabPanel(
        title = tagList(icon("copy"), " Duplicate Investigation"),
        div(
          class = "p-3",
          div(
            class = "row",
            div(
              class = "col-md-6",
              div(
                class = "panel-box mb-3",
                h5(class = "panel-title", "Duplicate Records Assessment"),
                uiOutput(ns("duplicate_summary_box"))
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "panel-box mb-3",
                h5(class = "panel-title", "Potential / Near-Duplicates"),
                p(class = "small text-muted", "Near-duplicates compare records ignoring primary identifier or index columns:"),
                uiOutput(ns("potential_duplicate_info"))
              )
            )
          ),
          div(
            class = "panel-box mt-2",
            div(class = "d-flex justify-content-between align-items-center mb-2",
                h5(class = "panel-title mb-0", "View Duplicate Records"),
                span(class = "small text-muted", "Exact identical rows are shown below for inspection.")),
            DT::dataTableOutput(ns("table_duplicates"))
          )
        )
      ),

      # ---- Tab 3: Constant & Low-Variance Columns ---------------------------
      tabPanel(
        title = tagList(icon("filter-circle-xmark"), " Constant & Uninformative Variables"),
        div(
          class = "p-3",
          div(
            class = "panel-box mb-3",
            h5(class = "panel-title", "Potentially Uninformative Variables"),
            p(class = "small text-muted",
              "Identifies variables containing a single value, near-constant categorical classes (> 90% concentration), ",
              "or near-zero numeric variance. Data Detective does not automatically delete columns."),
            DT::dataTableOutput(ns("table_constants"))
          )
        )
      )
    )
  )
}

mod_quality_server <- function(id, data_r, missing_r, duplicate_r, constant_r, quality_r) {
  moduleServer(id, function(input, output, session) {

    # Row-Level Missingness Stats
    output$row_missing_stats <- renderUI({
      req(missing_r())
      rs <- missing_r()$row_summary
      req(rs)

      tagList(
        p(class = "small mb-1", strong("Complete Rows (0 missing): "), format_number(rs$complete_rows), " (", rs$complete_rows_pct, "%)"),
        p(class = "small mb-1", strong("Rows with Any Missing: "), format_number(rs$rows_with_any_missing), " (", rs$rows_with_any_missing_pct, "%)"),
        p(class = "small mb-1", strong("Rows with 1–2 Missing: "), format_number(rs$rows_1_to_2_missing)),
        p(class = "small mb-1", strong("Rows with > 2 Missing: "), format_number(rs$rows_over_2_missing)),
        p(class = "small mb-0", strong("Max Missing in a Single Row: "), rs$max_missing_in_single_row)
      )
    })

    # Missing Bar Plot
    output$plot_missing_bar <- renderPlot({
      req(missing_r())
      m_df <- missing_r()$column_summary
      req(nrow(m_df) > 0)

      # Take columns that have at least 1 missing, or top 15
      plot_df <- m_df[m_df$missing_count > 0, ]
      if (nrow(plot_df) == 0) {
        # If no missing values
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "No missing values detected in dataset!", size = 5, color = "#10b981") +
          ggplot2::theme_void()
        return(p)
      }

      plot_df <- head(plot_df, 15)
      plot_df$column <- factor(plot_df$column, levels = rev(plot_df$column))

      ggplot2::ggplot(plot_df, ggplot2::aes(x = column, y = missing_pct, fill = severity)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = c("Low" = "#3b82f6", "Moderate" = "#f59e0b", "High" = "#ef4444", "Critical" = "#7f1d1d", "Complete" = "#10b981"),
          drop = FALSE
        ) +
        ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(100, max(plot_df$missing_pct) * 1.1))) +
        ggplot2::labs(
          title = "Missing Value Proportion by Variable (Top Missing)",
          x = "",
          y = "Missing Percentage (%)",
          fill = "Severity"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "bottom",
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold", size = 13)
        )
    })

    # Missing Table
    output$table_missing <- DT::renderDataTable({
      req(missing_r())
      m_df <- missing_r()$column_summary
      req(nrow(m_df) > 0)

      disp_df <- data.frame(
        Column = m_df$column,
        Missing_Count = format_number(m_df$missing_count),
        Missing_Pct = paste0(m_df$missing_pct, "%"),
        Non_Missing_Count = format_number(m_df$non_missing_count),
        Severity = m_df$severity,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 10, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Duplicate Summary Box
    output$duplicate_summary_box <- renderUI({
      req(duplicate_r())
      d <- duplicate_r()

      if (d$duplicate_rows_count == 0) {
        div(
          class = "alert alert-success mb-0",
          h6(icon("check-circle"), " No Exact Duplicates Found"),
          p(class = "small mb-0", "Every row is unique across all columns. No identical row copies exist.")
        )
      } else {
        div(
          class = "alert alert-warning mb-0",
          h6(icon("triangle-exclamation"), paste0(format_number(d$duplicate_rows_count), " Exact Duplicate Row(s)")),
          p(class = "small mb-1", paste0(d$duplicate_pct, "% of dataset comprises identical duplicated rows.")),
          p(class = "small mb-0", strong("Unique Rows: "), format_number(d$unique_rows_count), " / ", format_number(d$total_rows))
        )
      }
    })

    # Potential Duplicate Info
    output$potential_duplicate_info <- renderUI({
      req(duplicate_r())
      d <- duplicate_r()

      if (d$potential_count == 0) {
        p(class = "small text-success", "No near-duplicates detected outside of exact duplicate matches.")
      } else {
        p(class = "small text-warning",
          paste0("Detected ", format_number(d$potential_count), " row(s) sharing identical values across non-ID columns. Inspect records below.")
        )
      }
    })

    # Duplicate Table
    output$table_duplicates <- DT::renderDataTable({
      req(duplicate_r())
      d <- duplicate_r()

      if (nrow(d$duplicate_rows) == 0) {
        return(DT::datatable(
          data.frame(Message = "No duplicate rows to display."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        head(d$duplicate_rows, 100),
        options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
        rownames = TRUE,
        class = "compact stripe hover border-table"
      )
    })

    # Constants Table
    output$table_constants <- DT::renderDataTable({
      req(constant_r())
      c_df <- constant_r()$uninformative_columns

      if (nrow(c_df) == 0) {
        return(DT::datatable(
          data.frame(Status = "All variables exhibit healthy variability. No constant or near-constant columns detected."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        c_df,
        options = list(pageLength = 10, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
