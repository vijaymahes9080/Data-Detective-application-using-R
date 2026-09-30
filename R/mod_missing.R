# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_missing.R
# Description: Module 4 — Missing Value Analysis, Bar Chart, & Threshold Filters
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_missing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Top Stats Row
    div(
      class = "row mb-3",
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "TOTAL MISSING CELLS"), h4(class = "mb-0", textOutput(ns("total_missing"))))),
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "OVERALL MISSINGNESS"), h4(class = "mb-0", textOutput(ns("missing_pct"))))),
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "COMPLETE CASES (ROWS)"), h4(class = "mb-0 text-success", textOutput(ns("complete_cases"))))),
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "INCOMPLETE ROWS"), h4(class = "mb-0 text-danger", textOutput(ns("incomplete_cases")))))
    ),

    # Chart & Threshold Filter Section
    div(
      class = "panel-box mb-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-3 flex-wrap",
        h5(class = "panel-title mb-0", icon("chart-bar"), " Missing Percentage by Column"),
        div(
          class = "d-flex align-items-center gap-2",
          tags$span(class = "small text-muted fw-bold", "Highlight Threshold:"),
          selectInput(
            ns("thresh_filter"),
            label = NULL,
            choices = c("All (> 0%)" = 0, "> 5%" = 5, "> 10%" = 10, "> 20%" = 20, "> 30%" = 30, "> 50%" = 50),
            selected = 0,
            width = "140px"
          )
        )
      ),
      plotOutput(ns("plot_missing_bars"), height = "340px")
    ),

    # Missing Detail Data Table
    div(
      class = "panel-box",
      h5(class = "panel-title", icon("table-list"), " Detailed Missing Values Summary"),
      DT::dataTableOutput(ns("table_missing"))
    )
  )
}

mod_missing_server <- function(id, missing_r) {
  moduleServer(id, function(input, output, session) {

    # KPI Outputs
    output$total_missing <- renderText({
      req(missing_r())
      format_number(missing_r()$total_missing)
    })

    output$missing_pct <- renderText({
      req(missing_r())
      paste0(missing_r()$missing_pct, "%")
    })

    output$complete_cases <- renderText({
      req(missing_r())
      paste0(format_number(missing_r()$complete_cases), " (", missing_r()$complete_cases_pct, "%)")
    })

    output$incomplete_cases <- renderText({
      req(missing_r())
      format_number(missing_r()$incomplete_cases)
    })

    # Missing Bars Plot
    output$plot_missing_bars <- renderPlot({
      req(missing_r())
      m_df <- missing_r()$column_summary
      req(nrow(m_df) > 0)

      thresh <- as.numeric(input$thresh_filter)
      plot_df <- m_df[m_df$missing_pct >= thresh & m_df$missing_count > 0, ]

      if (nrow(plot_df) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 1, y = 1, label = paste0("No columns exceed the ", thresh, "% missingness threshold."), size = 4.5, color = "#10b981") +
                 ggplot2::theme_void())
      }

      plot_df <- utils::head(plot_df, 15)
      plot_df$column <- factor(plot_df$column, levels = rev(plot_df$column))

      ggplot2::ggplot(plot_df, ggplot2::aes(x = column, y = missing_pct, fill = severity)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(missing_pct, "%")), hjust = -0.15, size = 3.5, color = "#1e293b") +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = c("Low" = "#3b82f6", "Moderate" = "#f59e0b", "High" = "#ef4444", "Critical" = "#7f1d1d", "Complete" = "#10b981"),
          drop = FALSE
        ) +
        ggplot2::scale_y_continuous(limits = c(0, max(100, max(plot_df$missing_pct) * 1.15))) +
        ggplot2::labs(title = "Missing Percentage per Column", x = "", y = "Missing Percentage (%)", fill = "Severity") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Detail Table
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
  })
}
