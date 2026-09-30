# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_duplicates.R
# Description: Module 5 — Duplicate Record Investigation & Candidate Keys
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_duplicates_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Top KPI Metrics Row
    div(
      class = "row mb-3",
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "TOTAL ROWS"), h4(class = "mb-0", textOutput(ns("total_rows"))))),
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "UNIQUE ROWS"), h4(class = "mb-0 text-success", textOutput(ns("unique_rows"))))),
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "DUPLICATE ROWS"), h4(class = "mb-0 text-danger", textOutput(ns("duplicate_rows"))))),
      div(class = "col-md-3 col-6 mb-2", div(class = "p-3 bg-white border rounded text-center", div(class = "metric-title", "DUPLICATE RATE"), h4(class = "mb-0", textOutput(ns("duplicate_pct")))))
    ),

    # Status Banner
    div(class = "mb-3", uiOutput(ns("dup_alert_banner"))),

    # Duplicate Records Table
    div(
      class = "panel-box",
      div(
        class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
        h5(class = "panel-title mb-0", icon("copy"), " Inspect Duplicate Rows"),
        span(class = "small text-muted", "Identical records across all columns are displayed below. Data Detective never alters or deletes records.")
      ),
      DT::dataTableOutput(ns("table_duplicates"))
    )
  )
}

mod_duplicates_server <- function(id, duplicate_r) {
  moduleServer(id, function(input, output, session) {

    output$total_rows <- renderText({
      req(duplicate_r())
      format_number(duplicate_r()$total_rows)
    })

    output$unique_rows <- renderText({
      req(duplicate_r())
      format_number(duplicate_r()$unique_rows)
    })

    output$duplicate_rows <- renderText({
      req(duplicate_r())
      format_number(duplicate_r()$duplicate_rows_count)
    })

    output$duplicate_pct <- renderText({
      req(duplicate_r())
      paste0(duplicate_r()$duplicate_pct, "%")
    })

    output$dup_alert_banner <- renderUI({
      req(duplicate_r())
      d <- duplicate_r()

      if (!d$has_duplicates) {
        div(
          class = "alert alert-success d-flex align-items-center",
          icon("check-circle", class = "fs-4 me-3"),
          div(
            strong("Zero Duplicate Records Detected"),
            div(class = "small text-muted", "Every row is unique across all columns. No data redundancy identified.")
          )
        )
      } else {
        div(
          class = "alert alert-warning d-flex align-items-center",
          icon("triangle-exclamation", class = "fs-4 me-3"),
          div(
            strong(paste0("Attention: ", format_number(d$duplicate_rows_count), " Exact Duplicate Row(s) Detected (", d$duplicate_pct, "%)")),
            div(class = "small text-secondary", "Duplicate rows artificially inflate sample size and can skew statistics. Inspect copies below.")
          )
        )
      }
    })

    output$table_duplicates <- DT::renderDataTable({
      req(duplicate_r())
      d <- duplicate_r()

      if (!d$has_duplicates || nrow(d$duplicate_records) == 0) {
        return(DT::datatable(
          data.frame(Status = "No duplicate rows to display."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        utils::head(d$duplicate_records, 100),
        options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
        rownames = TRUE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
