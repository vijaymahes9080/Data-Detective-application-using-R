# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_quality.R
# Description: Module 3 — Data Quality Indicator & Granular Penalty Breakdown
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_quality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row",
      # Left column: Circular gauge & Status
      div(
        class = "col-lg-4 col-md-12",
        div(
          class = "panel-box text-center mb-3",
          h5(class = "panel-title", icon("award"), " DATA QUALITY INDICATOR"),
          uiOutput(ns("quality_gauge")),
          p(class = "text-muted small mt-3",
            "Custom composite indicator based on detected issues. Not presented as an absolute scientific truth."
          ),
          div(class = "border-top pt-2 mt-2", uiOutput(ns("severity_summary_pills")))
        )
      ),

      # Right column: Detailed deductions & rules
      div(
        class = "col-lg-8 col-md-12",
        div(
          class = "panel-box mb-3",
          h5(class = "panel-title", icon("calculator"), " Quality Score Deductions Breakdown"),
          p(class = "small text-muted", "Base baseline is 100 points. Penalties are systematically deducted according to transparent heuristic rules:"),
          uiOutput(ns("deductions_list"))
        ),
        div(
          class = "panel-box",
          h5(class = "panel-title", icon("circle-info"), " Heuristic Severity Tier Standards"),
          tags$table(
            class = "table table-sm table-bordered small mb-0",
            tags$thead(tags$tr(tags$th("Severity"), tags$th("Criteria"), tags$th("Example"))),
            tags$tbody(
              tags$tr(tags$td(span(class = "badge bg-danger", "HIGH")), tags$td("> 20% Missing, Duplicate rows \u2265 5%, Constant variables"), tags$td("Income contains 32% missing values.")),
              tags$tr(tags$td(span(class = "badge bg-warning text-dark", "MEDIUM")), tags$td("5% \u2013 20% Missing, 1% \u2013 5% Duplicates, Severe Imbalance"), tags$td("Variable has a high proportion of extreme values.")),
              tags$tr(tags$td(span(class = "badge bg-info text-white", "LOW")), tags$td("< 5% Missing, Moderate outliers, Minor skewness"), tags$td("3 observations flagged beyond 1.5 \u00d7 IQR.")),
              tags$tr(tags$td(span(class = "badge bg-secondary", "INFO")), tags$td("Baseline metrics, verified completeness"), tags$td("No duplicate rows detected."))
            )
          )
        )
      )
    )
  )
}

mod_quality_server <- function(id, quality_r, findings_r) {
  moduleServer(id, function(input, output, session) {

    # Quality Gauge UI
    output$quality_gauge <- renderUI({
      req(quality_r())
      q <- quality_r()

      tagList(
        div(
          class = "quality-score-circle",
          style = paste0("border-color: ", q$status_color, "; color: ", q$status_color, ";"),
          div(class = "score-val", q$score),
          div(class = "score-max", "/ 100")
        ),
        div(
          class = "quality-status-pill mt-2",
          style = paste0("background-color: ", q$status_color, "; color: #ffffff;"),
          q$status
        )
      )
    })

    # Severity Pills Summary
    output$severity_summary_pills <- renderUI({
      req(findings_r())
      f <- findings_r()
      n_high <- sum(f$severity == "HIGH")
      n_med <- sum(f$severity %in% c("MEDIUM", "WARNING"))
      n_low <- sum(f$severity == "LOW")
      n_info <- sum(f$severity == "INFO")

      div(
        class = "d-flex justify-content-center gap-2 flex-wrap",
        span(class = "badge bg-danger", paste0("High: ", n_high)),
        span(class = "badge bg-warning text-dark", paste0("Medium: ", n_med)),
        span(class = "badge bg-info text-white", paste0("Low: ", n_low)),
        span(class = "badge bg-secondary", paste0("Info: ", n_info))
      )
    })

    # Deductions List UI
    output$deductions_list <- renderUI({
      req(quality_r())
      q <- quality_r()

      if (length(q$deductions) == 0) {
        return(div(class = "alert alert-success p-3", icon("circle-check"), " Excellent! No data-quality penalties were triggered."))
      }

      d_items <- lapply(names(q$deductions), function(k) {
        item <- q$deductions[[k]]
        div(
          class = "p-2 mb-2 border rounded bg-light d-flex justify-content-between align-items-center",
          div(
            strong(item$name),
            div(class = "text-muted small", item$metric)
          ),
          span(class = "text-danger fw-bold fs-6", paste0("-", item$points, " pts"))
        )
      })

      tagList(d_items)
    })
  })
}
