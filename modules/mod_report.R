# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_report.R
# Description: Report generation, live report preview, and CSV/HTML downloads
# ==============================================================================

mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "p-3",
      # Download Controls Banner
      div(
        class = "panel-box mb-3",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap",
          div(
            h4(class = "panel-title mb-0", icon("file-invoice"), " DATASET INVESTIGATION REPORT"),
            div(class = "text-muted small", "Download publication-ready self-contained reports and structured CSV audit summaries.")
          ),
          div(
            class = "d-flex gap-2 flex-wrap mt-2 mt-md-0",
            downloadButton(ns("dl_html_report"), "Download HTML Report", class = "btn btn-primary"),
            downloadButton(ns("dl_findings_csv"), "Download Findings CSV", class = "btn btn-outline-secondary"),
            downloadButton(ns("dl_profile_csv"), "Download Profile CSV", class = "btn btn-outline-secondary")
          )
        )
      ),

      # Live Report Preview Panel
      div(
        class = "panel-box",
        h5(class = "panel-title", icon("eye"), " Live Report Dossier Preview"),
        uiOutput(ns("report_live_preview"))
      )
    )
  )
}

mod_report_server <- function(id, data_r, profile_r, quality_r, missing_r, duplicate_r, outlier_r, constant_r, corr_r, findings_r, file_name_r) {
  moduleServer(id, function(input, output, session) {

    # Live Report Preview UI
    output$report_live_preview <- renderUI({
      req(profile_r(), quality_r(), findings_r())
      fname <- if (!is.null(file_name_r()) && nzchar(file_name_r())) file_name_r() else "dataset.csv"
      f_df <- findings_r()$findings
      p_sum <- profile_r()$columns_summary

      tagList(
        div(
          class = "report-preview-container p-4 bg-white border rounded shadow-sm",
          div(class = "border-bottom pb-3 mb-3",
              h2("DATA DETECTIVE INVESTIGATION DOSSIER"),
              p(class = "text-muted mb-0", "Automated statistical data-quality & exploratory-analysis audit")),
          div(
            class = "alert alert-light border small mb-3",
            div(class = "row",
                div(class = "col-md-3", strong("Dataset: "), fname),
                div(class = "col-md-3", strong("Rows: "), format_number(profile_r()$n_rows)),
                div(class = "col-md-3", strong("Columns: "), format_number(profile_r()$n_cols)),
                div(class = "col-md-3", strong("Quality Indicator: "), span(style = paste0("color: ", quality_r()$status_color, "; font-weight: bold;"), paste0(quality_r()$score, "/100 (", quality_r()$status, ")")))
            )
          ),
          h4("1. Executive Summary"),
          p(class = "exec-summary-text", findings_r()$executive_summary),
          h4("2. Detected Findings Overview"),
          p(paste0("Total Flags: ", findings_r()$total_findings, " (High: ", findings_r()$n_high, " | Warning: ", findings_r()$n_warning, " | Info: ", findings_r()$n_info, ")")),
          h4("3. Practical Next Steps"),
          tags$ol(
            tags$li("Examine high-severity missingness and verify data collection protocols."),
            tags$li("Review flagged duplicate records to eliminate redundant logging."),
            tags$li("Evaluate multicollinear variable pairs before model training."),
            tags$li("Assess category imbalance and representation across target cohorts.")
          ),
          div(
            class = "alert alert-warning small mt-4 mb-0",
            strong("Report Disclaimer: "),
            "This report is an automated statistical investigation. Detected patterns are indicators for further investigation and should not automatically be interpreted as data errors, causal relationships, confirmed leakage, or confirmed bias."
          )
        )
      )
    })

    # Download HTML Report Handler
    output$dl_html_report <- downloadHandler(
      filename = function() {
        base_name <- tools::file_path_sans_ext(if (!is.null(file_name_r())) file_name_r() else "dataset")
        paste0("Data_Detective_Report_", base_name, "_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        fname <- if (!is.null(file_name_r()) && nzchar(file_name_r())) file_name_r() else "dataset.csv"
        html_content <- generate_html_report(
          profile = profile_r(),
          quality_info = quality_r(),
          missing_info = missing_r(),
          duplicate_info = duplicate_r(),
          outlier_info = outlier_r(),
          constant_info = constant_r(),
          correlation_info = corr_r(),
          leakage_info = NULL,
          representation_info = NULL,
          findings_result = findings_r(),
          file_name = fname
        )
        writeLines(html_content, con = file, useBytes = TRUE)
      }
    )

    # Download Findings CSV Handler
    output$dl_findings_csv <- downloadHandler(
      filename = function() {
        paste0("Data_Detective_Findings_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(findings_r())
        utils::write.csv(findings_r()$findings, file, row.names = FALSE)
      }
    )

    # Download Profile CSV Handler
    output$dl_profile_csv <- downloadHandler(
      filename = function() {
        paste0("Data_Detective_Profile_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(profile_r())
        utils::write.csv(profile_r()$columns_summary, file, row.names = FALSE)
      }
    )
  })
}
