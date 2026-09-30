# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_report.R
# Description: Module 12 — Investigation Report Dossier & Offline HTML Export
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "panel-box mb-3",
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap",
        div(
          h4(class = "panel-title mb-0", icon("file-lines"), " DATASET INVESTIGATION REPORT"),
          div(class = "text-muted small", "Download standalone HTML investigation dossiers and structured CSV audit summaries.")
        ),
        div(
          class = "d-flex gap-2 flex-wrap mt-2 mt-md-0",
          downloadButton(ns("dl_html"), "Download HTML Report", class = "btn btn-primary"),
          downloadButton(ns("dl_findings"), "Export Findings CSV", class = "btn btn-outline-secondary"),
          downloadButton(ns("dl_profile"), "Export Profile CSV", class = "btn btn-outline-secondary")
        )
      )
    ),

    # Live Report Dossier Preview
    div(
      class = "panel-box",
      h5(class = "panel-title", icon("eye"), " Live Investigation Dossier Preview"),
      uiOutput(ns("live_dossier"))
    )
  )
}

mod_report_server <- function(id, profile_r, quality_r, findings_r, filename_r) {
  moduleServer(id, function(input, output, session) {

    # Live preview UI
    output$live_dossier <- renderUI({
      req(profile_r(), quality_r(), findings_r())
      p <- profile_r()
      q <- quality_r()
      f <- findings_r()
      fname <- if (!is.null(filename_r())) filename_r() else "dataset.csv"

      n_high <- sum(f$severity == "HIGH")
      n_med <- sum(f$severity %in% c("MEDIUM", "WARNING"))
      n_low <- sum(f$severity == "LOW")

      tagList(
        div(
          class = "p-4 bg-white border rounded shadow-sm",
          div(class = "border-bottom pb-2 mb-3",
              h3("DATASET INVESTIGATION DOSSIER"),
              p(class = "text-muted mb-0", "Automated statistical data quality & exploratory audit report")),
          div(
            class = "alert alert-light border small mb-3",
            div(class = "row",
                div(class = "col-md-3", strong("Dataset: "), fname),
                div(class = "col-md-3", strong("Rows: "), format_number(p$n_rows)),
                div(class = "col-md-3", strong("Columns: "), format_number(p$n_cols)),
                div(class = "col-md-3", strong("Quality Indicator: "), span(style = paste0("color: ", q$status_color, "; font-weight: bold;"), paste0(q$score, "/100 (", q$status, ")")))
            )
          ),
          h5("1. Key Investigation Findings"),
          p(paste0("Total issues detected: ", nrow(f), " (High Severity: ", n_high, " | Medium Severity: ", n_med, " | Low Severity: ", n_low, ")")),
          if (nrow(f) > 0) {
            tags$ul(
              class = "list-group list-group-flush mb-3",
              lapply(seq_len(min(6, nrow(f))), function(i) {
                row <- f[i, ]
                b_color <- switch(row$severity, "HIGH" = "bg-danger", "MEDIUM" = "bg-warning text-dark", "LOW" = "bg-info text-white", "bg-secondary")
                tags$li(
                  class = "list-group-item small",
                  span(class = paste0("badge me-2 ", b_color), row$severity),
                  strong(row$category), " \u2014 ", row$message,
                  div(class = "text-muted small", paste0("Evidence: ", row$evidence, " | Next check: ", row$recommendation))
                )
              })
            )
          } else {
            p(class = "text-success", "No major automated issues detected under the selected checks.")
          },
          h5("2. Recommended Next Investigation Steps"),
          tags$ol(
            class = "small",
            tags$li("Examine columns with High severity missingness and verify data logging protocols."),
            tags$li("Inspect duplicate rows before modeling to prevent artificial inflation of sample weights."),
            tags$li("Evaluate multicollinear variable pairs to prevent coefficient instability in regression models."),
            tags$li("Assess representation distributions across sub-populations.")
          ),
          div(
            class = "alert alert-warning small mt-4 mb-0",
            strong("Report Disclaimer: "),
            "This report is an automated statistical investigation. Detected patterns are indicators for further investigation and should not automatically be interpreted as data errors, causal relationships, confirmed leakage, or confirmed bias."
          )
        )
      )
    })

    # Download HTML
    output$dl_html <- downloadHandler(
      filename = function() {
        base_name <- tools::file_path_sans_ext(if (!is.null(filename_r())) filename_r() else "dataset")
        paste0("Data_Detective_Report_", base_name, "_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        p <- profile_r()
        q <- quality_r()
        f <- findings_r()
        fname <- if (!is.null(filename_r())) filename_r() else "dataset.csv"

        # Build clean standalone HTML
        html_doc <- sprintf('<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Data Detective Report - %s</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; line-height: 1.6; color: #1e293b; background: #f8fafc; padding: 40px 20px; margin: 0; }
    .container { max-width: 900px; margin: 0 auto; background: #fff; padding: 40px; border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.06); }
    h1 { color: #0f172a; border-bottom: 2px solid #2563eb; padding-bottom: 10px; }
    h2 { color: #1e293b; margin-top: 30px; border-left: 4px solid #2563eb; padding-left: 10px; }
    .meta { background: #f1f5f9; padding: 15px; border-radius: 8px; margin: 20px 0; display: grid; grid-template-columns: repeat(4, 1fr); gap: 10px; }
    .badge { color: white; padding: 3px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: bold; }
    .badge-high { background: #ef4444; }
    .badge-medium { background: #f59e0b; color: #000; }
    .badge-low { background: #3b82f6; }
    .finding-box { border-left: 4px solid #cbd5e1; padding: 10px 15px; margin-bottom: 12px; background: #f8fafc; border-radius: 0 6px 6px 0; }
    .disclaimer { background: #fffbeb; border: 1px solid #fde68a; color: #92400e; padding: 15px; border-radius: 8px; margin-top: 30px; font-size: 0.85rem; }
  </style>
</head>
<body>
  <div class="container">
    <h1>DATA DETECTIVE</h1>
    <p>Automated Dataset Investigation Report &bull; <em>Upload. Investigate. Understand.</em></p>
    <div class="meta">
      <div><strong>Dataset:</strong> %s</div>
      <div><strong>Rows:</strong> %s</div>
      <div><strong>Columns:</strong> %s</div>
      <div><strong>Quality Score:</strong> %s/100 (%s)</div>
    </div>
    <h2>1. Executive Summary</h2>
    <p>The dataset contains %s rows and %s columns. An automated audit identified %d investigation flag(s) across quality, missingness, duplicates, outliers, and relational dimensions.</p>
    <h2>2. Key Findings</h2>
    %s
    <h2>3. Recommended Next Checks</h2>
    <ol>
      <li>Audit high-missingness variables and missingness mechanisms.</li>
      <li>Examine exact duplicate copies and determine logging causes.</li>
      <li>Review strong collinear pairs before predictive modeling.</li>
      <li>Screen representation balance across sub-demographics.</li>
    </ol>
    <div class="disclaimer">
      <strong>Statistical Investigation Disclaimer:</strong><br>
      This report is an automated statistical investigation. Detected patterns are indicators for further investigation and should not automatically be interpreted as data errors, causal relationships, confirmed leakage, or confirmed bias.
    </div>
  </div>
</body>
</html>',
          fname, fname, format_number(p$n_rows), format_number(p$n_cols), q$score, q$status,
          format_number(p$n_rows), format_number(p$n_cols), nrow(f),
          if (nrow(f) > 0) {
            paste(vapply(seq_len(nrow(f)), function(i) {
              r <- f[i, ]
              b_cls <- switch(r$severity, "HIGH" = "badge-high", "MEDIUM" = "badge-medium", "LOW" = "badge-low", "badge-low")
              sprintf('<div class="finding-box"><span class="badge %s">%s</span> <strong>%s</strong> (Column: %s)<p style="margin: 4px 0;">%s</p><small style="color: #64748b;">Evidence: %s | Action: %s</small></div>',
                      b_cls, r$severity, r$category, r$column, r$message, r$evidence, r$recommendation)
            }, character(1)), collapse = "\n")
          } else {
            "<p>No major automated issues detected.</p>"
          }
        )
        writeLines(html_doc, con = file, useBytes = TRUE)
      }
    )

    # Download Findings CSV
    output$dl_findings <- downloadHandler(
      filename = function() paste0("Data_Detective_Findings_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        req(findings_r())
        utils::write.csv(findings_r(), file, row.names = FALSE)
      }
    )

    # Download Profile CSV
    output$dl_profile <- downloadHandler(
      filename = function() paste0("Data_Detective_Profile_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        req(profile_r())
        utils::write.csv(profile_r()$columns_summary, file, row.names = FALSE)
      }
    )
  })
}
