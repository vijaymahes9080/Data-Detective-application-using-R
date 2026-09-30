# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: R/report_generator.R
# Description: Automated investigation report generation (HTML/CSV) and export helpers
# ==============================================================================

#' Generate a self-contained HTML Dataset Investigation Report
#' @param profile Dataset profile output
#' @param quality_info Output from calculate_quality_score()
#' @param missing_info Output from analyze_missing()
#' @param duplicate_info Output from analyze_duplicates()
#' @param outlier_info Output from analyze_outliers()
#' @param constant_info Output from analyze_constants()
#' @param correlation_info Output from analyze_correlations()
#' @param leakage_info Output from analyze_leakage_indicators()
#' @param representation_info Output from analyze_representation_indicators()
#' @param findings_result Output from generate_findings()
#' @param file_name Dataset source file name
#' @return String containing complete HTML report
generate_html_report <- function(profile,
                                 quality_info,
                                 missing_info,
                                 duplicate_info,
                                 outlier_info,
                                 constant_info,
                                 correlation_info,
                                 leakage_info,
                                 representation_info,
                                 findings_result,
                                 file_name = "dataset.csv") {

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  findings_df <- findings_result$findings

  # Format Profile Table rows
  profile_table_html <- ""
  if (nrow(profile$columns_summary) > 0) {
    p_rows <- vapply(seq_len(nrow(profile$columns_summary)), function(i) {
      r <- profile$columns_summary[i, ]
      sprintf(
        "<tr><td><strong>%s</strong></td><td>%s</td><td>%s (%s%%)</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
        r$column, r$data_type, format_number(r$missing_count), round(r$missing_pct * 100, 1),
        format_number(r$unique_count), ifelse(is.na(r$min), "-", r$min),
        ifelse(is.na(r$max), "-", r$max), ifelse(is.na(r$mean), "-", as.character(r$mean))
      )
    }, character(1))
    profile_table_html <- paste(p_rows, collapse = "\n")
  }

  # Format Findings Cards HTML
  findings_cards_html <- ""
  if (nrow(findings_df) > 0) {
    f_cards <- vapply(seq_len(nrow(findings_df)), function(i) {
      f <- findings_df[i, ]
      badge_color <- switch(f$severity, "HIGH" = "#ef4444", "WARNING" = "#f59e0b", "INFO" = "#3b82f6", "#6b7280")
      sprintf(
        '<div class="finding-card border-%s">
          <div class="finding-header">
            <span class="badge" style="background-color: %s;">%s</span>
            <span class="finding-category">%s</span>
            <span class="finding-id">[%s]</span>
            <span class="finding-col">Column: <strong>%s</strong></span>
          </div>
          <div class="finding-body">
            <p><strong>Description:</strong> %s</p>
            <p><strong>Evidence:</strong> %s</p>
            <p><strong>Why Investigate:</strong> %s</p>
            <p><strong>Recommended Action:</strong> <em>%s</em></p>
          </div>
        </div>',
        tolower(f$severity), badge_color, f$severity, f$category, f$id, f$column,
        f$description, f$evidence, f$why_investigate, f$recommendation
      )
    }, character(1))
    findings_cards_html <- paste(f_cards, collapse = "\n")
  } else {
    findings_cards_html <- "<p><em>No investigation flags were triggered for this dataset.</em></p>"
  }

  # Quality Score Breakdown HTML
  score_html <- sprintf(
    '<div class="score-card">
      <div class="score-number" style="color: %s;">%d / 100</div>
      <div class="score-status" style="background-color: %s;">%s</div>
      <p class="score-note">Custom composite indicator based on detected issues.</p>
      <p class="score-explanation">%s</p>
    </div>',
    quality_info$status_color, quality_info$score, quality_info$status_color,
    quality_info$status, quality_info$explanation
  )

  # Full HTML Document
  html_doc <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Data Detective Investigation Report - %s</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; color: #1e293b; background-color: #f8fafc; margin: 0; padding: 40px 20px; }
    .container { max-width: 1000px; margin: 0 auto; background: #ffffff; padding: 40px; border-radius: 12px; box-shadow: 0 4px 20px rgba(0,0,0,0.06); }
    h1 { font-size: 2.2rem; color: #0f172a; margin-bottom: 4px; border-bottom: 2px solid #3b82f6; padding-bottom: 12px; }
    h2 { font-size: 1.4rem; color: #1e293b; margin-top: 36px; border-left: 4px solid #3b82f6; padding-left: 12px; }
    .tagline { color: #64748b; font-size: 1.1rem; margin-top: 0; }
    .meta-box { background: #f1f5f9; padding: 16px 20px; border-radius: 8px; margin-bottom: 24px; font-size: 0.95rem; }
    .meta-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 12px; }
    .score-card { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 24px; text-align: center; margin: 20px 0; }
    .score-number { font-size: 3rem; font-weight: 800; line-height: 1; }
    .score-status { display: inline-block; color: #ffffff; font-weight: 700; padding: 4px 16px; border-radius: 20px; font-size: 0.9rem; margin-top: 8px; }
    .score-note { color: #64748b; font-size: 0.85rem; margin: 6px 0; }
    .score-explanation { font-size: 0.9rem; color: #334155; margin-top: 12px; max-width: 800px; margin-left: auto; margin-right: auto; }
    table { width: 100%%; border-collapse: collapse; margin-top: 16px; font-size: 0.9rem; }
    th, td { padding: 10px 12px; text-align: left; border-bottom: 1px solid #e2e8f0; }
    th { background: #f1f5f9; color: #334155; font-weight: 600; }
    .badge { color: white; padding: 3px 8px; border-radius: 4px; font-size: 0.75rem; font-weight: 700; display: inline-block; }
    .finding-card { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 16px 20px; margin-bottom: 16px; box-shadow: 0 1px 3px rgba(0,0,0,0.04); }
    .finding-card.border-high { border-left: 5px solid #ef4444; }
    .finding-card.border-warning { border-left: 5px solid #f59e0b; }
    .finding-card.border-info { border-left: 5px solid #3b82f6; }
    .finding-header { display: flex; align-items: center; gap: 12px; margin-bottom: 10px; flex-wrap: wrap; }
    .finding-category { font-weight: 600; color: #0f172a; }
    .finding-id { font-family: monospace; color: #64748b; }
    .finding-body p { margin: 6px 0; font-size: 0.92rem; }
    .disclaimer-box { background: #fffbeb; border: 1px solid #fde68a; color: #92400e; padding: 16px 20px; border-radius: 8px; margin-top: 40px; font-size: 0.88rem; }
  </style>
</head>
<body>
  <div class="container">
    <h1>DATA DETECTIVE</h1>
    <p class="tagline">Automated Dataset Investigation Report &bull; <em>Upload. Investigate. Understand.</em></p>

    <div class="meta-box">
      <div class="meta-grid">
        <div><strong>Dataset:</strong> %s</div>
        <div><strong>Rows:</strong> %s</div>
        <div><strong>Columns:</strong> %s</div>
        <div><strong>Analysis Time:</strong> %s</div>
      </div>
    </div>

    <h2>1. Executive Summary</h2>
    <p>%s</p>

    <h2>2. Data Quality Indicator</h2>
    %s

    <h2>3. Dataset Profile & Summary Statistics</h2>
    <table>
      <thead>
        <tr>
          <th>Column</th><th>Type</th><th>Missing</th><th>Unique</th><th>Min</th><th>Max</th><th>Mean</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>

    <h2>4. Investigation Findings (%d Total)</h2>
    %s

    <h2>5. Recommended Next Investigation Steps</h2>
    <ol>
      <li><strong>Address Critical & High Severity Findings:</strong> Review all highlighted high-severity cards above, particularly missingness and potential leakage flags.</li>
      <li><strong>Verify Outlier Observations:</strong> For skewed variables, verify whether extreme points reflect data collection artifacts or legitimate heavy tails.</li>
      <li><strong>Evaluate Multicollinearity:</strong> Review highly correlated variable pairs (|r| &ge; 0.70) before deploying linear or logistic regression models.</li>
      <li><strong>Review Representation Disparities:</strong> Inspect any differential missingness across categorical groups to mitigate localized performance drops.</li>
    </ol>

    <div class="disclaimer-box">
      <strong>Statistical Investigation Disclaimer:</strong><br>
      This report is an automated statistical investigation. Detected patterns are indicators for further investigation and should not automatically be interpreted as data errors, causal relationships, confirmed leakage, or confirmed bias.
    </div>
  </div>
</body>
</html>',
    file_name,
    file_name,
    format_number(profile$n_rows),
    format_number(profile$n_cols),
    timestamp,
    findings_result$executive_summary,
    score_html,
    profile_table_html,
    findings_result$total_findings,
    findings_cards_html
  )

  return(html_doc)
}
