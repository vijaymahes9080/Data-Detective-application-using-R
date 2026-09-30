# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_overview.R
# Description: Dashboard Overview, Summary Metric Cards, Quality Indicator, Preview
# ==============================================================================

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Summary Metric Cards Row
    div(
      class = "metrics-grid",
      div(class = "metric-card", div(class = "metric-title", "TOTAL ROWS"), div(class = "metric-value", textOutput(ns("card_rows")))),
      div(class = "metric-card", div(class = "metric-title", "COLUMNS"), div(class = "metric-value", textOutput(ns("card_cols")))),
      div(class = "metric-card", div(class = "metric-title", "MISSING CELLS"), div(class = "metric-value", textOutput(ns("card_missing")))),
      div(class = "metric-card", div(class = "metric-title", "DUPLICATE ROWS"), div(class = "metric-value", textOutput(ns("card_duplicates")))),
      div(class = "metric-card", div(class = "metric-title", "NUMERIC VARS"), div(class = "metric-value", textOutput(ns("card_numeric")))),
      div(class = "metric-card", div(class = "metric-title", "CATEGORICAL VARS"), div(class = "metric-value", textOutput(ns("card_categorical")))),
      div(class = "metric-card", div(class = "metric-title", "OUTLIER FLAGS"), div(class = "metric-value", textOutput(ns("card_outliers")))),
      div(class = "metric-card card-alert", div(class = "metric-title", "INVESTIGATION FLAGS"), div(class = "metric-value", textOutput(ns("card_flags"))))
    ),

    # Quality Indicator & Executive Summary Row
    div(
      class = "row-cards mt-4",
      div(
        class = "col-lg-4 col-md-12",
        div(
          class = "panel-box text-center",
          h4(class = "panel-title", "DATA QUALITY STATUS"),
          uiOutput(ns("quality_badge")),
          div(class = "quality-score-sub", "Data Quality Indicator: Custom composite indicator based on detected issues."),
          div(
            class = "mt-3",
            actionButton(ns("btn_how_calc"), "How is this calculated?", icon = icon("info-circle"), class = "btn btn-sm btn-outline-info")
          )
        )
      ),
      div(
        class = "col-lg-8 col-md-12",
        div(
          class = "panel-box",
          h4(class = "panel-title", icon("chart-line"), " AUTOMATIC EXECUTIVE SUMMARY"),
          div(class = "exec-summary-text", textOutput(ns("exec_summary_text"))),
          div(class = "mt-2 text-muted small", "* Every metric in this summary is dynamically computed from your active dataset.")
        )
      )
    ),

    # Dataset Profile & Preview Tabs
    div(
      class = "panel-box mt-4",
      tabsetPanel(
        id = ns("overview_tabs"),
        tabPanel(
          title = tagList(icon("table"), " Dataset Preview"),
          div(
            class = "p-2",
            div(
              class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
              div(class = "text-muted small", "Showing first records with selectable pagination."),
              radioButtons(
                ns("preview_rows"),
                label = NULL,
                choices = c("10 rows" = 10, "25 rows" = 25, "50 rows" = 50, "100 rows" = 100),
                selected = 25,
                inline = TRUE
              )
            ),
            DT::dataTableOutput(ns("table_preview"))
          )
        ),
        tabPanel(
          title = tagList(icon("list-check"), " Dataset Profile Matrix"),
          div(
            class = "p-2",
            div(class = "text-muted small mb-2", "Comprehensive variable summary: types, missing counts, uniqueness, and central tendencies."),
            DT::dataTableOutput(ns("table_profile"))
          )
        )
      )
    )
  )
}

mod_overview_server <- function(id, data_r, profile_r, quality_r, findings_r, duplicate_r, outlier_r, missing_r) {
  moduleServer(id, function(input, output, session) {

    # Card Outputs
    output$card_rows <- renderText({
      req(profile_r())
      format_number(profile_r()$n_rows)
    })

    output$card_cols <- renderText({
      req(profile_r())
      format_number(profile_r()$n_cols)
    })

    output$card_missing <- renderText({
      req(missing_r())
      paste0(format_number(missing_r()$total_missing), " (", missing_r()$missing_pct, "%)")
    })

    output$card_duplicates <- renderText({
      req(duplicate_r())
      format_number(duplicate_r()$duplicate_rows_count)
    })

    output$card_numeric <- renderText({
      req(profile_r())
      as.character(profile_r()$n_numeric)
    })

    output$card_categorical <- renderText({
      req(profile_r())
      as.character(profile_r()$n_categorical)
    })

    output$card_outliers <- renderText({
      req(outlier_r())
      as.character(outlier_r()$total_outlier_variables)
    })

    output$card_flags <- renderText({
      req(findings_r())
      as.character(findings_r()$total_findings)
    })

    # Quality Badge Output
    output$quality_badge <- renderUI({
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

    # Modal explanation of Quality Score
    observeEvent(input$btn_how_calc, {
      req(quality_r())
      q <- quality_r()

      deductions_ui <- if (length(q$deductions) > 0) {
        lapply(names(q$deductions), function(dname) {
          item <- q$deductions[[dname]]
          div(
            class = "mb-2 p-2 border-bottom",
            div(class = "d-flex justify-content-between", strong(item$name), span(class = "text-danger font-weight-bold", paste0("-", item$points, " pts"))),
            div(class = "small text-muted", item$metric),
            div(class = "small text-secondary", item$description)
          )
        })
      } else {
        p(class = "text-success", "No deductions incurred! Data meets all baseline completeness and uniqueness standards.")
      }

      showModal(modalDialog(
        title = "How is Data Quality Status Calculated?",
        div(
          p("Data Detective applies an explicit, rule-based deduction model starting from a base score of 100."),
          p(class = "alert alert-warning small",
            strong("Important: "),
            "This status is based on the configurable checks used by Data Detective. It is NOT an objective universal quality standard."
          ),
          h5("Score Deductions Breakdown:"),
          deductions_ui,
          hr(),
          p(strong("Status Thresholds:"), " 90–100: Excellent | 75–89: Good | 50–74: Needs Attention | <50: Poor.")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # Executive Summary Text
    output$exec_summary_text <- renderText({
      req(findings_r())
      findings_r()$executive_summary
    })

    # Dataset Preview Table
    output$table_preview <- DT::renderDataTable({
      req(data_r())
      n_show <- as.numeric(input$preview_rows)
      df_sub <- head(data_r(), n_show)

      DT::datatable(
        df_sub,
        options = list(
          pageLength = n_show,
          scrollX = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = TRUE,
        class = "compact stripe hover border-table"
      )
    })

    # Profile Table
    output$table_profile <- DT::renderDataTable({
      req(profile_r())
      p_df <- profile_r()$columns_summary
      req(nrow(p_df) > 0)

      disp_df <- data.frame(
        Column = p_df$column,
        Type = p_df$data_type,
        Missing = paste0(format_number(p_df$missing_count), " (", round(p_df$missing_pct * 100, 1), "%)"),
        Unique = paste0(format_number(p_df$unique_count), " (", round(p_df$unique_pct * 100, 1), "%)"),
        Min = ifelse(is.na(p_df$min), "-", p_df$min),
        Max = ifelse(is.na(p_df$max), "-", p_df$max),
        Mean = ifelse(is.na(p_df$mean), "-", as.character(p_df$mean)),
        Median = ifelse(is.na(p_df$median), "-", as.character(p_df$median)),
        SD = ifelse(is.na(p_df$sd), "-", as.character(p_df$sd)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
