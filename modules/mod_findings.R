# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_findings.R
# Description: Central Investigation Center, finding filters, cards, and severity rules
# ==============================================================================

mod_findings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "p-3",
      # Investigation Center Header & Summary Counters
      div(
        class = "panel-box mb-3",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap mb-3",
          div(
            h4(class = "panel-title mb-0", icon("magnifying-glass-arrow-right"), " DATASET INVESTIGATION CENTER"),
            div(class = "text-muted small", "Rule-based statistical anomaly findings, evidence, and actionable next steps.")
          ),
          actionButton(ns("btn_severity_rules"), "View Severity Rules", icon = icon("book-bookmark"), class = "btn btn-outline-secondary btn-sm")
        ),
        div(
          class = "row text-center",
          div(class = "col-md-3 col-6 mb-2", div(class = "p-2 rounded bg-light border", div(class = "text-muted small font-weight-bold", "TOTAL FINDINGS"), h3(class = "mb-0", textOutput(ns("count_total"))))),
          div(class = "col-md-3 col-6 mb-2", div(class = "p-2 rounded bg-danger-subtle border border-danger", div(class = "text-danger small font-weight-bold", "HIGH SEVERITY"), h3(class = "text-danger mb-0", textOutput(ns("count_high"))))),
          div(class = "col-md-3 col-6 mb-2", div(class = "p-2 rounded bg-warning-subtle border border-warning", div(class = "text-warning-emphasis small font-weight-bold", "WARNINGS"), h3(class = "text-warning mb-0", textOutput(ns("count_warning"))))),
          div(class = "col-md-3 col-6 mb-2", div(class = "p-2 rounded bg-info-subtle border border-info", div(class = "text-info small font-weight-bold", "INFORMATIONAL"), h3(class = "text-info mb-0", textOutput(ns("count_info")))))
        )
      ),

      # Finding Filters Row
      div(
        class = "panel-box mb-3",
        div(
          class = "row align-items-center",
          div(
            class = "col-md-4 mb-2 mb-md-0",
            selectInput(
              ns("filter_severity"),
              "Filter by Severity:",
              choices = c("All Severities" = "ALL", "High" = "HIGH", "Warning" = "WARNING", "Info" = "INFO"),
              selected = "ALL",
              width = "100%"
            )
          ),
          div(
            class = "col-md-4 mb-2 mb-md-0",
            selectInput(
              ns("filter_category"),
              "Filter by Category:",
              choices = c("All Categories" = "ALL"),
              selected = "ALL",
              width = "100%"
            )
          ),
          div(
            class = "col-md-4",
            textInput(ns("search_text"), "Search Findings Keyword:", placeholder = "Search column, text, ID...", width = "100%")
          )
        )
      ),

      # Findings Dynamic Cards Container
      div(
        class = "panel-box",
        div(class = "d-flex justify-content-between align-items-center mb-2",
            h5(class = "panel-title mb-0", "Investigation Findings Dossier"),
            span(class = "text-muted small", textOutput(ns("showing_count_label")))),
        uiOutput(ns("findings_cards_container"))
      )
    )
  )
}

mod_findings_server <- function(id, findings_r) {
  moduleServer(id, function(input, output, session) {

    # Update category choices based on actual detected findings
    observe({
      req(findings_r())
      f_df <- findings_r()$findings
      if (nrow(f_df) > 0) {
        cats <- unique(f_df$category)
        choices <- c("All Categories" = "ALL", setNames(cats, cats))
        current <- input$filter_category
        selected <- if (!is.null(current) && current %in% choices) current else "ALL"
        updateSelectInput(session, "filter_category", choices = choices, selected = selected)
      }
    })

    # Summary Counters
    output$count_total <- renderText({
      req(findings_r())
      as.character(findings_r()$total_findings)
    })
    output$count_high <- renderText({
      req(findings_r())
      as.character(findings_r()$n_high)
    })
    output$count_warning <- renderText({
      req(findings_r())
      as.character(findings_r()$n_warning)
    })
    output$count_info <- renderText({
      req(findings_r())
      as.character(findings_r()$n_info)
    })

    # Severity Rules Modal
    observeEvent(input$btn_severity_rules, {
      showModal(modalDialog(
        title = "Data Detective Severity Classification Rules",
        div(
          p("Severity ratings are generated through explicit, transparent heuristic rules:"),
          tags$table(
            class = "table table-sm table-bordered",
            tags$thead(
              tags$tr(tags$th("Category"), tags$th("INFO"), tags$th("WARNING"), tags$th("HIGH"))
            ),
            tags$tbody(
              tags$tr(
                tags$td(strong("Missing Values")),
                tags$td("\u2264 5% missing"),
                tags$td("> 5% and \u2264 20%"),
                tags$td("> 20% (High) or > 50% (Critical)")
              ),
              tags$tr(
                tags$td(strong("Duplicate Rows")),
                tags$td("< 1% duplicates"),
                tags$td("1% \u2013 5% duplicates"),
                tags$td("\u2265 5% duplicate records")
              ),
              tags$tr(
                tags$td(strong("Outliers (IQR)")),
                tags$td("< 5% outliers"),
                tags$td("\u2265 5% outliers"),
                tags$td("\u2265 15% extreme cluster")
              ),
              tags$tr(
                tags$td(strong("Uninformative")),
                tags$td("Near-zero variance"),
                tags$td("Near-constant (\u226590%)"),
                tags$td("Strictly constant or 100% NA")
              ),
              tags$tr(
                tags$td(strong("Correlation")),
                tags$td("|r| 0.40 \u2013 0.69"),
                tags$td("|r| 0.70 \u2013 0.89"),
                tags$td("|r| \u2265 0.90 (Multicollinear)")
              ),
              tags$tr(
                tags$td(strong("Leakage")),
                tags$td("Subtle name cue"),
                tags$td("Target in name"),
                tags$td("Near-identity or |r| \u2265 0.95")
              )
            )
          ),
          p(class = "small text-muted mb-0", "* Recommendations represent statistical investigative guidance and never alter source data.")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # Filtered findings reactive
    filtered_findings <- reactive({
      req(findings_r())
      f_df <- findings_r()$findings
      if (nrow(f_df) == 0) return(f_df)

      # Severity filter
      if (!is.null(input$filter_severity) && input$filter_severity != "ALL") {
        f_df <- f_df[f_df$severity == input$filter_severity, ]
      }

      # Category filter
      if (!is.null(input$filter_category) && input$filter_category != "ALL") {
        f_df <- f_df[f_df$category == input$filter_category, ]
      }

      # Search filter
      if (!is.null(input$search_text) && nzchar(trimws(input$search_text))) {
        pattern <- tolower(trimws(input$search_text))
        match_idx <- grepl(pattern, tolower(f_df$column)) |
          grepl(pattern, tolower(f_df$description)) |
          grepl(pattern, tolower(f_df$evidence)) |
          grepl(pattern, tolower(f_df$id)) |
          grepl(pattern, tolower(f_df$category))
        f_df <- f_df[match_idx, ]
      }

      f_df
    })

    output$showing_count_label <- renderText({
      f_df <- filtered_findings()
      paste0("Displaying ", nrow(f_df), " finding(s)")
    })

    # Render finding cards
    output$findings_cards_container <- renderUI({
      f_df <- filtered_findings()

      if (nrow(f_df) == 0) {
        return(div(
          class = "alert alert-light text-center p-4",
          icon("clipboard-check", class = "fs-2 text-success mb-2"),
          h5("No findings matching your selected criteria."),
          p(class = "small text-muted mb-0", "Adjust your severity, category, or search filters above to inspect other findings.")
        ))
      }

      cards <- lapply(seq_len(nrow(f_df)), function(i) {
        row <- f_df[i, ]
        badge_cls <- switch(row$severity, "HIGH" = "bg-danger", "WARNING" = "bg-warning text-dark", "INFO" = "bg-info text-white", "bg-secondary")
        card_border_cls <- switch(row$severity, "HIGH" = "border-danger-left", "WARNING" = "border-warning-left", "INFO" = "border-info-left", "")

        div(
          class = paste0("finding-card-item mb-3 p-3 rounded shadow-sm border ", card_border_cls),
          div(
            class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
            div(
              span(class = paste0("badge me-2 ", badge_cls), row$severity),
              strong(class = "me-2", row$category),
              span(class = "text-muted font-monospace small", paste0("[", row$id, "]")),
              span(class = "badge bg-light text-dark border ms-2", paste0("Column: ", row$column))
            )
          ),
          div(
            class = "small mt-2",
            div(class = "mb-1", strong("What? "), row$description),
            div(class = "mb-1 text-secondary", strong("Evidence: "), row$evidence),
            div(class = "mb-1 text-muted", strong("Why investigate? "), row$why_investigate),
            div(class = "p-2 mt-2 bg-light rounded", strong(class = "text-primary", icon("arrow-right"), " Recommended next step: "), em(row$recommendation))
          )
        )
      })

      tagList(cards)
    })
  })
}
