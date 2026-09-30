# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_bias.R
# Description: Module 11 — Potential Representation & Imbalance Diagnostics
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_bias_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "panel-box mb-3",
      div(
        class = "row align-items-center",
        div(class = "col-md-5", selectInput(ns("group_var"), "Select Demographic / Cohort Grouping Variable:", choices = NULL, width = "100%")),
        div(class = "col-md-5", selectInput(ns("outcome_var"), "Select Outcome / Performance Variable:", choices = NULL, width = "100%")),
        div(class = "col-md-2", div(class = "small text-muted mt-3", "Descriptive representation screening."))
      )
    ),

    # Ethical Disclaimer & Table
    div(
      class = "panel-box mb-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
        h5(class = "panel-title mb-0", icon("scale-unbalanced"), " Representation Disparity Indicators"),
        span(class = "badge bg-info text-white", "Descriptive Statistics Only")
      ),
      p(class = "small text-muted mb-3",
        strong("Important Ethical Principle: "),
        "Statistical differences across sub-populations require domain context. ",
        "Observed category imbalances or differential missingness rates are descriptive indicators, not automatic proof of discrimination or bias."
      ),
      DT::dataTableOutput(ns("table_bias_indicators"))
    ),

    # Stratified Cohort Summary Table
    div(
      class = "panel-box",
      h5(class = "panel-title", icon("users"), " Stratified Cohort Group Profile"),
      DT::dataTableOutput(ns("table_stratified"))
    )
  )
}

mod_bias_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(data_r())
      cat_cols <- names(data_r())[vapply(data_r(), function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      num_cols <- names(data_r())[vapply(data_r(), function(x) is.numeric(x) && !is.logical(x), logical(1))]

      if (length(cat_cols) > 0) updateSelectInput(session, "group_var", choices = cat_cols, selected = cat_cols[1])
      if (length(num_cols) > 0) updateSelectInput(session, "outcome_var", choices = num_cols, selected = num_cols[1])
    })

    bias_res <- reactive({
      req(data_r())
      detect_bias_indicators(data_r())
    })

    output$table_bias_indicators <- DT::renderDataTable({
      req(bias_res())
      b <- bias_res()

      if (!b$has_indicators || nrow(b$indicators) == 0) {
        return(DT::datatable(
          data.frame(Status = "No severe category imbalance (\u2265 90%) detected in categorical variables."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Variable = b$indicators$column,
        Indicator_Type = b$indicators$indicator_type,
        Severity = b$indicators$severity,
        Evidence = b$indicators$evidence,
        Investigation_Note = b$indicators$reason,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 6, dom = 'tp'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    output$table_stratified <- DT::renderDataTable({
      req(input$group_var, input$outcome_var, data_r())
      df <- data_r()
      g_col <- input$group_var
      o_col <- input$outcome_var
      req(g_col %in% names(df), o_col %in% names(df))

      g_vals <- as.character(df[[g_col]])
      o_vals <- df[[o_col]]

      valid <- !is.na(g_vals) & trimws(g_vals) != ""
      if (sum(valid) == 0) return(DT::datatable(data.frame(Message = "No valid group data.")))

      g_sub <- g_vals[valid]
      o_sub <- o_vals[valid]

      grps <- unique(g_sub)
      rows <- lapply(grps, function(grp) {
        idx <- which(g_sub == grp)
        vals_g <- o_sub[idx]
        data.frame(
          Cohort_Group = grp,
          Total_Records = length(vals_g),
          Missing_Outcome = sum(is.na(vals_g)),
          Outcome_Mean = round(safe_mean(vals_g), 2),
          Outcome_Median = round(safe_median(vals_g), 2),
          stringsAsFactors = FALSE
        )
      })

      strat_df <- do.call(rbind, rows)
      strat_df <- strat_df[order(-strat_df$Total_Records), ]
      strat_df$Group_Share <- paste0(round((strat_df$Total_Records / sum(strat_df$Total_Records)) * 100, 1), "%")

      DT::datatable(
        strat_df,
        options = list(pageLength = 8, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
