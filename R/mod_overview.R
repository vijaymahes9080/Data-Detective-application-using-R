# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_overview.R
# Description: Module 2 — Dataset Overview, Dimension Cards, Type Table & Preview
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Top KPI Metric Cards Grid
    div(
      class = "metrics-grid mb-3",
      div(class = "metric-card", div(class = "metric-title", "ROWS"), div(class = "metric-value", textOutput(ns("kpi_rows")))),
      div(class = "metric-card", div(class = "metric-title", "COLUMNS"), div(class = "metric-value", textOutput(ns("kpi_cols")))),
      div(class = "metric-card", div(class = "metric-title", "MEMORY FOOTPRINT"), div(class = "metric-value", textOutput(ns("kpi_mem")))),
      div(class = "metric-card", div(class = "metric-title", "NUMERIC VARS"), div(class = "metric-value", textOutput(ns("kpi_num")))),
      div(class = "metric-card", div(class = "metric-title", "CATEGORICAL VARS"), div(class = "metric-value", textOutput(ns("kpi_cat")))),
      div(class = "metric-card", div(class = "metric-title", "DATE / TIME"), div(class = "metric-value", textOutput(ns("kpi_date")))),
      div(class = "metric-card", div(class = "metric-title", "CONSTANT VARS"), div(class = "metric-value", textOutput(ns("kpi_const")))),
      div(class = "metric-card card-alert", div(class = "metric-title", "INVESTIGATION ISSUES"), div(class = "metric-value", textOutput(ns("kpi_issues"))))
    ),

    # Data Types & Characteristics Table
    div(
      class = "panel-box mb-3",
      h5(class = "panel-title", icon("list-check"), " Variable Types & Column Profiling"),
      DT::dataTableOutput(ns("table_types"))
    ),

    # Interactive Data Preview
    div(
      class = "panel-box",
      div(
        class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
        h5(class = "panel-title mb-0", icon("table"), " Interactive Data Preview"),
        div(
          class = "d-flex gap-2 align-items-center",
          radioButtons(
            ns("preview_mode"),
            label = NULL,
            choices = c("First N Rows" = "head", "Last N Rows" = "tail"),
            selected = "head",
            inline = TRUE
          ),
          selectInput(
            ns("preview_n"),
            label = NULL,
            choices = c("10" = 10, "25" = 25, "50" = 50, "100" = 100),
            selected = "25",
            width = "80px"
          )
        )
      ),
      div(
        class = "mb-2",
        selectizeInput(
          ns("selected_cols"),
          label = "Filter Displayed Columns (Optional):",
          choices = NULL,
          multiple = TRUE,
          width = "100%",
          options = list(placeholder = "All columns displayed by default...")
        )
      ),
      DT::dataTableOutput(ns("table_preview"))
    )
  )
}

mod_overview_server <- function(id, data_r, profile_r, issues_count_r = NULL) {
  moduleServer(id, function(input, output, session) {

    # KPI Outputs
    output$kpi_rows <- renderText({
      req(profile_r())
      format_number(profile_r()$n_rows)
    })

    output$kpi_cols <- renderText({
      req(profile_r())
      format_number(profile_r()$n_cols)
    })

    output$kpi_mem <- renderText({
      req(profile_r())
      profile_r()$memory_formatted
    })

    output$kpi_num <- renderText({
      req(profile_r())
      as.character(profile_r()$n_numeric)
    })

    output$kpi_cat <- renderText({
      req(profile_r())
      as.character(profile_r()$n_categorical)
    })

    output$kpi_date <- renderText({
      req(profile_r())
      as.character(profile_r()$n_date)
    })

    output$kpi_const <- renderText({
      req(profile_r())
      p <- profile_r()$columns_summary
      as.character(sum(p$is_constant, na.rm = TRUE))
    })

    output$kpi_issues <- renderText({
      if (!is.null(issues_count_r) && !is.null(issues_count_r())) {
        as.character(issues_count_r())
      } else {
        "0"
      }
    })

    # Update Column Filter choices
    observe({
      req(data_r())
      updateSelectizeInput(session, "selected_cols", choices = names(data_r()), selected = character(0))
    })

    # Types Table
    output$table_types <- DT::renderDataTable({
      req(profile_r())
      p <- profile_r()$columns_summary
      req(nrow(p) > 0)

      disp_df <- data.frame(
        Column = p$column,
        Data_Type = p$data_type,
        Unique_Values = paste0(format_number(p$unique_count), " (", p$unique_pct, "%)"),
        Missing_Values = paste0(format_number(p$missing_count), " (", p$missing_pct, "%)"),
        Example_Value = p$example,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 10, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Data Preview Table
    output$table_preview <- DT::renderDataTable({
      req(data_r())
      df <- data_r()
      n <- as.numeric(input$preview_n)
      mode <- input$preview_mode

      # Filter columns if selected
      cols <- input$selected_cols
      if (!is.null(cols) && length(cols) > 0) {
        valid_cols <- intersect(cols, names(df))
        if (length(valid_cols) > 0) df <- df[, valid_cols, drop = FALSE]
      }

      preview_data <- if (mode == "tail") utils::tail(df, n) else utils::head(df, n)

      DT::datatable(
        preview_data,
        options = list(
          pageLength = n,
          scrollX = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = TRUE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
