# ==============================================================================
# DATA DETECTIVE PRO: Automated Dataset Investigation & Data Quality Platform
# Full Modular R + Shiny Application
# Shinylive / WebAssembly Compatible | GitHub Pages Ready
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
})

# Source all analytical functions, engines, and modules in R/
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f, local = FALSE)
}

# Theme definition using bslib (WebAssembly / Shinylive compatible, font loaded via CSS)
theme_custom <- bslib::bs_theme(
  version = 5,
  bootswatch = "zephyr",
  primary = "#1e40af",
  secondary = "#64748b",
  success = "#16a34a",
  warning = "#d97706",
  danger = "#dc2626",
  info = "#0284c7"
)

# ---- Application UI ---------------------------------------------------------
ui <- bslib::page_sidebar(
  theme = theme_custom,
  title = div(
    class = "d-flex align-items-center gap-2",
    icon("magnifying-glass-chart", class = "text-primary"),
    span(class = "fw-bold", "DATA DETECTIVE PRO"),
    span(class = "text-muted fs-6 d-none d-md-inline", "| Intelligent Exploration & Interactive Data Quality Workbench")
  ),

  # ---- Sidebar --------------------------------------------------------------
  sidebar = bslib::sidebar(
    width = 300,
    open = "desktop",
    title = "Dataset & Controls",

    # Upload Section
    h6(class = "fw-bold text-uppercase text-muted small mb-2", "Upload Dataset"),
    fileInput(
      "user_file",
      label = NULL,
      accept = c(".csv", ".tsv", ".txt", ".rds"),
      buttonLabel = "Choose Dataset",
      placeholder = "Upload .csv...",
      width = "100%"
    ),

    # Sample Datasets
    div(
      class = "mb-3",
      span(class = "small text-muted d-block mb-1", "Or explore immediate benchmark samples:"),
      div(
        class = "btn-group-vertical w-100 gap-1",
        actionButton("btn_clean_sample", "Clean HR Dataset", icon = icon("check"), class = "btn btn-sm btn-outline-success text-start"),
        actionButton("btn_messy_sample", "Messy Anomaly Dataset", icon = icon("triangle-exclamation"), class = "btn btn-sm btn-outline-warning text-start"),
        actionButton("btn_sales_sample", "Realistic Sales Dataset", icon = icon("chart-line"), class = "btn btn-sm btn-outline-primary text-start")
      )
    ),

    hr(class = "my-2"),

    # Persistent Dataset Status Pill
    uiOutput("sidebar_dataset_status"),

    hr(class = "my-2"),

    # Actions
    h6(class = "fw-bold text-uppercase text-muted small mb-2", "Analysis & Fixing Controls"),
    actionButton("btn_sidebar_undo", "Undo Last Fix", icon = icon("arrow-rotate-left"), class = "btn btn-sm btn-outline-warning w-100 mb-2"),
    downloadButton("btn_sidebar_export", "Export Cleaned CSV", class = "btn btn-sm btn-outline-success w-100 mb-2"),
    actionButton("btn_reset", "Reset to Raw Baseline", icon = icon("rotate-left"), class = "btn btn-sm btn-outline-danger w-100 mb-2"),
    actionButton("btn_settings", "Threshold Settings", icon = icon("gear"), class = "btn btn-sm btn-outline-secondary w-100 mb-2"),

    # Security & Privacy badge
    div(
      class = "mt-4 p-2 bg-light border rounded small text-muted text-center",
      icon("shield-halved", class = "text-success me-1"),
      "100% Client/Local Processing. No dataset leaves your browser."
    )
  ),

  # ---- Main Content Panel ---------------------------------------------------
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "app.js")
    ),

    # Top KPI Summary Cards Row
    uiOutput("top_summary_cards"),

    # Navigation Tabs
    navset_card_tab(
      id = "main_tabs",

      # 1. Overview
      nav_panel(
        title = tagList(icon("gauge-high"), " Overview"),
        mod_overview_ui("mod_overview")
      ),

      # 2. Quality & Fixes Workbench (CORE INTERACTIVE UPGRADE)
      nav_panel(
        title = tagList(icon("wand-magic-sparkles", class = "text-primary"), " Quality & Fixes Workbench"),
        mod_workbench_ui("mod_workbench")
      ),

      # 3. Column Studio (CORE UNIVARIATE DRILLDOWN)
      nav_panel(
        title = tagList(icon("columns", class = "text-info"), " Column Studio"),
        mod_column_studio_ui("mod_column_studio")
      ),

      # 4. Data Quality
      nav_panel(
        title = tagList(icon("shield-halved"), " Data Quality"),
        mod_quality_ui("mod_quality")
      ),

      # 5. Missing Values
      nav_panel(
        title = tagList(icon("magnifying-glass-chart"), " Missing Values"),
        mod_missing_ui("mod_missing")
      ),

      # 6. Duplicates
      nav_panel(
        title = tagList(icon("copy"), " Duplicates"),
        mod_duplicates_ui("mod_duplicates")
      ),

      # 7. Outliers
      nav_panel(
        title = tagList(icon("chart-simple"), " Outliers"),
        mod_outliers_ui("mod_outliers")
      ),

      # 8. Distributions
      nav_panel(
        title = tagList(icon("chart-area"), " Distributions"),
        mod_distributions_ui("mod_distributions")
      ),

      # 9. Correlations
      nav_panel(
        title = tagList(icon("braille"), " Correlations"),
        mod_correlation_ui("mod_correlation")
      ),

      # 10. Relationships
      nav_panel(
        title = tagList(icon("circle-nodes"), " Relationships"),
        mod_relationships_ui("mod_relationships")
      ),

      # 11. Leakage
      nav_panel(
        title = tagList(icon("bullseye"), " Leakage"),
        mod_leakage_ui("mod_leakage")
      ),

      # 12. Bias
      nav_panel(
        title = tagList(icon("scale-unbalanced"), " Bias"),
        mod_bias_ui("mod_bias")
      ),

      # 13. Investigation Report
      nav_panel(
        title = tagList(icon("file-lines"), " Investigation Report"),
        mod_report_ui("mod_report")
      )
    )
  )
)

# ---- Application Server -----------------------------------------------------
server <- function(input, output, session) {

  # Central Reactive Datasets & History Stack
  dataset_raw <- reactiveVal(NULL)
  dataset <- reactiveVal(NULL) # active working dataset
  dataset_history <- reactiveVal(list())
  dataset_name <- reactiveVal("Awaiting Dataset")

  # Load sample datasets
  load_sample <- function(path, name) {
    if (file.exists(path)) {
      delim <- detect_delimiter(path)
      df <- utils::read.csv(
        path,
        sep = delim,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA", "N/A", "null", "NULL", "NaN")
      )
      # Clean and repair headers
      cn <- names(df)
      empty_idx <- which(is.na(cn) | trimws(cn) == "")
      if (length(empty_idx) > 0) cn[empty_idx] <- paste0("V_", empty_idx)
      if (any(duplicated(cn))) cn <- make.unique(cn, sep = "_")
      names(df) <- cn

      dataset_raw(df)
      dataset(df)
      dataset_name(name)

      init_entry <- list(
        id = 1,
        timestamp = format(Sys.time(), "%H:%M:%S"),
        action = paste0("Ingestion: ", name),
        affected_rows = 0,
        affected_cells = 0,
        data = df
      )
      dataset_history(list(init_entry))
    }
  }

  observeEvent(input$btn_clean_sample, {
    load_sample("sample_data/sample_clean.csv", "sample_clean.csv")
  })

  observeEvent(input$btn_messy_sample, {
    load_sample("sample_data/sample_messy.csv", "sample_messy.csv")
  })

  observeEvent(input$btn_sales_sample, {
    load_sample("sample_data/sample_sales.csv", "sample_sales.csv")
  })

  # File upload handler
  observeEvent(input$user_file, {
    req(input$user_file)
    f <- input$user_file
    ext <- tolower(tools::file_ext(f$name))

    tryCatch({
      df <- if (ext == "rds") {
        readRDS(f$datapath)
      } else {
        delim <- detect_delimiter(f$datapath)
        utils::read.csv(
          f$datapath,
          sep = delim,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          na.strings = c("", "NA", "N/A", "null", "NULL", "NaN")
        )
      }

      val <- validate_dataset(df)
      if (val$valid) {
        cn <- names(df)
        empty_idx <- which(is.na(cn) | trimws(cn) == "")
        if (length(empty_idx) > 0) cn[empty_idx] <- paste0("V_", empty_idx)
        if (any(duplicated(cn))) cn <- make.unique(cn, sep = "_")
        names(df) <- cn

        dataset_raw(df)
        dataset(df)
        dataset_name(f$name)

        init_entry <- list(
          id = 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Uploaded: ", f$name),
          affected_rows = 0,
          affected_cells = 0,
          data = df
        )
        dataset_history(list(init_entry))
      } else {
        showNotification(paste(val$errors, collapse = " | "), type = "error", duration = 6)
      }
    }, error = function(e) {
      showNotification(paste0("Parse failure: ", e$message), type = "error", duration = 6)
    })
  })

  # Sidebar Undo Handler
  observeEvent(input$btn_sidebar_undo, {
    h <- dataset_history()
    if (length(h) > 1) {
      h_new <- h[seq_len(length(h) - 1)]
      dataset_history(h_new)
      prev_entry <- h_new[[length(h_new)]]
      dataset(prev_entry$data)
      showNotification(paste0("\u21ba Undid last action. Restored Version ", prev_entry$id, "."), type = "message")
    } else {
      showNotification("Already at baseline raw version. Nothing to undo.", type = "warning")
    }
  })

  # Sidebar Export Handler
  output$btn_sidebar_export <- downloadHandler(
    filename = function() {
      orig_name <- if (!is.null(dataset_name())) dataset_name() else "dataset"
      paste0(tools::file_path_sans_ext(orig_name), "_cleaned.csv")
    },
    content = function(file) {
      df <- dataset()
      req(df)
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  # Reset button (reverts working dataset to raw baseline)
  observeEvent(input$btn_reset, {
    if (!is.null(dataset_raw())) {
      df_raw <- dataset_raw()
      init_entry <- list(
        id = 1,
        timestamp = format(Sys.time(), "%H:%M:%S"),
        action = "Baseline Reset to Original Ingestion",
        affected_rows = 0,
        affected_cells = 0,
        data = df_raw
      )
      dataset_history(list(init_entry))
      dataset(df_raw)
      showNotification("\u21ba Reverted to original raw dataset. History stack cleared.", type = "message", duration = 3)
    } else {
      dataset(NULL)
      dataset_name("Awaiting Dataset")
      dataset_history(list())
      showNotification("Analysis reset. Upload a dataset or choose a sample.", type = "message", duration = 3)
    }
  })

  # Default initial dataset: clean sample so the app is immediately alive on load
  observe({
    if (is.null(dataset())) {
      load_sample("sample_data/sample_clean.csv", "sample_clean.csv")
    }
  })

  # ---- Persistent Dataset Status Indicator (Sidebar) ------------------------
  output$sidebar_dataset_status <- renderUI({
    df <- dataset()
    fname <- dataset_name()
    h <- dataset_history()
    v_num <- length(h)

    if (is.null(df)) {
      div(
        class = "p-2 bg-light border rounded small",
        div(strong("Dataset: "), span(class = "text-warning", "None")),
        div(strong("Status: "), span(class = "text-secondary", "Awaiting Upload"))
      )
    } else {
      div(
        class = "p-2 bg-light border rounded small",
        div(strong("Dataset: "), span(class = "text-primary text-truncate d-inline-block", style = "max-width: 170px;", fname)),
        div(strong("Active Rows: "), format_number(nrow(df))),
        div(strong("Columns: "), format_number(ncol(df))),
        div(strong("Version: "), span(class = "badge bg-info text-white", paste0("v", v_num))),
        div(strong("Status: "), span(class = "text-success fw-bold", "\u2713 Live & Analyzed"))
      )
    }
  })

  # ---- Analytical Reactive Engines ------------------------------------------
  profile_res <- reactive({
    req(dataset())
    inspect_dataset(dataset())
  })

  missing_res <- reactive({
    req(dataset())
    detect_missing(dataset())
  })

  duplicate_res <- reactive({
    req(dataset())
    detect_duplicates(dataset())
  })

  constant_df <- reactive({
    req(dataset())
    detect_constant_columns(dataset())
  })

  outlier_res <- reactive({
    req(dataset())
    detect_outliers(dataset())
  })

  corr_res <- reactive({
    req(dataset())
    calculate_correlations(dataset())
  })

  leakage_res <- reactive({
    req(dataset())
    detect_leakage_indicators(dataset())
  })

  bias_res <- reactive({
    req(dataset())
    detect_bias_indicators(dataset())
  })

  quality_res <- reactive({
    req(profile_res(), missing_res(), duplicate_res(), outlier_res())
    calculate_quality_indicator(profile_res(), missing_res(), duplicate_res(), outlier_res(), constant_df())
  })

  findings_res <- reactive({
    req(profile_res(), missing_res(), duplicate_res(), outlier_res(), corr_res())
    tryCatch(
      generate_findings(
        profile     = profile_res(),
        missing_res = missing_res(),
        dup_res     = duplicate_res(),
        out_res     = outlier_res(),
        const_df    = constant_df(),
        corr_res    = corr_res(),
        leak_res    = leakage_res(),
        bias_res    = bias_res(),
        df          = dataset()
      ),
      error = function(e) {
        message("generate_findings error: ", e$message)
        data.frame(
          issue_id = character(0),
          category = character(0), severity = character(0),
          column = character(0), message = character(0),
          evidence = character(0), recommendation = character(0),
          fix_category = character(0), supported_fixes = character(0),
          recommended_fix = character(0), affected_count = integer(0),
          affected_pct = numeric(0),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  # ---- Top Dashboard KPI Cards Row ------------------------------------------
  output$top_summary_cards <- renderUI({
    req(profile_res(), missing_res(), duplicate_res())
    p <- profile_res()
    m <- missing_res()
    d <- duplicate_res()
    f <- tryCatch(findings_res(), error = function(e) NULL)
    n_issues <- if (is.null(f) || !is.data.frame(f)) 0L else nrow(f)

    div(
      class = "metrics-grid mb-3",
      div(class = "metric-card", div(class = "metric-title", "ROWS"), div(class = "metric-value", format_number(p$n_rows))),
      div(class = "metric-card", div(class = "metric-title", "COLUMNS"), div(class = "metric-value", format_number(p$n_cols))),
      div(class = "metric-card", div(class = "metric-title", "MISSING CELLS"), div(class = "metric-value", paste0(format_number(m$total_missing), " (", m$missing_pct, "%)"))),
      div(class = "metric-card", div(class = "metric-title", "DUPLICATE ROWS"), div(class = "metric-value", format_number(d$duplicate_rows_count))),
      div(class = "metric-card", div(class = "metric-title", "NUMERIC VARS"), div(class = "metric-value", p$n_numeric)),
      div(class = "metric-card", div(class = "metric-title", "CATEGORICAL VARS"), div(class = "metric-value", p$n_categorical)),
      div(class = "metric-card card-alert", div(class = "metric-title", "QUALITY FINDINGS"), div(class = "metric-value", n_issues))
    )
  })

  # Settings Modal
  observeEvent(input$btn_settings, {
    showModal(modalDialog(
      title = "Investigation Threshold Settings",
      p(class = "small text-muted", "Configure heuristics used by Data Detective engines. These are configurable heuristics, not universal standards."),
      sliderInput("set_missing", "Missing Values Alert Threshold (%):", min = 1, max = 50, value = 5, step = 1),
      sliderInput("set_outlier", "Outlier IQR Multiplier:", min = 1.0, max = 3.0, value = 1.5, step = 0.25),
      sliderInput("set_corr", "Strong Correlation Threshold (|r|):", min = 0.5, max = 0.95, value = 0.70, step = 0.05),
      easyClose = TRUE,
      footer = modalButton("Done")
    ))
  })

  # ---- Submodule Servers ----------------------------------------------------
  mod_overview_server("mod_overview", data_r = dataset, profile_r = profile_res, issues_count_r = reactive({ f <- findings_res(); if (is.null(f) || !is.data.frame(f)) 0L else nrow(f) }))
  mod_workbench_server("mod_workbench", data_working = dataset, data_raw = dataset_raw, history_stack = dataset_history, findings_r = findings_res, quality_r = quality_res, dataset_name_r = dataset_name)
  mod_column_studio_server("mod_column_studio", data_r = dataset)
  mod_quality_server("mod_quality", quality_r = quality_res, findings_r = findings_res)
  mod_missing_server("mod_missing", missing_r = missing_res)
  mod_duplicates_server("mod_duplicates", duplicate_r = duplicate_res)
  mod_outliers_server("mod_outliers", data_r = dataset, outlier_r = outlier_res)
  mod_distributions_server("mod_distributions", data_r = dataset)
  mod_correlation_server("mod_correlation", data_r = dataset)
  mod_relationships_server("mod_relationships", data_r = dataset)
  mod_leakage_server("mod_leakage", data_r = dataset)
  mod_bias_server("mod_bias", data_r = dataset)
  mod_report_server("mod_report", profile_r = profile_res, quality_r = quality_res, findings_r = findings_res, filename_r = dataset_name)
}

# Run Application
shinyApp(ui = ui, server = server)
