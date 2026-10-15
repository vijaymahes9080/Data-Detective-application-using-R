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
  primary = "#2563eb",
  secondary = "#64748b",
  success = "#10b981",
  warning = "#f59e0b",
  danger = "#ef4444",
  info = "#0284c7"
)

# ---- Application UI ---------------------------------------------------------
ui <- bslib::page_sidebar(
  theme = theme_custom,
  title = NULL,
  window_title = "Data Detective Pro | Intelligent Data Intelligence Platform",

  # ---- Persistent Deep Navy Sidebar ----------------------------------------
  sidebar = bslib::sidebar(
    width = 260,
    open = "desktop",
    title = NULL,
    class = "sidebar",

    div(
      class = "sidebar-inner",

      # Brand Title & Logo
      div(
        class = "d-flex align-items-center gap-2 mb-3 pb-2 border-bottom border-secondary border-opacity-25",
        div(class = "header-brand-icon", icon("magnifying-glass-chart")),
        div(
          div(class = "fw-bold text-white small", "DATA DETECTIVE"),
          div(class = "text-muted", style = "font-size: 0.65rem; letter-spacing: 0.05em;", "DATA INTELLIGENCE PRO")
        )
      ),

      # Ingestion & Sample Dataset Box
      div(
        class = "sidebar-ingestion-box",
        div(class = "sidebar-section-title text-uppercase mb-1", "Upload Dataset"),
        fileInput(
          "user_file",
          label = NULL,
          accept = c(".csv", ".tsv", ".txt", ".rds"),
          buttonLabel = "Browse",
          placeholder = "Upload .csv...",
          width = "100%"
        ),
        div(class = "sidebar-section-title text-uppercase mb-1 mt-2", "Benchmark Samples"),
        div(
          class = "sidebar-sample-btn-group",
          actionButton("btn_clean_sample", "Clean HR Sample", icon = icon("check", class = "text-success"), class = "sidebar-sample-btn"),
          actionButton("btn_messy_sample", "Messy Anomaly", icon = icon("triangle-exclamation", class = "text-warning"), class = "sidebar-sample-btn"),
          actionButton("btn_sales_sample", "Realistic Sales", icon = icon("chart-line", class = "text-info"), class = "sidebar-sample-btn")
        )
      ),

      # Navigation Section: WORKSPACE
      div(class = "sidebar-section-title", "Workspace"),
      div(
        class = "sidebar-nav-list",
        actionButton("nav_overview", tagList(icon("gauge-high", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Overview")), class = "sidebar-nav-btn active", `data-tab-target` = "tab_overview"),
        actionButton("nav_workbench", tagList(icon("wand-magic-sparkles", class = "sidebar-nav-icon text-primary"), span(class = "sidebar-nav-label", "Data Preparation")), class = "sidebar-nav-btn", `data-tab-target` = "tab_workbench"),
        actionButton("nav_column_studio", tagList(icon("columns", class = "sidebar-nav-icon text-info"), span(class = "sidebar-nav-label", "Column Studio")), class = "sidebar-nav-btn", `data-tab-target` = "tab_column_studio")
      ),

      # Navigation Section: DATA QUALITY
      div(class = "sidebar-section-title", "Data Quality & Fixing"),
      div(
        class = "sidebar-nav-list",
        actionButton("nav_quality", tagList(icon("shield-halved", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Quality Hub")), class = "sidebar-nav-btn", `data-tab-target` = "tab_quality"),
        actionButton("nav_missing", tagList(icon("chart-simple", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Missing Values")), class = "sidebar-nav-btn", `data-tab-target` = "tab_missing"),
        actionButton("nav_duplicates", tagList(icon("copy", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Duplicate Records")), class = "sidebar-nav-btn", `data-tab-target` = "tab_duplicates"),
        actionButton("nav_outliers", tagList(icon("arrow-up-right-dots", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Outlier Diagnostics")), class = "sidebar-nav-btn", `data-tab-target` = "tab_outliers")
      ),

      # Navigation Section: ADVANCED ANALYTICS
      div(class = "sidebar-section-title", "Advanced Analytics"),
      div(
        class = "sidebar-nav-list",
        actionButton("nav_distributions", tagList(icon("chart-area", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Distributions")), class = "sidebar-nav-btn", `data-tab-target` = "tab_distributions"),
        actionButton("nav_correlations", tagList(icon("braille", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Correlation Matrix")), class = "sidebar-nav-btn", `data-tab-target` = "tab_correlations"),
        actionButton("nav_relationships", tagList(icon("circle-nodes", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Visual Analytics & 3D")), class = "sidebar-nav-btn", `data-tab-target` = "tab_relationships"),
        actionButton("nav_leakage", tagList(icon("bullseye", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Leakage Screening")), class = "sidebar-nav-btn", `data-tab-target` = "tab_leakage"),
        actionButton("nav_bias", tagList(icon("scale-unbalanced", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Bias & Representation")), class = "sidebar-nav-btn", `data-tab-target` = "tab_bias")
      ),

      # Navigation Section: DELIVERABLES
      div(class = "sidebar-section-title", "Deliverables"),
      div(
        class = "sidebar-nav-list",
        actionButton("nav_report", tagList(icon("file-lines", class = "sidebar-nav-icon"), span(class = "sidebar-nav-label", "Investigation Report")), class = "sidebar-nav-btn", `data-tab-target` = "tab_report")
      ),

      # Sidebar Status & Actions Footer
      div(
        class = "sidebar-footer mt-auto",
        uiOutput("sidebar_dataset_status"),
        div(
          class = "d-flex gap-1 mt-2",
          actionButton("btn_sidebar_undo", "Undo", icon = icon("arrow-rotate-left"), class = "btn btn-sm btn-outline-secondary w-50", style = "font-size: 0.75rem; color: #cbd5e1; border-color: #334155;"),
          downloadButton("btn_sidebar_export", "Export", class = "btn btn-sm btn-outline-success w-50", style = "font-size: 0.75rem;")
        ),
        div(
          class = "d-flex gap-1 mt-1",
          actionButton("btn_reset", "Reset", icon = icon("rotate-left"), class = "btn btn-sm btn-outline-danger w-50", style = "font-size: 0.75rem;"),
          actionButton("btn_settings", "Settings", icon = icon("gear"), class = "btn btn-sm btn-outline-secondary w-50", style = "font-size: 0.75rem; color: #cbd5e1; border-color: #334155;")
        ),
        div(
          class = "sidebar-privacy-badge mt-3",
          icon("shield-halved"),
          span("100% Client/Local Processing")
        )
      )
    )
  ),

  # ---- Main Workspace Panel ------------------------------------------------
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "app.js")
    ),

    div(
      class = "d-flex flex-column min-vh-100",

      # Top Header Bar
      div(
        class = "app-header-bar",
        div(
          class = "header-left",
          div(class = "header-brand-icon d-md-none", icon("magnifying-glass-chart")),
          div(
            class = "header-brand",
            span("DATA DETECTIVE"),
            span(class = "header-brand-tag", "PRO")
          ),
          uiOutput("header_breadcrumb")
        ),
        div(
          class = "header-right",
          uiOutput("header_dataset_pill"),
          actionButton("btn_header_undo", "Undo Fix", icon = icon("arrow-rotate-left"), class = "btn btn-sm btn-outline-secondary d-none d-sm-inline-flex"),
          downloadButton("btn_header_export", "Export CSV", class = "btn btn-sm btn-outline-success d-none d-sm-inline-flex"),
          actionButton("btn_header_settings", "Settings", icon = icon("gear"), class = "btn btn-sm btn-outline-secondary")
        )
      ),

      # Spacious Main Workspace Content
      div(
        class = "app-workspace-content flex-grow-1",
        bslib::navset_hidden(
          id = "main_tabs",
          bslib::nav_panel("tab_overview", mod_overview_ui("mod_overview")),
          bslib::nav_panel("tab_workbench", mod_workbench_ui("mod_workbench")),
          bslib::nav_panel("tab_column_studio", mod_column_studio_ui("mod_column_studio")),
          bslib::nav_panel("tab_quality", mod_quality_ui("mod_quality")),
          bslib::nav_panel("tab_missing", mod_missing_ui("mod_missing")),
          bslib::nav_panel("tab_duplicates", mod_duplicates_ui("mod_duplicates")),
          bslib::nav_panel("tab_outliers", mod_outliers_ui("mod_outliers")),
          bslib::nav_panel("tab_distributions", mod_distributions_ui("mod_distributions")),
          bslib::nav_panel("tab_correlations", mod_correlation_ui("mod_correlation")),
          bslib::nav_panel("tab_relationships", mod_relationships_ui("mod_relationships")),
          bslib::nav_panel("tab_leakage", mod_leakage_ui("mod_leakage")),
          bslib::nav_panel("tab_bias", mod_bias_ui("mod_bias")),
          bslib::nav_panel("tab_report", mod_report_ui("mod_report"))
        )
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

  # Navigation State
  active_nav <- reactiveVal("tab_overview")

  switch_to_tab <- function(tab_id) {
    active_nav(tab_id)
    bslib::nav_select("main_tabs", tab_id)
  }

  observeEvent(input$nav_overview,       { switch_to_tab("tab_overview") })
  observeEvent(input$nav_workbench,      { switch_to_tab("tab_workbench") })
  observeEvent(input$nav_column_studio,  { switch_to_tab("tab_column_studio") })
  observeEvent(input$nav_quality,        { switch_to_tab("tab_quality") })
  observeEvent(input$nav_missing,        { switch_to_tab("tab_missing") })
  observeEvent(input$nav_duplicates,     { switch_to_tab("tab_duplicates") })
  observeEvent(input$nav_outliers,       { switch_to_tab("tab_outliers") })
  observeEvent(input$nav_distributions,  { switch_to_tab("tab_distributions") })
  observeEvent(input$nav_correlations,   { switch_to_tab("tab_correlations") })
  observeEvent(input$nav_relationships,  { switch_to_tab("tab_relationships") })
  observeEvent(input$nav_leakage,        { switch_to_tab("tab_leakage") })
  observeEvent(input$nav_bias,           { switch_to_tab("tab_bias") })
  observeEvent(input$nav_report,         { switch_to_tab("tab_report") })

  observeEvent(input$selected_nav_js, {
    req(input$selected_nav_js)
    switch_to_tab(input$selected_nav_js)
  })

  # Header Dynamic Breadcrumb
  output$header_breadcrumb <- renderUI({
    curr <- active_nav()
    title <- switch(
      curr,
      "tab_overview" = "Overview Dashboard",
      "tab_workbench" = "Data Preparation & Fixes Workbench",
      "tab_column_studio" = "Column Studio",
      "tab_quality" = "Data Quality Hub",
      "tab_missing" = "Missing Values Analysis",
      "tab_duplicates" = "Duplicate Records",
      "tab_outliers" = "Outlier Diagnostics",
      "tab_distributions" = "Distributions",
      "tab_correlations" = "Correlation Matrix",
      "tab_relationships" = "Visual Analytics & 3D Explorer",
      "tab_leakage" = "Data Leakage Screening",
      "tab_bias" = "Bias & Representation",
      "tab_report" = "Investigation Report",
      "Overview"
    )
    div(
      class = "header-breadcrumb ms-2",
      span(class = "d-none d-md-inline text-muted", "Platform"),
      span(class = "breadcrumb-separator d-none d-md-inline", "/"),
      span(class = "breadcrumb-current", title)
    )
  })

  # Header Dataset Status Pill
  output$header_dataset_pill <- renderUI({
    df <- dataset()
    fname <- dataset_name()
    h <- dataset_history()
    v_num <- length(h)

    if (is.null(df)) {
      div(class = "header-dataset-pill", span(class = "dot-indicator bg-warning"), "Awaiting Dataset")
    } else {
      div(
        class = "header-dataset-pill",
        span(class = "dot-indicator"),
        span(class = "fw-bold text-truncate", style = "max-width: 130px;", fname),
        span(class = "text-muted", paste0("(", format_number(nrow(df)), " \u00d7 ", ncol(df), ")")),
        span(class = "badge bg-primary text-white", paste0("v", v_num))
      )
    }
  })

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

  # Undo Action Handlers (Sidebar & Header)
  do_undo <- function() {
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
  }

  observeEvent(input$btn_sidebar_undo, { do_undo() })
  observeEvent(input$btn_header_undo, { do_undo() })

  # Settings Modal Handlers (Sidebar & Header)
  show_settings_modal <- function() {
    showModal(modalDialog(
      title = "Investigation Threshold Settings",
      p(class = "small text-muted", "Configure heuristics used by Data Detective engines. These are configurable heuristics, not universal standards."),
      sliderInput("set_missing", "Missing Values Alert Threshold (%):", min = 1, max = 50, value = 5, step = 1),
      sliderInput("set_outlier", "Outlier IQR Multiplier:", min = 1.0, max = 3.0, value = 1.5, step = 0.25),
      sliderInput("set_corr", "Strong Correlation Threshold (|r|):", min = 0.5, max = 0.95, value = 0.70, step = 0.05),
      easyClose = TRUE,
      footer = modalButton("Done")
    ))
  }

  observeEvent(input$btn_settings, { show_settings_modal() })
  observeEvent(input$btn_header_settings, { show_settings_modal() })

  # Export Handler (Sidebar & Header)
  export_fn <- function(file) {
    df <- dataset()
    req(df)
    utils::write.csv(df, file, row.names = FALSE)
  }

  output$btn_sidebar_export <- downloadHandler(
    filename = function() {
      orig_name <- if (!is.null(dataset_name())) dataset_name() else "dataset"
      paste0(tools::file_path_sans_ext(orig_name), "_cleaned.csv")
    },
    content = export_fn
  )

  output$btn_header_export <- downloadHandler(
    filename = function() {
      orig_name <- if (!is.null(dataset_name())) dataset_name() else "dataset"
      paste0(tools::file_path_sans_ext(orig_name), "_cleaned.csv")
    },
    content = export_fn
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
        class = "sidebar-status-box",
        div(class = "sidebar-status-row", span(class = "sidebar-status-label", "Dataset:"), span(class = "sidebar-status-val text-warning", "None")),
        div(class = "sidebar-status-row", span(class = "sidebar-status-label", "Status:"), span(class = "sidebar-status-val text-secondary", "Awaiting Ingestion"))
      )
    } else {
      div(
        class = "sidebar-status-box",
        div(class = "sidebar-status-row", span(class = "sidebar-status-label", "Dataset:"), span(class = "sidebar-status-val text-truncate d-inline-block", style = "max-width: 120px;", fname)),
        div(class = "sidebar-status-row", span(class = "sidebar-status-label", "Active Rows:"), span(class = "sidebar-status-val tabular-nums", format_number(nrow(df)))),
        div(class = "sidebar-status-row", span(class = "sidebar-status-label", "Columns:"), span(class = "sidebar-status-val tabular-nums", format_number(ncol(df)))),
        div(class = "sidebar-status-row", span(class = "sidebar-status-label", "Version:"), span(class = "badge bg-primary text-white", paste0("v", v_num))),
        div(class = "sidebar-status-row", span(class = "sidebar-status-label", "Status:"), span(class = "text-success fw-bold", "\u2713 Live & Analyzed"))
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
