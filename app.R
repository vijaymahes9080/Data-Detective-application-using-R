# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# Production-Quality R + Shiny Application
# Tagline: "Upload. Investigate. Understand."
# ==============================================================================

# Suppress startup package messages
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
})

# Source all analytical engine scripts in R/
r_scripts <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (script in r_scripts) {
  source(script, local = FALSE)
}

# Source all UI and server modules in modules/
mod_scripts <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
for (mscript in mod_scripts) {
  source(mscript, local = FALSE)
}

# Define modern theme using bslib
app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "zephyr",
  primary = "#2563eb",
  secondary = "#64748b",
  success = "#10b981",
  warning = "#f59e0b",
  danger = "#ef4444",
  info = "#0284c7",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)

# ---- Application UI ---------------------------------------------------------
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$title("Data Detective | Automated Dataset Investigation Platform")
  ),

  # Header with branding and persistent dataset status indicator
  div(
    class = "app-header",
    div(
      h1(class = "brand-title", icon("magnifying-glass-chart"), " DATA DETECTIVE"),
      p(class = "brand-subtitle", "Discover data quality issues, statistical patterns, anomalies, and relationships before they become problems.")
    ),
    div(
      class = "d-flex align-items-center gap-3",
      # Persistent Dataset Status Indicator (Section 5)
      uiOutput("persistent_status_indicator"),
      # Settings Modal Trigger
      actionButton("btn_settings", "Settings", icon = icon("gear"), class = "btn btn-outline-light btn-sm")
    )
  ),

  # Main Application Body
  div(
    class = "container-fluid py-3",
    uiOutput("main_body_ui")
  )
)

# ---- Application Server -----------------------------------------------------
server <- function(input, output, session) {

  # ---- Global Reactive State ------------------------------------------------
  raw_dataset <- reactiveVal(NULL)
  active_dataset <- reactiveVal(NULL)
  dataset_metadata <- reactiveVal(NULL)
  dataset_filename <- reactiveVal(NULL)
  load_notification <- reactiveVal(NULL)

  # Heuristic settings reactive values (Section 35)
  settings <- reactiveValues(
    missing_low = 5,
    missing_moderate = 20,
    missing_critical = 50,
    corr_threshold = 0.70,
    very_corr_threshold = 0.90,
    outlier_iqr = 1.5,
    concentration_threshold = 0.90
  )

  # ---- Sample Datasets Loader -----------------------------------------------
  observeEvent(input$btn_load_clean, {
    file_path <- file.path("sample_data", "sample_clean.csv")
    if (file.exists(file_path)) {
      res <- load_csv_data(file_path)
      if (res$success) {
        raw_dataset(res$raw_data)
        active_dataset(res$data)
        dataset_metadata(res$metadata)
        dataset_filename("sample_clean.csv")
        load_notification("Sample Clean Dataset loaded successfully.")
      }
    }
  })

  observeEvent(input$btn_load_messy, {
    file_path <- file.path("sample_data", "sample_messy.csv")
    if (file.exists(file_path)) {
      res <- load_csv_data(file_path)
      if (res$success) {
        raw_dataset(res$raw_data)
        active_dataset(res$data)
        dataset_metadata(res$metadata)
        dataset_filename("sample_messy.csv")
        load_notification("Sample Messy Dataset (with intentional anomalies) loaded successfully.")
      }
    }
  })

  observeEvent(input$btn_load_sales, {
    file_path <- file.path("sample_data", "sample_sales.csv")
    if (file.exists(file_path)) {
      res <- load_csv_data(file_path)
      if (res$success) {
        raw_dataset(res$raw_data)
        active_dataset(res$data)
        dataset_metadata(res$metadata)
        dataset_filename("sample_sales.csv")
        load_notification("Sample Sales Dataset loaded successfully.")
      }
    }
  })

  # File Upload Handler
  observeEvent(input$file_upload, {
    req(input$file_upload)
    finfo <- input$file_upload
    res <- load_csv_data(finfo$datapath)

    if (res$success) {
      raw_dataset(res$raw_data)
      active_dataset(res$data)
      dataset_metadata(res$metadata)
      dataset_filename(finfo$name)
      load_notification(paste0("Dataset '", finfo$name, "' uploaded and analyzed successfully."))
    } else {
      showNotification(res$error, type = "error", duration = 6)
    }
  })

  # ---- Persistent Dataset Status Indicator (Section 5) -----------------------
  output$persistent_status_indicator <- renderUI({
    df <- active_dataset()
    fname <- dataset_filename()

    if (is.null(df)) {
      div(
        class = "dataset-status-pill",
        div(class = "status-item", span(class = "status-label", "Dataset:"), span(class = "status-value text-warning", "None")),
        div(class = "status-item", span(class = "status-label", "Rows:"), span(class = "status-value", "-")),
        div(class = "status-item", span(class = "status-label", "Columns:"), span(class = "status-value", "-")),
        div(class = "status-item", span(class = "status-label", "Status:"), span(class = "status-value text-secondary", "Awaiting Upload"))
      )
    } else {
      div(
        class = "dataset-status-pill",
        div(class = "status-item", span(class = "status-label", "Dataset:"), span(class = "status-value text-truncate", style = "max-width: 140px;", fname)),
        div(class = "status-item", span(class = "status-label", "Rows:"), span(class = "status-value", format_number(nrow(df)))),
        div(class = "status-item", span(class = "status-label", "Columns:"), span(class = "status-value", format_number(ncol(df)))),
        div(class = "status-item", span(class = "status-label", "Status:"), span(class = "status-value text-success", "\u2713 Analyzed"))
      )
    }
  })

  # ---- Settings Modal (Section 35) ------------------------------------------
  observeEvent(input$btn_settings, {
    showModal(modalDialog(
      title = "Investigation Heuristic Settings",
      div(
        p(class = "text-muted small", "Configure heuristic thresholds used by Data Detective engines. These thresholds are configurable heuristics and do not imply universal standards."),
        hr(),
        h6("Missingness Severity Thresholds (%):"),
        div(class = "row",
            div(class = "col-4", numericInput("set_miss_low", "Low / Warning (%)", value = settings$missing_low, min = 1, max = 20)),
            div(class = "col-4", numericInput("set_miss_mod", "Moderate (%)", value = settings$missing_moderate, min = 10, max = 40)),
            div(class = "col-4", numericInput("set_miss_crit", "Critical (%)", value = settings$missing_critical, min = 40, max = 90))
        ),
        hr(),
        h6("Statistical Anomaly Thresholds:"),
        div(class = "row",
            div(class = "col-4", numericInput("set_corr", "Strong Correlation (|r|)", value = settings$corr_threshold, min = 0.5, max = 0.95, step = 0.05)),
            div(class = "col-4", numericInput("set_iqr", "Outlier IQR Multiplier", value = settings$outlier_iqr, min = 1.0, max = 3.0, step = 0.25)),
            div(class = "col-4", numericInput("set_conc", "Category Concentration (%)", value = settings$concentration_threshold * 100, min = 70, max = 99))
        )
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_save_settings", "Apply Settings", class = "btn btn-primary")
      )
    ))
  })

  observeEvent(input$btn_save_settings, {
    settings$missing_low <- input$set_miss_low
    settings$missing_moderate <- input$set_miss_mod
    settings$missing_critical <- input$set_miss_crit
    settings$corr_threshold <- input$set_corr
    settings$outlier_iqr <- input$set_iqr
    settings$concentration_threshold <- input$set_conc / 100
    removeModal()
    showNotification("Investigation settings updated successfully.", type = "message", duration = 3)
  })

  # ---- Central Analytical Engines (Reactive) --------------------------------
  profile_r <- reactive({
    req(active_dataset())
    profile_dataset(active_dataset())
  })

  missing_r <- reactive({
    req(active_dataset())
    analyze_missing(
      active_dataset(),
      thresholds = list(
        low = settings$missing_low,
        moderate = settings$missing_moderate,
        critical = settings$missing_critical
      )
    )
  })

  duplicate_r <- reactive({
    req(active_dataset())
    analyze_duplicates(active_dataset())
  })

  constant_r <- reactive({
    req(active_dataset())
    analyze_constants(active_dataset(), concentration_threshold = settings$concentration_threshold)
  })

  outlier_r <- reactive({
    req(active_dataset())
    analyze_outliers(active_dataset(), iqr_multiplier = settings$outlier_iqr)
  })

  cat_r <- reactive({
    req(active_dataset())
    analyze_categorical(active_dataset(), concentration_threshold = settings$concentration_threshold)
  })

  corr_r <- reactive({
    req(active_dataset())
    analyze_correlations(
      active_dataset(),
      strong_thresh = settings$corr_threshold,
      very_strong_thresh = settings$very_corr_threshold
    )
  })

  quality_r <- reactive({
    req(profile_r(), duplicate_r(), outlier_r(), constant_r())
    calculate_quality_score(
      profile = profile_r(),
      duplicate_info = duplicate_r(),
      outlier_info = outlier_r(),
      constant_info = constant_r()
    )
  })

  findings_r <- reactive({
    req(profile_r(), missing_r(), duplicate_r(), constant_r(), outlier_r(), corr_r())
    generate_findings(
      profile = profile_r(),
      missing_info = missing_r(),
      duplicate_info = duplicate_r(),
      constant_info = constant_r(),
      outlier_info = outlier_r(),
      correlation_info = corr_r(),
      leakage_info = NULL,
      representation_info = NULL
    )
  })

  # ---- Main Body UI: Welcome Screen vs Investigation Workspace --------------
  output$main_body_ui <- renderUI({
    df <- active_dataset()

    if (is.null(df)) {
      # Section 6: Landing / Upload Experience Welcome Screen
      div(
        class = "welcome-hero",
        div(class = "hero-icon", icon("magnifying-glass-chart")),
        h2(class = "hero-title", "DATA DETECTIVE"),
        p(class = "hero-tagline", "Upload. Investigate. Understand."),
        p(class = "text-muted", "Upload any CSV dataset to systematically audit its data quality, statistical distributions, anomalies, correlations, and potential investigation areas."),

        # Upload Control Box
        div(
          class = "upload-card-box",
          fileInput(
            "file_upload",
            label = tags$strong("Choose CSV Dataset:"),
            accept = c(".csv", "text/csv", "text/plain"),
            width = "100%",
            placeholder = "Select .csv file to inspect..."
          )
        ),

        div(
          class = "mt-4 pt-3 border-top",
          p(class = "small text-muted mb-2", "Or explore immediately with pre-loaded synthetic datasets:"),
          div(
            class = "d-flex justify-content-center gap-2 flex-wrap",
            actionButton("btn_load_clean", "Clean HR Dataset", icon = icon("shield-check"), class = "btn btn-outline-success btn-sm"),
            actionButton("btn_load_messy", "Messy Anomaly Dataset", icon = icon("triangle-exclamation"), class = "btn btn-outline-warning btn-sm"),
            actionButton("btn_load_sales", "Realistic Sales Dataset", icon = icon("chart-line"), class = "btn btn-outline-primary btn-sm")
          )
        )
      )
    } else {
      # Analyzed Dataset Active Workspace
      tagList(
        # Upload Success Notification Banner (Section 6)
        div(
          class = "alert alert-success alert-dismissible fade show d-flex align-items-center justify-content-between mb-3",
          role = "alert",
          div(
            icon("circle-check", class = "me-2"),
            strong(dataset_filename()), " \u2014 ",
            paste0("\u2713 Loaded ", format_number(nrow(df)), " rows | "),
            paste0("\u2713 ", format_number(ncol(df)), " columns | "),
            paste0("\u2713 ", profile_r()$n_numeric, " numeric variables | "),
            paste0("\u2713 ", profile_r()$n_categorical, " categorical variables")
          ),
          div(
            fileInput("file_upload_change", label = NULL, buttonLabel = "Upload Another CSV", accept = c(".csv"), width = "160px"),
            style = "margin-bottom: -15px;"
          )
        ),

        # Section 5 Navigation Tabs
        navlistPanel(
          id = "nav_tabs",
          widths = c(2, 10),
          well = TRUE,

          tabPanel(
            title = tagList(icon("gauge-high"), " Dashboard"),
            value = "tab_dashboard",
            mod_overview_ui("overview_mod")
          ),
          tabPanel(
            title = tagList(icon("shield-halved"), " Data Quality"),
            value = "tab_quality",
            mod_quality_ui("quality_mod")
          ),
          tabPanel(
            title = tagList(icon("chart-simple"), " Outliers"),
            value = "tab_outliers",
            mod_outliers_ui("outliers_mod")
          ),
          tabPanel(
            title = tagList(icon("chart-area"), " Distributions"),
            value = "tab_distributions",
            mod_distributions_ui("distributions_mod")
          ),
          tabPanel(
            title = tagList(icon("tags"), " Categorical"),
            value = "tab_categorical",
            mod_categorical_ui("categorical_mod")
          ),
          tabPanel(
            title = tagList(icon("braille"), " Correlations"),
            value = "tab_correlations",
            mod_correlations_ui("correlations_mod")
          ),
          tabPanel(
            title = tagList(icon("network-wired"), " Pattern Detection"),
            value = "tab_patterns",
            mod_patterns_ui("patterns_mod")
          ),
          tabPanel(
            title = tagList(icon("magnifying-glass-arrow-right"), " Investigation Center"),
            value = "tab_findings",
            mod_findings_ui("findings_mod")
          ),
          tabPanel(
            title = tagList(icon("file-arrow-down"), " Report"),
            value = "tab_report",
            mod_report_ui("report_mod")
          )
        )
      )
    }
  })

  # Handle secondary upload button
  observeEvent(input$file_upload_change, {
    req(input$file_upload_change)
    finfo <- input$file_upload_change
    res <- load_csv_data(finfo$datapath)
    if (res$success) {
      raw_dataset(res$raw_data)
      active_dataset(res$data)
      dataset_metadata(res$metadata)
      dataset_filename(finfo$name)
    }
  })

  # ---- Initialize Modules ---------------------------------------------------
  mod_overview_server(
    id = "overview_mod",
    data_r = active_dataset,
    profile_r = profile_r,
    quality_r = quality_r,
    findings_r = findings_r,
    duplicate_r = duplicate_r,
    outlier_r = outlier_r,
    missing_r = missing_r
  )

  mod_quality_server(
    id = "quality_mod",
    data_r = active_dataset,
    missing_r = missing_r,
    duplicate_r = duplicate_r,
    constant_r = constant_r,
    quality_r = quality_r
  )

  mod_outliers_server(
    id = "outliers_mod",
    data_r = active_dataset,
    outlier_r = outlier_r
  )

  mod_distributions_server(
    id = "distributions_mod",
    data_r = active_dataset,
    profile_r = profile_r
  )

  mod_categorical_server(
    id = "categorical_mod",
    data_r = active_dataset,
    cat_r = cat_r
  )

  mod_correlations_server(
    id = "correlations_mod",
    data_r = active_dataset,
    corr_r = corr_r
  )

  mod_patterns_server(
    id = "patterns_mod",
    data_r = active_dataset,
    profile_r = profile_r
  )

  mod_findings_server(
    id = "findings_mod",
    findings_r = findings_r
  )

  mod_report_server(
    id = "report_mod",
    data_r = active_dataset,
    profile_r = profile_r,
    quality_r = quality_r,
    missing_r = missing_r,
    duplicate_r = duplicate_r,
    outlier_r = outlier_r,
    constant_r = constant_r,
    corr_r = corr_r,
    findings_r = findings_r,
    file_name_r = dataset_filename
  )
}

# ---- Launch Application -----------------------------------------------------
shinyApp(ui = ui, server = server)
