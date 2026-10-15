# ==============================================================================
# DATA DETECTIVE PRO: Intelligent Data Quality & Feature Enrichment Workbench
# File: R/mod_workbench.R
# Description: End-to-end raw-to-useful transformation, smart recipes, NLP queries,
#              feature engineering, and reproducible R pipeline exports
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_workbench_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Top Control & Multi-Dimensional Score Banner
    div(
      class = "workbench-header-bar p-3 mb-3 bg-white border rounded shadow-sm",
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        div(
          class = "d-flex align-items-center gap-3",
          div(
            class = "quality-score-badge d-flex align-items-center gap-2",
            uiOutput(ns("score_comparison_badge"))
          ),
          div(
            h5(class = "mb-0 fw-bold text-dark", icon("wand-magic-sparkles", class = "text-primary me-2"), "RAW-TO-USEFUL DATA INTELLIGENCE WORKBENCH"),
            span(class = "small text-muted", "Autonomous recipe synthesis, feature engineering, and natural language dataset exploration.")
          )
        ),
        div(
          class = "d-flex gap-2 flex-wrap align-items-center",
          actionButton(ns("btn_smart_recipe_top"), "1-Click Smart Recipe", icon = icon("bolt-lightning"), class = "btn btn-sm btn-primary"),
          actionButton(ns("btn_undo_top"), "Undo Last Action", icon = icon("arrow-rotate-left"), class = "btn btn-sm btn-outline-warning"),
          actionButton(ns("btn_revert_top"), "Reset to Baseline", icon = icon("trash-can"), class = "btn btn-sm btn-outline-danger"),
          downloadButton(ns("btn_export_r_script"), "Export R Script", class = "btn btn-sm btn-outline-dark"),
          downloadButton(ns("btn_export_clean"), "Export Cleaned CSV", class = "btn btn-sm btn-success")
        )
      ),
      hr(class = "my-2"),
      # KPI Stats Row
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2 pt-1",
        div(
          class = "d-flex gap-2 flex-wrap",
          uiOutput(ns("status_pills"))
        ),
        div(
          class = "small text-muted",
          uiOutput(ns("dataset_version_status"))
        )
      )
    ),

    # Main Workbench Navigation
    tabsetPanel(
      id = ns("workbench_tabs"),
      type = "tabs",

      # ---- TAB 1: Autonomous One-Click Smart Recipe (HERO INNOVATION) --------
      tabPanel(
        title = tagList(icon("bolt-lightning", class = "text-warning"), " 1-Click Smart Recipe"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "row align-items-center mb-3",
            div(
              class = "col-lg-8 col-md-12",
              h6(class = "fw-bold mb-1 text-primary", icon("robot"), " Autonomous Raw-to-Useful Pipeline Synthesizer"),
              p(class = "small text-muted mb-0",
                "Data Detective analyzes dataset defects, infers data types, detects outliers, and synthesizes a complete remediation and feature-enrichment recipe tailored to your active data."
              )
            ),
            div(
              class = "col-lg-4 col-md-12 text-lg-end mt-2 mt-lg-0",
              actionButton(ns("btn_execute_recipe"), "Apply Complete Smart Recipe", icon = icon("wand-magic-sparkles"), class = "btn btn-primary fw-bold shadow-sm")
            )
          ),
          div(
            class = "row",
            div(
              class = "col-lg-7 col-md-12",
              h6(class = "fw-bold small text-muted text-uppercase mb-2", "Synthesized Transformation Recipe Steps"),
              uiOutput(ns("recipe_steps_list"))
            ),
            div(
              class = "col-lg-5 col-md-12",
              div(
                class = "p-3 border rounded bg-light mb-3",
                h6(class = "fw-bold mb-2", icon("chart-pie"), " Multi-Dimensional Data Health Radar"),
                uiOutput(ns("usefulness_radar_summary")),
                hr(class = "my-2"),
                div(
                  class = "small text-muted",
                  strong("What makes data 'Useful'?"),
                  tags$ul(
                    class = "mb-0 ps-3 mt-1",
                    tags$li("Zero artificial duplicate inflation"),
                    tags$li("Statistically robust, imputed completeness"),
                    tags$li("Winsorized extreme outliers for stable variance"),
                    tags$li("Derived temporal anchors and log transformations")
                  )
                )
              )
            )
          )
        )
      ),

      # ---- TAB 2: Feature Engineering & Enrichment Studio (CREATIVE UPGRADE) -
      tabPanel(
        title = tagList(icon("wand-magic", class = "text-success"), " Feature Studio & Enrichment"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "row",
            # Card 1: Date & Temporal Features
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("calendar-days"), " Temporal Feature Extraction"),
                p(class = "small text-muted", "Derive Year, Month, Quarter, Day, DayOfWeek, and IsWeekend indicator columns from dates."),
                selectInput(ns("fe_date_col"), "Select Date Variable:", choices = NULL, width = "100%"),
                actionButton(ns("btn_fe_date"), "Extract Date Features", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 2: Log Transformation & Skew Normalization
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("chart-line"), " Log1p Normalization"),
                p(class = "small text-muted", "Apply log(1 + x) transformation to right-skewed numeric variables to stabilize variance."),
                selectInput(ns("fe_log_col"), "Select Skewed Variable:", choices = NULL, width = "100%"),
                actionButton(ns("btn_fe_log"), "Apply Log Transform", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 3: Statistical Scaling (Z-Score & Min-Max)
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("arrows-left-right-to-line"), " Variable Scaling & Standardization"),
                p(class = "small text-muted", "Scale features for distance-based ML algorithms (Z-score mean=0/sd=1 or Min-Max [0, 1])."),
                selectInput(ns("fe_scale_col"), "Select Numeric Variable:", choices = NULL, width = "100%"),
                selectInput(ns("fe_scale_method"), "Scaling Technique:", choices = c("Z-Score (Mean=0, SD=1)" = "standardize_z", "Min-Max Normalized [0, 1]" = "min_max", "Robust IQR Scaled" = "robust_iqr"), width = "100%"),
                actionButton(ns("btn_fe_scale"), "Generate Scaled Feature", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 4: Business Ratio Feature
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("divide"), " Business Ratio Features"),
                p(class = "small text-muted", "Create custom business ratios (e.g. debt_to_income = debt / income)."),
                selectInput(ns("fe_ratio_num"), "Numerator Column (A):", choices = NULL, width = "100%"),
                selectInput(ns("fe_ratio_den"), "Denominator Column (B):", choices = NULL, width = "100%"),
                textInput(ns("fe_ratio_name"), "Feature Name (Optional):", placeholder = "e.g. ratio_a_to_b", width = "100%"),
                actionButton(ns("btn_fe_ratio"), "Calculate Ratio Feature", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 5: Quantile Discretization & Binning
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("layer-group"), " Quantile Binning & Tiers"),
                p(class = "small text-muted", "Convert continuous distributions into balanced quantile categories (e.g. Quartiles)."),
                selectInput(ns("fe_bin_col"), "Select Numeric Variable:", choices = NULL, width = "100%"),
                selectInput(ns("fe_bin_count"), "Number of Bins:", choices = c("4 Tiers (Quartiles: Low, Med, High, Top)" = 4, "3 Tiers (Low, Medium, High)" = 3, "5 Tiers (Quintiles)" = 5), selected = 4, width = "100%"),
                actionButton(ns("btn_fe_bin"), "Generate Binned Tiers", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 6: One-Hot Indicator Encoding
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("code-fork"), " One-Hot Category Encoding"),
                p(class = "small text-muted", "Generate binary 0/1 dummy indicator columns for dominant category classes."),
                selectInput(ns("fe_onehot_col"), "Select Categorical Variable:", choices = NULL, width = "100%"),
                actionButton(ns("btn_fe_onehot"), "Derive One-Hot Indicators", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 7: Cyclical Seasonality Encoding (Sine / Cosine)
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("arrows-spin"), " Cyclical Sine/Cosine Encoding"),
                p(class = "small text-muted", "Encode periodic features (months, weekdays, hours) with smooth trigonometric coordinates."),
                selectInput(ns("fe_cyclical_col"), "Periodic Numeric Variable:", choices = NULL, width = "100%"),
                selectInput(ns("fe_cyclical_period"), "Cycle Period:", choices = c("12 (Months of Year)" = 12, "7 (Days of Week)" = 7, "24 (Hours of Day)" = 24), selected = 12, width = "100%"),
                actionButton(ns("btn_fe_cyclical"), "Derive Cyclical Features", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 8: Text Metadata Extraction
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("font"), " Text Metadata Extraction"),
                p(class = "small text-muted", "Derive character length and word count features from text and character attributes."),
                selectInput(ns("fe_text_col"), "Select Text Variable:", choices = NULL, width = "100%"),
                actionButton(ns("btn_fe_text"), "Extract Text Features", icon = icon("plus"), class = "btn btn-sm btn-outline-primary w-100 mt-2")
              )
            ),
            # Card 9: Column Header Lexical Sanitizer
            div(
              class = "col-lg-4 col-md-6 mb-3",
              div(
                class = "p-3 border rounded bg-light h-100",
                h6(class = "fw-bold text-primary", icon("heading"), " Column Name Sanitizer"),
                p(class = "small text-muted", "Standardize all column headers into clean, database-grade snake_case identifiers."),
                selectInput(ns("fe_clean_style"), "Target Casing Style:", choices = c("snake_case (clean_name)" = "snake_case", "SCREAMING_SNAKE (CLEAN_NAME)" = "screaming_snake"), width = "100%"),
                actionButton(ns("btn_fe_cleannames"), "Sanitize All Column Names", icon = icon("wand-magic-sparkles"), class = "btn btn-sm btn-primary w-100 mt-2")
              )
            )
          )
        )
      ),

      # ---- TAB 3: 'Ask Data Detective' Natural Language Intelligence --------
      tabPanel(
        title = tagList(icon("comments", class = "text-info"), " Ask Data Detective"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "p-3 bg-light border rounded mb-3",
            h6(class = "fw-bold", icon("magnifying-glass"), " Inquire in Plain English"),
            p(class = "small text-muted", "Ask questions about distributions, anomalies, key drivers, or summary findings:"),
            div(
              class = "input-group mb-2",
              textInput(ns("nlp_query_input"), label = NULL, placeholder = "e.g. 'What are the key drivers?', 'Show outliers in income', 'Executive summary'...", width = "100%")
            ),
            div(
              class = "d-flex gap-2 flex-wrap align-items-center",
              span(class = "small text-muted fw-bold", "Instant Queries:"),
              actionLink(ns("chip_drivers"), "Key Drivers & Correlations", class = "badge bg-white text-primary border p-2 text-decoration-none"),
              actionLink(ns("chip_outliers"), "Outlier Diagnostics", class = "badge bg-white text-danger border p-2 text-decoration-none"),
              actionLink(ns("chip_missing"), "Missingness Overview", class = "badge bg-white text-warning border text-dark p-2 text-decoration-none"),
              actionLink(ns("chip_summary"), "Executive Summary Story", class = "badge bg-white text-success border p-2 text-decoration-none")
            )
          ),
          uiOutput(ns("nlp_response_card"))
        )
      ),

      # ---- TAB 4: Issue-by-Issue Fix Stream ----------------------------------
      tabPanel(
        title = tagList(icon("list-check"), " Issue-by-Issue Stream"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2",
            div(
              class = "d-flex gap-2 align-items-center flex-wrap",
              selectInput(
                ns("filter_severity"),
                label = NULL,
                choices = c("All Severities" = "ALL", "High Severity" = "HIGH", "Medium Severity" = "MEDIUM", "Low Severity" = "LOW"),
                selected = "ALL",
                width = "160px"
              ),
              selectInput(
                ns("filter_category"),
                label = NULL,
                choices = c("All Categories" = "ALL", "Missing Values" = "Missing Values", "Duplicate Records" = "Duplicate Records", "Outliers" = "Outliers", "Type Mismatch" = "Type Mismatch", "Inconsistent Categories" = "Inconsistent Categories"),
                selected = "ALL",
                width = "190px"
              )
            )
          ),
          uiOutput(ns("issues_cards_stream"))
        )
      ),

      # ---- TAB 5: Missing Values Center -------------------------------------
      tabPanel(
        title = tagList(icon("magnifying-glass-chart"), " Missing Value Center"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "row",
            div(
              class = "col-lg-4 col-md-12",
              div(
                class = "p-3 border rounded bg-light mb-3",
                h6(class = "fw-bold", icon("sliders"), " Missingness Remediation Config"),
                selectInput(ns("miss_col_select"), "Target Column:", choices = NULL, width = "100%"),
                selectInput(
                  ns("miss_method_select"),
                  "Remediation Strategy:",
                  choices = c(
                    "Median Imputation (Numeric)" = "median",
                    "Mean Imputation (Numeric)" = "mean",
                    "Mode Imputation (Frequent value)" = "mode",
                    "Remove Rows with Missing Values" = "remove_rows",
                    "Forward Fill (Time/Ordered)" = "forward_fill",
                    "Backward Fill (Time/Ordered)" = "backward_fill",
                    "Custom Constant Fill" = "custom",
                    "Create Indicator Flag Column" = "indicator_col"
                  ),
                  selected = "median",
                  width = "100%"
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'custom'", ns("miss_method_select")),
                  textInput(ns("miss_custom_val"), "Custom Value:", value = "0", width = "100%"
                )),
                div(
                  class = "d-flex gap-2 mt-3",
                  actionButton(ns("btn_miss_preview"), "Preview Fix", icon = icon("eye"), class = "btn btn-sm btn-outline-primary w-50"),
                  actionButton(ns("btn_miss_apply"), "Apply Imputation", icon = icon("check"), class = "btn btn-sm btn-primary w-50")
                )
              )
            ),
            div(
              class = "col-lg-8 col-md-12",
              div(
                class = "border rounded p-3",
                h6(class = "fw-bold mb-2", icon("table-list"), " Current Missingness Profile by Column"),
                DT::dataTableOutput(ns("miss_table_overview"))
              )
            )
          )
        )
      ),

      # ---- TAB 6: Deduplication Center --------------------------------------
      tabPanel(
        title = tagList(icon("copy"), " Deduplication Center"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "row",
            div(
              class = "col-lg-4 col-md-12",
              div(
                class = "p-3 border rounded bg-light mb-3",
                h6(class = "fw-bold", icon("filter"), " Deduplication Strategy"),
                radioButtons(
                  ns("dup_scope"),
                  "Deduplication Criteria:",
                  choices = c("Exact Duplicate Rows (All Columns)" = "exact", "Key-Based Column Matching" = "keys"),
                  selected = "exact"
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'keys'", ns("dup_scope")),
                  selectizeInput(ns("dup_keys_select"), "Select Key Columns:", choices = NULL, multiple = TRUE, width = "100%")
                ),
                selectInput(
                  ns("dup_keep_rule"),
                  "Instance Retention Rule:",
                  choices = c(
                    "Keep First Instance" = "first",
                    "Keep Last Instance" = "last",
                    "Keep Instance with Fewest NAs" = "fewest_na"
                  ),
                  selected = "first",
                  width = "100%"
                ),
                div(
                  class = "d-flex gap-2 mt-3",
                  actionButton(ns("btn_dup_preview"), "Preview Deduplication", icon = icon("eye"), class = "btn btn-sm btn-outline-primary w-50"),
                  actionButton(ns("btn_dup_apply"), "Deduplicate Now", icon = icon("check"), class = "btn btn-sm btn-danger w-50")
                )
              )
            ),
            div(
              class = "col-lg-8 col-md-12",
              div(
                class = "border rounded p-3",
                h6(class = "fw-bold mb-2", icon("table"), " Identified Duplicate Records Preview"),
                DT::dataTableOutput(ns("dup_table_preview"))
              )
            )
          )
        )
      ),

      # ---- TAB 7: Outlier Treatment -----------------------------------------
      tabPanel(
        title = tagList(icon("chart-simple"), " Outlier Treatment"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "row",
            div(
              class = "col-lg-4 col-md-12",
              div(
                class = "p-3 border rounded bg-light mb-3",
                h6(class = "fw-bold", icon("sliders"), " Outlier Remediation Config"),
                selectInput(ns("out_col_select"), "Target Numeric Variable:", choices = NULL, width = "100%"),
                selectInput(
                  ns("out_action_select"),
                  "Remediation Action:",
                  choices = c(
                    "Winsorize / Clamp to Bounds (Preserve Count)" = "cap_bounds",
                    "Replace Outliers with Median" = "replace_median",
                    "Filter / Delete Outlier Rows" = "remove_rows"
                  ),
                  selected = "cap_bounds",
                  width = "100%"
                ),
                div(
                  class = "d-flex gap-2 mt-3",
                  actionButton(ns("btn_out_preview"), "Preview Treatment", icon = icon("eye"), class = "btn btn-sm btn-outline-primary w-50"),
                  actionButton(ns("btn_out_apply"), "Apply Treatment", icon = icon("check"), class = "btn btn-sm btn-primary w-50")
                )
              )
            ),
            div(
              class = "col-lg-8 col-md-12",
              div(
                class = "border rounded p-3",
                h6(class = "fw-bold mb-2", icon("chart-area"), " Outlier Distribution Diagnostic"),
                plotOutput(ns("plot_outlier_treatment_preview"), height = "280px")
              )
            )
          )
        )
      ),

      # ---- TAB 8: Type & Category Standardization Center --------------------
      tabPanel(
        title = tagList(icon("arrows-rotate"), " Type & Category Center"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "row",
            div(
              class = "col-lg-4 col-md-12",
              div(
                class = "p-3 border rounded bg-light mb-3",
                h6(class = "fw-bold", icon("sliders"), " Type & Text Standardization Config"),
                selectInput(ns("tc_col_select"), "Target Column:", choices = NULL, width = "100%"),
                selectInput(
                  ns("tc_action_select"),
                  "Transformation Action:",
                  choices = c(
                    "Cast to Numeric (Continuous)" = "cast_numeric",
                    "Cast to Integer (Whole Number)" = "cast_integer",
                    "Cast to Character (Text)" = "cast_character",
                    "Cast to Factor (Categorical)" = "cast_factor",
                    "Cast to Logical (Boolean T/F)" = "cast_logical",
                    "Parse as Date" = "cast_date",
                    "Strip Currency & Percentage ($/€/%)" = "currency_percent",
                    "Trim Leading & Trailing Whitespace" = "trim_whitespace",
                    "Convert to Title Case (Standardize)" = "case_title",
                    "Convert to Lowercase" = "case_lower",
                    "Convert to UPPERCASE" = "case_upper"
                  ),
                  selected = "trim_whitespace",
                  width = "100%"
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'cast_date'", ns("tc_action_select")),
                  selectInput(ns("tc_date_format"), "Date Format:", choices = c("%Y-%m-%d" = "%Y-%m-%d", "%d/%m/%Y" = "%d/%m/%Y", "%m/%d/%Y" = "%m/%d/%Y", "%Y/%m/%d" = "%Y/%m/%d"), width = "100%")
                ),
                div(
                  class = "d-flex gap-2 mt-3",
                  actionButton(ns("btn_tc_preview"), "Preview Fix", icon = icon("eye"), class = "btn btn-sm btn-outline-primary w-50"),
                  actionButton(ns("btn_tc_apply"), "Apply Fix", icon = icon("check"), class = "btn btn-sm btn-primary w-50")
                )
              )
            ),
            div(
              class = "col-lg-8 col-md-12",
              div(
                class = "border rounded p-3",
                h6(class = "fw-bold mb-2", icon("table-list"), " Column Type & Category Profile"),
                DT::dataTableOutput(ns("tc_profile_table"))
              )
            )
          )
        )
      ),

      # ---- TAB 9: Business Rules & Custom Validator -------------------------
      tabPanel(
        title = tagList(icon("scale-balanced"), " Business Rules & Validator"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "row",
            div(
              class = "col-lg-4 col-md-12",
              div(
                class = "p-3 border rounded bg-light mb-3",
                h6(class = "fw-bold", icon("scale-balanced"), " Custom Business Rule Builder"),
                p(class = "small text-muted", "Define custom bounds, regex patterns, or missingness constraints to enforce domain integrity."),
                selectInput(ns("rule_col_select"), "Target Column:", choices = NULL, width = "100%"),
                selectInput(
                  ns("rule_op_select"),
                  "Comparison Operator:",
                  choices = c(
                    ">= (Greater than or equal to)" = ">=",
                    "<= (Less than or equal to)" = "<=",
                    "> (Strictly greater than)" = ">",
                    "< (Strictly less than)" = "<",
                    "== (Exactly equal to)" = "==",
                    "!= (Not equal to)" = "!=",
                    "regex_match (Regular Expression)" = "regex_match",
                    "not_missing (Must not be empty/NA)" = "not_missing"
                  ),
                  selected = ">=",
                  width = "100%"
                ),
                textInput(ns("rule_thresh_input"), "Constraint / Threshold Value:", value = "0", width = "100%"),
                selectInput(
                  ns("rule_action_select"),
                  "Enforcement Strategy:",
                  choices = c(
                    "Filter & Remove Non-Compliant Rows" = "filter_violations",
                    "Flag Violations in New Indicator Column" = "flag_indicator"
                  ),
                  selected = "filter_violations",
                  width = "100%"
                ),
                div(
                  class = "d-flex gap-2 mt-3",
                  actionButton(ns("btn_rule_preview"), "Preview Violations", icon = icon("eye"), class = "btn btn-sm btn-outline-primary w-50"),
                  actionButton(ns("btn_rule_apply"), "Enforce Rule", icon = icon("check"), class = "btn btn-sm btn-primary w-50")
                )
              )
            ),
            div(
              class = "col-lg-8 col-md-12",
              div(
                class = "border rounded p-3",
                h6(class = "fw-bold mb-2", icon("triangle-exclamation"), " Non-Compliant Rows Preview"),
                DT::dataTableOutput(ns("rule_violations_table"))
              )
            )
          )
        )
      ),

      # ---- TAB 10: Visual Pipeline DAG & Transformation History -------------
      tabPanel(
        title = tagList(icon("clock-rotate-left"), " Pipeline History & Exports"),
        div(
          class = "p-3 bg-white border border-top-0 rounded-bottom mb-3",
          div(
            class = "d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2",
            div(
              h6(class = "fw-bold mb-0", icon("timeline"), " Session Transformation Audit Trail"),
              span(class = "small text-muted", "Every applied cleaning and feature engineering operation is tracked with full rollback support.")
            ),
            div(
              actionButton(ns("btn_undo_tab"), "Undo Last Operation", icon = icon("arrow-rotate-left"), class = "btn btn-sm btn-warning"),
              actionButton(ns("btn_revert_tab"), "Revert All to Original Data", icon = icon("rotate-left"), class = "btn btn-sm btn-outline-danger")
            )
          ),
          DT::dataTableOutput(ns("history_table"))
        )
      )
    )
  )
}

mod_workbench_server <- function(id, data_working, data_raw, history_stack, findings_r, quality_r, dataset_name_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    skipped_issues <- reactiveVal(character(0))
    active_nlp_query <- reactiveVal("overview")

    # Current synthesized smart recipe
    active_recipe <- reactive({
      df <- data_working()
      req(df)
      f <- tryCatch(findings_r(), error = function(e) NULL)
      p <- inspect_dataset(df)
      synthesize_smart_recipe(df, profile = p, findings = f)
    })

    # ---- 1. Header Badges & Multi-Dimensional Radar -------------------------

    output$score_comparison_badge <- renderUI({
      req(quality_r(), data_raw())
      q_curr <- quality_r()
      
      df_orig <- data_raw()
      p_orig <- inspect_dataset(df_orig)
      m_orig <- detect_missing(df_orig)
      d_orig <- detect_duplicates(df_orig)
      o_orig <- detect_outliers(df_orig)
      c_orig <- detect_constant_columns(df_orig)
      q_orig <- calculate_quality_indicator(p_orig, m_orig, d_orig, o_orig, c_orig)

      delta <- q_curr$score - q_orig$score
      delta_badge <- if (delta > 0) {
        span(class = "badge bg-success fs-6", paste0("+", delta, " pts recovered!"))
      } else if (delta == 0) {
        span(class = "badge bg-secondary fs-6", "Baseline Score")
      } else {
        span(class = "badge bg-warning text-dark fs-6", paste0(delta, " pts"))
      }

      div(
        class = "d-flex align-items-center gap-2",
        div(
          class = "p-2 rounded border text-center bg-light",
          style = "min-width: 85px;",
          div(class = "small text-muted fw-bold", "CURRENT"),
          div(class = "fw-bold fs-4", style = paste0("color: ", q_curr$status_color, ";"), q_curr$score)
        ),
        div(
          class = "p-2 rounded border text-center bg-light",
          style = "min-width: 85px;",
          div(class = "small text-muted fw-bold", "BASELINE"),
          div(class = "fw-bold fs-4 text-muted", q_orig$score)
        ),
        delta_badge
      )
    })

    output$status_pills <- renderUI({
      f <- tryCatch(findings_r(), error = function(e) NULL)
      h <- history_stack()
      n_fixed <- length(h) - 1
      if (n_fixed < 0) n_fixed <- 0
      n_total <- if (!is.null(f) && is.data.frame(f)) nrow(f) else 0

      div(
        class = "d-flex gap-2 flex-wrap",
        span(class = "badge bg-primary", paste0("Detected Issues: ", n_total)),
        span(class = "badge bg-success", paste0("Transformations Applied: ", n_fixed)),
        span(class = "badge bg-info text-white", paste0("Recipe Steps Ready: ", length(active_recipe())))
      )
    })

    output$dataset_version_status <- renderUI({
      h <- history_stack()
      v_num <- length(h)
      df <- data_working()
      r_count <- if (!is.null(df)) nrow(df) else 0
      c_count <- if (!is.null(df)) ncol(df) else 0

      span(
        paste0("Active Dataset: Version ", v_num, " (", format_number(r_count), " rows \u00d7 ", c_count, " cols)")
      )
    })

    # Multi-Dimensional Health Radar Summary
    output$usefulness_radar_summary <- renderUI({
      df <- data_working()
      req(df)
      radar <- calculate_data_usefulness_radar(df)

      scores <- radar$scores
      tagList(
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          span(strong("Usefulness Index: "), span(style = paste0("color: ", radar$status_color, "; font-weight: 800; font-size: 1.2rem;"), paste0(radar$overall_index, " / 100"))),
          span(class = "badge", style = paste0("background-color: ", radar$status_color, "; color: white;"), radar$status)
        ),
        lapply(names(scores), function(dim_name) {
          sc <- scores[[dim_name]]
          bar_color <- if (sc >= 85) "bg-success" else if (sc >= 65) "bg-primary" else if (sc >= 45) "bg-warning" else "bg-danger"
          div(
            class = "mb-2",
            div(class = "d-flex justify-content-between small text-muted", span(gsub("_", " ", dim_name)), span(paste0(sc, "%"))),
            div(class = "progress", style = "height: 6px;", div(class = paste0("progress-bar ", bar_color), style = paste0("width: ", sc, "%;")))
          )
        })
      )
    })

    # Update variable choices across all feature and fix dropdowns
    observe({
      df <- data_working()
      req(df)
      cols <- names(df)
      num_cols <- cols[vapply(df, is.numeric, logical(1))]
      char_cols <- cols[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
      date_cols <- cols[vapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt") || detect_column_type(x) == "date_candidate", logical(1))]

      updateSelectInput(session, "miss_col_select", choices = cols, selected = if (length(cols) > 0) cols[1] else NULL)
      updateSelectizeInput(session, "dup_keys_select", choices = cols)
      updateSelectInput(session, "out_col_select", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "fe_date_col", choices = if (length(date_cols) > 0) date_cols else cols)
      updateSelectInput(session, "fe_log_col", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "fe_scale_col", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "fe_ratio_num", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "fe_ratio_den", choices = num_cols, selected = if (length(num_cols) > 1) num_cols[2] else if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "fe_bin_col", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "fe_onehot_col", choices = if (length(char_cols) > 0) char_cols else cols)
      updateSelectInput(session, "fe_cyclical_col", choices = num_cols, selected = if (length(num_cols) > 0) num_cols[1] else NULL)
      updateSelectInput(session, "fe_text_col", choices = if (length(char_cols) > 0) char_cols else cols)
      updateSelectInput(session, "tc_col_select", choices = cols, selected = if (length(cols) > 0) cols[1] else NULL)
      updateSelectInput(session, "rule_col_select", choices = cols, selected = if (length(cols) > 0) cols[1] else NULL)
    })

    # ---- 2. Smart Recipe List & Executor ------------------------------------

    output$recipe_steps_list <- renderUI({
      recipe <- active_recipe()
      if (length(recipe) == 0) {
        return(div(class = "alert alert-success p-3", icon("check-circle", class = "me-2"), "All primary automated remediation steps complete! Active dataset is clean and structured."))
      }

      step_items <- lapply(recipe, function(step) {
        cat_badge <- switch(
          step$category,
          "Deduplication" = "bg-danger",
          "Sanitization" = "bg-secondary",
          "Normalization" = "bg-info text-white",
          "Type Casting" = "bg-primary",
          "Imputation" = "bg-warning text-dark",
          "Outlier Winsorization" = "bg-dark",
          "Feature Enrichment" = "bg-success",
          "bg-secondary"
        )

        div(
          class = "p-2 mb-2 border rounded bg-white shadow-sm d-flex align-items-center justify-content-between",
          div(
            div(
              span(class = paste0("badge me-2 ", cat_badge), step$category),
              strong(step$title)
            ),
            div(class = "small text-muted mt-1", step$description)
          ),
          span(class = "badge bg-light text-success border ms-2", "\u2713 Ready")
        )
      })

      tagList(step_items)
    })

    # Execute Smart Recipe Handler
    do_apply_recipe <- function() {
      recipe <- active_recipe()
      req(length(recipe) > 0, data_working())

      exec_res <- execute_recipe(data_working(), recipe)
      if (exec_res$steps_completed > 0) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Smart Recipe Execution (", exec_res$steps_completed, " automated steps)"),
          affected_rows = exec_res$affected_rows,
          affected_cells = exec_res$affected_cells,
          data = exec_res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(exec_res$data)
        showNotification(paste0("\u2713 Success! Applied ", exec_res$steps_completed, " transformation & feature steps in 1 click."), type = "default", duration = 5)
      } else {
        showNotification("Recipe completed with no pending actions.", type = "message")
      }
    }

    observeEvent(input$btn_execute_recipe, { do_apply_recipe() })
    observeEvent(input$btn_smart_recipe_top, { do_apply_recipe() })

    # Export Reproducible R Script
    output$btn_export_r_script <- downloadHandler(
      filename = function() {
        orig_name <- if (!is.null(dataset_name_r())) dataset_name_r() else "dataset"
        paste0("preprocess_", tools::file_path_sans_ext(orig_name), ".R")
      },
      content = function(file) {
        recipe <- active_recipe()
        r_code <- generate_reproducible_r_code(recipe, dataset_name = dataset_name_r())
        writeLines(r_code, file)
      }
    )

    # ---- 3. Feature Engineering & Enrichment Handlers -----------------------

    # Date Feature Derivation
    observeEvent(input$btn_fe_date, {
      req(data_working(), input$fe_date_col)
      res <- derive_date_features(data_working(), col = input$fe_date_col)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: Temporal Features (", input$fe_date_col, ")"),
          affected_rows = nrow(res$data),
          affected_cells = length(res$new_columns) * nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # Log Transformation
    observeEvent(input$btn_fe_log, {
      req(data_working(), input$fe_log_col)
      res <- derive_log_transform(data_working(), col = input$fe_log_col)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: Log Transform (", input$fe_log_col, ")"),
          affected_rows = nrow(res$data),
          affected_cells = nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # Scaling Feature
    observeEvent(input$btn_fe_scale, {
      req(data_working(), input$fe_scale_col)
      res <- derive_scaled_features(data_working(), col = input$fe_scale_col, method = input$fe_scale_method)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: Scale Variable (", input$fe_scale_col, ")"),
          affected_rows = nrow(res$data),
          affected_cells = nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # Ratio Feature
    observeEvent(input$btn_fe_ratio, {
      req(data_working(), input$fe_ratio_num, input$fe_ratio_den)
      res <- derive_ratio_features(data_working(), num_col_a = input$fe_ratio_num, num_col_b = input$fe_ratio_den, new_name = input$fe_ratio_name)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: Ratio (", input$fe_ratio_num, " / ", input$fe_ratio_den, ")"),
          affected_rows = nrow(res$data),
          affected_cells = nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # Binning Feature
    observeEvent(input$btn_fe_bin, {
      req(data_working(), input$fe_bin_col)
      res <- derive_binned_features(data_working(), col = input$fe_bin_col, n_bins = as.numeric(input$fe_bin_count))
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: Quantile Binning (", input$fe_bin_col, ")"),
          affected_rows = nrow(res$data),
          affected_cells = nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # One-Hot Encoding
    observeEvent(input$btn_fe_onehot, {
      req(data_working(), input$fe_onehot_col)
      res <- derive_one_hot_encoding(data_working(), col = input$fe_onehot_col)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: One-Hot Encoding (", input$fe_onehot_col, ")"),
          affected_rows = nrow(res$data),
          affected_cells = length(res$new_columns) * nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # Cyclical Features
    observeEvent(input$btn_fe_cyclical, {
      req(data_working(), input$fe_cyclical_col)
      p_val <- as.numeric(if (!is.null(input$fe_cyclical_period)) input$fe_cyclical_period else 12)
      res <- derive_cyclical_features(data_working(), col = input$fe_cyclical_col, period = p_val)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: Cyclical Coordinates (", input$fe_cyclical_col, ", period=", p_val, ")"),
          affected_rows = nrow(res$data),
          affected_cells = length(res$new_columns) * nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # Text Metadata Extraction
    observeEvent(input$btn_fe_text, {
      req(data_working(), input$fe_text_col)
      res <- derive_text_features(data_working(), col = input$fe_text_col)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Enrich: Text Metadata (", input$fe_text_col, ")"),
          affected_rows = nrow(res$data),
          affected_cells = length(res$new_columns) * nrow(res$data),
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # Column Name Sanitizer
    observeEvent(input$btn_fe_cleannames, {
      req(data_working())
      c_style <- if (!is.null(input$fe_clean_style)) input$fe_clean_style else "snake_case"
      res <- transform_clean_names(data_working(), case_style = c_style)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Sanitize Column Names (", c_style, ")"),
          affected_rows = nrow(res$data),
          affected_cells = res$affected_cells,
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    # ---- 4. 'Ask Data Detective' NLP Handlers -------------------------------

    observeEvent(input$chip_drivers, { updateTextInput(session, "nlp_query_input", value = "What are the key drivers and correlations in this dataset?") })
    observeEvent(input$chip_outliers, { updateTextInput(session, "nlp_query_input", value = "Identify extreme outliers and leverage anomalies.") })
    observeEvent(input$chip_missing, { updateTextInput(session, "nlp_query_input", value = "Which columns have missing data and what is the completeness?") })
    observeEvent(input$chip_summary, { updateTextInput(session, "nlp_query_input", value = "Provide an executive summary and dataset story.") })

    output$nlp_response_card <- renderUI({
      q <- input$nlp_query_input
      df <- data_working()
      req(df)
      f <- tryCatch(findings_r(), error = function(e) NULL)
      p <- inspect_dataset(df)

      ans <- answer_data_query(query = if (!is.null(q) && nzchar(trimws(q))) q else "summary", df = df, profile = p, findings = f)

      div(
        class = "card p-3 border-primary shadow-sm",
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          h6(class = "fw-bold text-primary mb-0", icon("lightbulb", class = "me-2"), "DATA DETECTIVE INTELLIGENCE REPORT"),
          span(class = "badge bg-primary text-uppercase", ans$intent)
        ),
        h5(class = "fw-bold text-dark mb-2", ans$headline),
        p(class = "text-secondary mb-2", ans$details),
        div(
          class = "alert alert-light border small p-2 mb-2",
          strong("Recommended Strategic Action: "), ans$recommendation
        )
      )
    })

    # ---- 5. Granular Issue Cards Stream -------------------------------------

    output$issues_cards_stream <- renderUI({
      f <- tryCatch(findings_r(), error = function(e) NULL)
      if (is.null(f) || !is.data.frame(f) || nrow(f) == 0) {
        return(div(
          class = "alert alert-success p-4 text-center",
          icon("circle-check", class = "fs-2 text-success mb-2 d-block"),
          h5("Outstanding! No unresolved quality issues detected in the working dataset.")
        ))
      }

      sev_filter <- input$filter_severity
      cat_filter <- input$filter_category
      
      sub_f <- f
      if (sev_filter != "ALL") sub_f <- sub_f[sub_f$severity == sev_filter, ]
      if (cat_filter != "ALL") sub_f <- sub_f[sub_f$category == cat_filter, ]
      
      skip_ids <- skipped_issues()
      if (length(skip_ids) > 0 && "issue_id" %in% names(sub_f)) {
        sub_f <- sub_f[!(sub_f$issue_id %in% skip_ids), ]
      }

      if (nrow(sub_f) == 0) {
        return(div(class = "alert alert-info p-3", "No issues match the selected filter criteria."))
      }

      card_list <- lapply(seq_len(nrow(sub_f)), function(i) {
        row <- sub_f[i, ]
        iss_id <- if ("issue_id" %in% names(row) && !is.na(row$issue_id)) row$issue_id else paste0("iss_", i)
        sev_color <- switch(row$severity, "HIGH" = "border-danger", "MEDIUM" = "border-warning", "LOW" = "border-info", "border-secondary")
        badge_color <- switch(row$severity, "HIGH" = "bg-danger", "MEDIUM" = "bg-warning text-dark", "LOW" = "bg-info text-white", "bg-secondary")

        div(
          class = paste0("card mb-3 shadow-sm border-start border-4 ", sev_color),
          div(
            class = "card-body p-3",
            div(
              class = "d-flex justify-content-between align-items-center mb-2 flex-wrap gap-2",
              div(
                span(class = paste0("badge me-2 ", badge_color), row$severity),
                strong(class = "fs-6", row$category, if (!is.na(row$column) && row$column != "") paste0(" \u2014 Column '", row$column, "'")),
                if ("affected_count" %in% names(row) && row$affected_count > 0) {
                  span(class = "badge bg-light text-dark border ms-2", paste0(format_number(row$affected_count), " affected rows"))
                }
              ),
              div(
                actionButton(
                  ns(paste0("btn_quick_fix_", iss_id)),
                  "Apply Quick Fix",
                  icon = icon("wand-magic-sparkles"),
                  class = "btn btn-sm btn-outline-success me-1",
                  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});", ns("trigger_quick_fix"), iss_id)
                ),
                actionButton(
                  ns(paste0("btn_skip_", iss_id)),
                  "Skip",
                  icon = icon("forward"),
                  class = "btn btn-sm btn-outline-secondary",
                  onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});", ns("trigger_skip_issue"), iss_id)
                )
              )
            ),
            p(class = "mb-1 text-dark", row$message),
            div(
              class = "small text-muted mb-2",
              strong("Evidence: "), row$evidence, " | ",
              strong("Recommended Action: "), row$recommendation
            )
          )
        )
      })

      tagList(card_list)
    })

    observeEvent(input$trigger_skip_issue, {
      req(input$trigger_skip_issue)
      iss_id <- input$trigger_skip_issue
      curr <- skipped_issues()
      skipped_issues(unique(c(curr, iss_id)))
      showNotification(paste0("Issue '", iss_id, "' skipped for this session."), type = "message", duration = 3)
    })

    observeEvent(input$trigger_quick_fix, {
      req(input$trigger_quick_fix)
      iss_id <- input$trigger_quick_fix
      f <- findings_r()
      req(f)
      matched <- f[f$issue_id == iss_id, ]
      if (nrow(matched) == 0) return()

      row <- matched[1, ]
      df_curr <- data_working()
      res <- NULL

      if (row$fix_category == "missing") {
        res <- transform_missing(df_curr, col = row$column, method = row$recommended_fix)
      } else if (row$fix_category == "duplicate") {
        res <- transform_duplicates(df_curr, keep_rule = row$recommended_fix)
      } else if (row$fix_category == "outlier") {
        res <- transform_outliers(df_curr, col = row$column, action = row$recommended_fix)
      } else if (row$fix_category == "category") {
        res <- transform_categories(df_curr, col = row$column, action = row$recommended_fix)
      } else if (row$fix_category == "type") {
        res <- transform_types(df_curr, col = row$column, target_type = row$recommended_fix)
      }

      if (!is.null(res) && res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Quick-Fix: ", row$category, " (", row$column, ")"),
          affected_rows = res$affected_rows,
          affected_cells = res$affected_cells,
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 Applied fix: ", res$message), type = "default", duration = 4)
      } else {
        err_msg <- if (!is.null(res)) res$message else "Fix could not be automatically applied."
        showNotification(paste0("Fix failed: ", err_msg), type = "error", duration = 5)
      }
    })

    # ---- 6. Missingness, Duplicates, and Outliers Handlers -------------------

    output$miss_table_overview <- DT::renderDataTable({
      df <- data_working()
      req(df)
      m <- detect_missing(df)
      req(m)

      disp <- m$column_summary
      if (nrow(disp) == 0) {
        return(DT::datatable(data.frame(Status = "Zero missing values present."), rownames = FALSE))
      }

      DT::datatable(
        disp[, c("column", "data_type", "missing_count", "missing_pct", "severity")],
        options = list(pageLength = 8, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    observeEvent(input$btn_miss_preview, {
      req(data_working(), input$miss_col_select)
      col <- input$miss_col_select
      meth <- input$miss_method_select
      c_val <- input$miss_custom_val

      res <- transform_missing(data_working(), col = col, method = meth, custom_val = c_val)
      if (!res$success) {
        showNotification(paste0("Cannot preview: ", res$message), type = "error")
        return()
      }

      diff <- compute_transform_diff(data_working(), res$data, col = col)
      show_diff_modal(session, title = paste0("Preview Missing Fix on '", col, "'"), diff_obj = diff, method_desc = res$message)
    })

    observeEvent(input$btn_miss_apply, {
      req(data_working(), input$miss_col_select)
      col <- input$miss_col_select
      meth <- input$miss_method_select
      c_val <- input$miss_custom_val

      res <- transform_missing(data_working(), col = col, method = meth, custom_val = c_val)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Impute Missing (", col, " \u2192 ", meth, ")"),
          affected_rows = res$affected_rows,
          affected_cells = res$affected_cells,
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 Success: ", res$message), type = "default")
      } else {
        showNotification(paste0("Error: ", res$message), type = "error")
      }
    })

    output$dup_table_preview <- DT::renderDataTable({
      df <- data_working()
      req(df)
      scope <- input$dup_scope
      keys <- if (scope == "keys") input$dup_keys_select else NULL
      sub_df <- if (!is.null(keys) && length(keys) > 0) df[, keys, drop = FALSE] else df
      
      dup_rows <- df[duplicated(sub_df) | duplicated(sub_df, fromLast = TRUE), , drop = FALSE]
      if (nrow(dup_rows) == 0) {
        return(DT::datatable(data.frame(Status = "Zero duplicate rows found under current criteria."), rownames = FALSE))
      }

      DT::datatable(
        utils::head(dup_rows, 50),
        options = list(pageLength = 8, scrollX = TRUE, dom = 'frtip'),
        rownames = TRUE,
        class = "compact stripe hover"
      )
    })

    observeEvent(input$btn_dup_preview, {
      req(data_working())
      scope <- input$dup_scope
      keys <- if (scope == "keys") input$dup_keys_select else NULL
      rule <- input$dup_keep_rule

      res <- transform_duplicates(data_working(), key_cols = keys, keep_rule = rule)
      diff <- compute_transform_diff(data_working(), res$data)
      show_diff_modal(session, title = "Preview Deduplication", diff_obj = diff, method_desc = res$message)
    })

    observeEvent(input$btn_dup_apply, {
      req(data_working())
      scope <- input$dup_scope
      keys <- if (scope == "keys") input$dup_keys_select else NULL
      rule <- input$dup_keep_rule

      res <- transform_duplicates(data_working(), key_cols = keys, keep_rule = rule)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Remove Duplicates (keep '", rule, "')"),
          affected_rows = res$affected_rows,
          affected_cells = res$affected_cells,
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 Deduplicated: ", res$message), type = "default")
      }
    })

    output$plot_outlier_treatment_preview <- renderPlot({
      df <- data_working()
      col <- input$out_col_select
      req(df, col, col %in% names(df))

      v <- df[[col]]
      req(is.numeric(v))

      res <- transform_outliers(df, col = col, method = "iqr_1.5", action = "cap_bounds")
      b <- res$bounds

      plot_df <- data.frame(val = v[!is.na(v)])
      ggplot2::ggplot(plot_df, ggplot2::aes(x = val)) +
        ggplot2::geom_histogram(bins = 30, fill = "#3b82f6", alpha = 0.6, color = "#1e40af") +
        ggplot2::geom_vline(xintercept = b[1], color = "#ef4444", linetype = "dashed", size = 1) +
        ggplot2::geom_vline(xintercept = b[2], color = "#ef4444", linetype = "dashed", size = 1) +
        ggplot2::annotate("text", x = b[2], y = Inf, label = paste0(" Upper Bound: ", round(b[2], 2)), vjust = 2, hjust = -0.1, color = "#ef4444", fontface = "bold") +
        ggplot2::labs(title = paste0("Distribution & Boundary Fences for '", col, "'"), x = col, y = "Count") +
        ggplot2::theme_minimal(base_size = 13)
    })

    observeEvent(input$btn_out_preview, {
      req(data_working(), input$out_col_select)
      col <- input$out_col_select
      act <- input$out_action_select

      res <- transform_outliers(data_working(), col = col, method = "iqr_1.5", action = act)
      diff <- compute_transform_diff(data_working(), res$data, col = col)
      show_diff_modal(session, title = paste0("Preview Outlier Treatment on '", col, "'"), diff_obj = diff, method_desc = res$message)
    })

    observeEvent(input$btn_out_apply, {
      req(data_working(), input$out_col_select)
      col <- input$out_col_select
      act <- input$out_action_select

      res <- transform_outliers(data_working(), col = col, method = "iqr_1.5", action = act)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Outlier Treatment (", col, " \u2192 ", act, ")"),
          affected_rows = res$affected_rows,
          affected_cells = res$affected_cells,
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 Outliers Treated: ", res$message), type = "default")
      }
    })

    # ---- 7. Undo, Revert, and History Handlers -------------------------------

    output$history_table <- DT::renderDataTable({
      h <- history_stack()
      if (length(h) <= 1) {
        return(DT::datatable(
          data.frame(Version = 1, Timestamp = "Initial", Action = "Original Dataset Ingestion", Affected_Rows = 0, Affected_Cells = 0),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      h_rows <- lapply(h, function(item) {
        data.frame(
          Version = item$id,
          Timestamp = item$timestamp,
          Action = item$action,
          Affected_Rows = format_number(item$affected_rows),
          Affected_Cells = format_number(item$affected_cells),
          stringsAsFactors = FALSE
        )
      })

      h_df <- do.call(rbind, h_rows)
      DT::datatable(
        h_df,
        options = list(pageLength = 10, dom = 'tip'),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    do_undo <- function() {
      h <- history_stack()
      if (length(h) <= 1) {
        showNotification("Already at baseline version. Nothing to undo.", type = "warning")
        return()
      }
      h_new <- h[seq_len(length(h) - 1)]
      history_stack(h_new)
      prev_entry <- h_new[[length(h_new)]]
      data_working(prev_entry$data)
      showNotification(paste0("\u21ba Undid last action. Restored Version ", prev_entry$id, "."), type = "message")
    }

    observeEvent(input$btn_undo_top, { do_undo() })
    observeEvent(input$btn_undo_tab, { do_undo() })

    do_revert <- function() {
      req(data_raw())
      df_orig <- data_raw()
      init_entry <- list(
        id = 1,
        timestamp = format(Sys.time(), "%H:%M:%S"),
        action = "Baseline Reset to Original Ingestion",
        affected_rows = 0,
        affected_cells = 0,
        data = df_orig
      )
      history_stack(list(init_entry))
      data_working(df_orig)
      skipped_issues(character(0))
      showNotification("\u21ba Reverted to original raw dataset. History stack cleared.", type = "warning", duration = 4)
    }

    observeEvent(input$btn_revert_top, { do_revert() })
    observeEvent(input$btn_revert_tab, { do_revert() })

    output$btn_export_clean <- downloadHandler(
      filename = function() {
        orig_name <- if (!is.null(dataset_name_r())) dataset_name_r() else "dataset"
        base_name <- tools::file_path_sans_ext(orig_name)
        paste0(base_name, "_useful_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- data_working()
        req(df)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # ---- 7. Type & Category Center Handlers ---------------------------------
    output$tc_profile_table <- DT::renderDataTable({
      df <- data_working()
      req(df)
      col <- input$tc_col_select
      if (is.null(col) || !(col %in% names(df))) col <- names(df)[1]

      vec <- df[[col]]
      p_type <- detect_column_type(vec)
      n_distinct <- length(unique(vec[!is.na(vec)]))
      sample_vals <- paste(head(as.character(unique(vec[!is.na(vec)])), 5), collapse = ", ")

      tab_summary <- data.frame(
        Property = c("Column Name", "Current Storage Class", "Inferred Semantic Type", "Total Observations", "Non-Missing Count", "Distinct Values", "Top Sample Values"),
        Value = c(col, class(vec)[1], p_type, as.character(length(vec)), as.character(sum(!is.na(vec))), as.character(n_distinct), sample_vals),
        stringsAsFactors = FALSE
      )

      DT::datatable(tab_summary, options = list(dom = 't', paging = FALSE), rownames = FALSE, class = "compact stripe hover")
    })

    observeEvent(input$btn_tc_preview, {
      req(data_working(), input$tc_col_select, input$tc_action_select)
      col <- input$tc_col_select
      act <- input$tc_action_select
      df_curr <- data_working()
      res <- NULL

      if (grepl("^cast_", act)) {
        t_type <- gsub("^cast_", "", act)
        d_fmt <- if (!is.null(input$tc_date_format)) input$tc_date_format else "%Y-%m-%d"
        res <- transform_types(df_curr, col = col, target_type = t_type, date_format = d_fmt)
      } else if (act == "currency_percent") {
        res <- transform_currency_percent(df_curr, col = col)
      } else {
        res <- transform_categories(df_curr, col = col, action = act)
      }

      if (is.null(res) || !res$success) {
        showNotification(paste0("Cannot preview: ", if (!is.null(res)) res$message else "Action failed."), type = "error")
        return()
      }

      diff <- compute_transform_diff(df_curr, res$data, col = col)
      show_diff_modal(session, title = paste0("Preview Type/Text Fix on '", col, "'"), diff_obj = diff, method_desc = res$message)
    })

    observeEvent(input$btn_tc_apply, {
      req(data_working(), input$tc_col_select, input$tc_action_select)
      col <- input$tc_col_select
      act <- input$tc_action_select
      df_curr <- data_working()
      res <- NULL

      if (grepl("^cast_", act)) {
        t_type <- gsub("^cast_", "", act)
        d_fmt <- if (!is.null(input$tc_date_format)) input$tc_date_format else "%Y-%m-%d"
        res <- transform_types(df_curr, col = col, target_type = t_type, date_format = d_fmt)
      } else if (act == "currency_percent") {
        res <- transform_currency_percent(df_curr, col = col)
      } else {
        res <- transform_categories(df_curr, col = col, action = act)
      }

      if (!is.null(res) && res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Type/Category Fix: ", act, " (", col, ")"),
          affected_rows = res$affected_rows,
          affected_cells = res$affected_cells,
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 Applied fix: ", res$message), type = "default", duration = 4)
      } else {
        err_msg <- if (!is.null(res)) res$message else "Transformation could not be applied."
        showNotification(paste0("Fix failed: ", err_msg), type = "error", duration = 5)
      }
    })

    # ---- 8. Business Rules & Validator Handlers -----------------------------
    output$rule_violations_table <- DT::renderDataTable({
      df <- data_working()
      req(df)
      col <- input$rule_col_select
      op  <- input$rule_op_select
      thr <- input$rule_thresh_input
      if (is.null(col) || !(col %in% names(df))) col <- names(df)[1]

      vec <- df[[col]]
      is_valid <- rep(TRUE, nrow(df))
      if (op == ">=") is_valid <- !is.na(vec) & (suppressWarnings(as.numeric(vec)) >= suppressWarnings(as.numeric(thr)))
      else if (op == "<=") is_valid <- !is.na(vec) & (suppressWarnings(as.numeric(vec)) <= suppressWarnings(as.numeric(thr)))
      else if (op == ">") is_valid <- !is.na(vec) & (suppressWarnings(as.numeric(vec)) > suppressWarnings(as.numeric(thr)))
      else if (op == "<") is_valid <- !is.na(vec) & (suppressWarnings(as.numeric(vec)) < suppressWarnings(as.numeric(thr)))
      else if (op == "==") is_valid <- !is.na(vec) & (as.character(vec) == as.character(thr))
      else if (op == "!=") is_valid <- !is.na(vec) & (as.character(vec) != as.character(thr))
      else if (op == "regex_match") is_valid <- !is.na(vec) & grepl(as.character(thr), as.character(vec))
      else if (op == "not_missing") is_valid <- !is.na(vec) & trimws(as.character(vec)) != ""

      viol_rows <- df[!is_valid, , drop = FALSE]
      if (nrow(viol_rows) == 0) {
        return(DT::datatable(data.frame(Status = "All rows comply with the specified constraint."), rownames = FALSE))
      }

      DT::datatable(utils::head(viol_rows, 50), options = list(pageLength = 8, scrollX = TRUE, dom = 'frtip'), rownames = TRUE, class = "compact stripe hover")
    })

    observeEvent(input$btn_rule_preview, {
      req(data_working(), input$rule_col_select)
      col <- input$rule_col_select
      op  <- input$rule_op_select
      thr <- input$rule_thresh_input
      act <- input$rule_action_select

      res <- transform_business_rules(data_working(), col = col, operator = op, threshold = thr, action = act)
      if (!res$success) {
        showNotification(paste0("Rule Preview: ", res$message), type = "message")
        return()
      }

      diff <- compute_transform_diff(data_working(), res$data, col = col)
      show_diff_modal(session, title = paste0("Rule Enforcement Preview on '", col, "'"), diff_obj = diff, method_desc = res$message)
    })

    observeEvent(input$btn_rule_apply, {
      req(data_working(), input$rule_col_select)
      col <- input$rule_col_select
      op  <- input$rule_op_select
      thr <- input$rule_thresh_input
      act <- input$rule_action_select

      res <- transform_business_rules(data_working(), col = col, operator = op, threshold = thr, action = act)
      if (res$success) {
        h <- history_stack()
        new_entry <- list(
          id = length(h) + 1,
          timestamp = format(Sys.time(), "%H:%M:%S"),
          action = paste0("Business Rule Enforcement (", col, " ", op, " ", thr, ")"),
          affected_rows = res$affected_rows,
          affected_cells = res$affected_cells,
          data = res$data
        )
        history_stack(c(h, list(new_entry)))
        data_working(res$data)
        showNotification(paste0("\u2713 Applied rule: ", res$message), type = "default", duration = 4)
      } else {
        showNotification(paste0("Rule failed: ", res$message), type = "error")
      }
    })

  })
}

# Helper modal to display side-by-side diff
show_diff_modal <- function(session, title, diff_obj, method_desc) {
  ns <- session$ns
  showModal(modalDialog(
    title = div(icon("magnifying-glass-arrow-right", class = "text-primary me-2"), title),
    size = "l",
    easyClose = TRUE,
    div(
      class = "mb-3 p-2 bg-light border rounded",
      p(class = "mb-1 fw-bold text-primary", method_desc),
      div(
        class = "d-flex gap-3 small text-muted",
        span(strong("Rows Before: "), diff_obj$rows_before),
        span(strong("Rows After: "), diff_obj$rows_after),
        span(strong("Row Delta: "), if (diff_obj$row_delta < 0) span(class = "text-danger fw-bold", diff_obj$row_delta) else if (diff_obj$row_delta > 0) span(class = "text-success fw-bold", paste0("+", diff_obj$row_delta)) else "0"),
        span(strong("Modified Cells: "), format_number(diff_obj$changed_cells))
      )
    ),
    h6(class = "fw-bold", "Sample Changed Observations (Diff):"),
    if (nrow(diff_obj$diff_preview) > 0) {
      tags$div(
        style = "max-height: 350px; overflow-y: auto;",
        tags$table(
          class = "table table-sm table-bordered small mb-0",
          tags$thead(
            tags$tr(
              lapply(names(diff_obj$diff_preview), function(h) tags$th(h))
            )
          ),
          tags$tbody(
            lapply(seq_len(min(30, nrow(diff_obj$diff_preview))), function(idx) {
              row <- diff_obj$diff_preview[idx, ]
              tags$tr(
                lapply(seq_along(row), function(col_i) {
                  val <- as.character(row[[col_i]])
                  cn <- names(row)[col_i]
                  if (cn == "Original_Value") {
                    tags$td(class = "diff-original", tags$del(val))
                  } else if (cn == "New_Value") {
                    tags$td(class = "diff-proposed", tags$strong(val))
                  } else {
                    tags$td(val)
                  }
                })
              )
            })
          )
        )
      )
    } else {
      div(class = "alert alert-info small", "No values would be modified by this operation.")
    },
    footer = tagList(
      modalButton("Dismiss Preview")
    )
  ))
}
