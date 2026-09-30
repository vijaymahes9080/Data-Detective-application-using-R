# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_distributions.R
# Description: Module 7 — Numerical & Categorical Distribution Analysis
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_distributions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("dist_tabs"),

      # Tab 1: Numeric Distributions
      tabPanel(
        title = tagList(icon("chart-area"), " Numeric Distributions"),
        div(
          class = "p-3",
          div(
            class = "panel-box mb-3",
            div(
              class = "row align-items-center",
              div(class = "col-md-5", selectInput(ns("select_num"), "Select Numeric Variable:", choices = NULL, width = "100%")),
              div(class = "col-md-7", uiOutput(ns("skew_alert")))
            )
          ),
          div(
            class = "row mb-3",
            div(
              class = "col-lg-7 col-md-12",
              div(class = "panel-box", h6("Histogram with Empirical Density"), plotOutput(ns("plot_num_hist"), height = "320px"))
            ),
            div(
              class = "col-lg-5 col-md-12",
              div(class = "panel-box", h6("Summary Parameters"), uiOutput(ns("num_stats_grid")))
            )
          )
        )
      ),

      # Tab 2: Categorical Distributions
      tabPanel(
        title = tagList(icon("tags"), " Categorical Distributions"),
        div(
          class = "p-3",
          div(
            class = "panel-box mb-3",
            div(
              class = "row align-items-center",
              div(class = "col-md-5", selectInput(ns("select_cat"), "Select Categorical Variable:", choices = NULL, width = "100%")),
              div(class = "col-md-7", uiOutput(ns("cat_alert")))
            )
          ),
          div(
            class = "row",
            div(
              class = "col-lg-7 col-md-12",
              div(class = "panel-box", h6("Class Frequencies"), plotOutput(ns("plot_cat_bar"), height = "320px"))
            ),
            div(
              class = "col-lg-5 col-md-12",
              div(class = "panel-box", h6("Frequency Table"), DT::dataTableOutput(ns("table_cat_freq")))
            )
          )
        )
      )
    )
  )
}

mod_distributions_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {

    # Update variable choices
    observe({
      req(data_r())
      num_cols <- names(data_r())[vapply(data_r(), function(x) is.numeric(x) && !is.logical(x), logical(1))]
      cat_cols <- names(data_r())[vapply(data_r(), function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]

      if (length(num_cols) > 0) updateSelectInput(session, "select_num", choices = num_cols, selected = num_cols[1])
      if (length(cat_cols) > 0) updateSelectInput(session, "select_cat", choices = cat_cols, selected = cat_cols[1])
    })

    # Numeric Skewness Alert
    output$skew_alert <- renderUI({
      req(input$select_num, data_r())
      v <- data_r()[[input$select_num]]
      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 2)

      sk <- safe_skewness(clean_v)
      if (is.na(sk)) return(NULL)

      if (abs(sk) >= 1.0) {
        div(
          class = "alert alert-warning p-2 small mb-0",
          icon("triangle-exclamation"),
          strong(" Pronounced Skewness Detected: "),
          paste0("Sample skewness is ", round(sk, 2), " (", ifelse(sk > 0, "Right/Positive", "Left/Negative"), " skew). Consider transformation if applying linear models.")
        )
      } else {
        div(
          class = "alert alert-success p-2 small mb-0",
          icon("check-circle"),
          paste0("Distribution exhibits moderate symmetry (Skewness = ", round(sk, 2), ").")
        )
      }
    })

    # Numeric Histogram
    output$plot_num_hist <- renderPlot({
      req(input$select_num, data_r())
      v <- data_r()[[input$select_num]]
      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 2)

      plot_df <- data.frame(val = clean_v)

      ggplot2::ggplot(plot_df, ggplot2::aes(x = val)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30, fill = "#93c5fd", color = "#ffffff", alpha = 0.75) +
        ggplot2::geom_density(color = "#1d4ed8", linewidth = 1.2) +
        ggplot2::labs(title = paste0("Empirical Distribution: ", input$select_num), x = input$select_num, y = "Density") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Numeric Stats Grid
    output$num_stats_grid <- renderUI({
      req(input$select_num, data_r())
      v <- data_r()[[input$select_num]]
      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 0)

      q <- stats::quantile(clean_v, probs = c(0, 0.25, 0.5, 0.75, 1))

      div(
        class = "metrics-compact-grid",
        div(class = "compact-cell", strong("Mean: "), round(mean(clean_v), 3)),
        div(class = "compact-cell", strong("Median: "), round(q[3], 3)),
        div(class = "compact-cell", strong("Std Dev: "), round(safe_sd(clean_v), 3)),
        div(class = "compact-cell", strong("IQR: "), round(q[4] - q[2], 3)),
        div(class = "compact-cell", strong("Min: "), round(q[1], 3)),
        div(class = "compact-cell", strong("Max: "), round(q[5], 3)),
        div(class = "compact-cell", strong("Skewness: "), round(safe_skewness(clean_v), 3)),
        div(class = "compact-cell", strong("Valid N: "), format_number(length(clean_v)))
      )
    })

    # Categorical Concentration Alert
    output$cat_alert <- renderUI({
      req(input$select_cat, data_r())
      v <- data_r()[[input$select_cat]]
      non_na <- v[!is.na(v) & trimws(as.character(v)) != ""]
      req(length(non_na) > 0)

      tab <- table(non_na)
      dom_pct <- max(tab) / length(non_na)

      if (dom_pct >= 0.90) {
        div(
          class = "alert alert-warning p-2 small mb-0",
          icon("triangle-exclamation"),
          strong(" High Category Concentration: "),
          paste0("The class '", names(tab)[which.max(tab)], "' represents ", round(dom_pct * 100, 1), "% of observations.")
        )
      } else {
        div(
          class = "alert alert-success p-2 small mb-0",
          icon("check-circle"),
          paste0("Evenly distributed across ", length(tab), " unique categories.")
        )
      }
    })

    # Categorical Bar Plot
    output$plot_cat_bar <- renderPlot({
      req(input$select_cat, data_r())
      v <- data_r()[[input$select_cat]]
      non_na <- v[!is.na(v) & trimws(as.character(v)) != ""]
      req(length(non_na) > 0)

      tab <- sort(table(non_na), decreasing = TRUE)
      plot_tab <- utils::head(tab, 12)
      plot_df <- data.frame(Class = names(plot_tab), Count = as.numeric(plot_tab))
      plot_df$Class <- factor(plot_df$Class, levels = rev(plot_df$Class))

      ggplot2::ggplot(plot_df, ggplot2::aes(x = Class, y = Count)) +
        ggplot2::geom_col(fill = "#6366f1", width = 0.6) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = paste0("Class Frequencies: ", input$select_cat), x = "", y = "Record Count") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Categorical Table
    output$table_cat_freq <- DT::renderDataTable({
      req(input$select_cat, data_r())
      v <- data_r()[[input$select_cat]]
      non_na <- v[!is.na(v) & trimws(as.character(v)) != ""]
      req(length(non_na) > 0)

      tab <- sort(table(non_na), decreasing = TRUE)
      n_total <- length(non_na)
      counts <- as.numeric(tab)
      pcts <- round((counts / n_total) * 100, 2)

      disp_df <- data.frame(
        Category = names(tab),
        Count = format_number(counts),
        Percentage = paste0(pcts, "%"),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        utils::head(disp_df, 50),
        options = list(pageLength = 8, dom = 'tp'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
