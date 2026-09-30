# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_distributions.R
# Description: Distribution investigation, histogram/density/boxplot, factual observations
# ==============================================================================

mod_distributions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "p-3",
      div(
        class = "panel-box mb-3",
        div(
          class = "row align-items-center",
          div(
            class = "col-md-5",
            selectInput(ns("select_num_var"), "Select Numeric Variable to Examine:", choices = NULL, width = "100%")
          ),
          div(
            class = "col-md-7",
            div(class = "small text-muted", "Factual observations and empirical distribution profiles are generated below without assuming normality.")
          )
        )
      ),

      # Metrics & Factual Observations Grid
      div(
        class = "row mb-3",
        div(
          class = "col-lg-7 col-md-12",
          div(
            class = "panel-box h-100",
            h5(class = "panel-title", icon("lightbulb"), " Factual Observations"),
            uiOutput(ns("factual_obs_list")),
            div(class = "small text-muted mt-2", "* Observations report observed arithmetic properties. They do not assert statistical significance without formal hypothesis testing.")
          )
        ),
        div(
          class = "col-lg-5 col-md-12",
          div(
            class = "panel-box h-100",
            h5(class = "panel-title", icon("calculator"), " Summary Parameters"),
            uiOutput(ns("summary_metrics_grid"))
          )
        )
      ),

      # Charts Row
      div(
        class = "row",
        div(
          class = "col-lg-7 col-md-12",
          div(
            class = "panel-box mb-3",
            h5(class = "panel-title", icon("chart-area"), " Histogram with Empirical Density Curve"),
            plotOutput(ns("plot_hist_density"), height = "340px")
          )
        ),
        div(
          class = "col-lg-5 col-md-12",
          div(
            class = "panel-box mb-3",
            h5(class = "panel-title", icon("box-archive"), " Five-Number Boxplot"),
            plotOutput(ns("plot_dist_box"), height = "340px")
          )
        )
      )
    )
  )
}

mod_distributions_server <- function(id, data_r, profile_r) {
  moduleServer(id, function(input, output, session) {

    # Populate numeric choices
    observe({
      req(data_r(), profile_r())
      num_cols <- names(data_r())[vapply(data_r(), function(x) is.numeric(x) && !is.logical(x), logical(1))]
      if (length(num_cols) > 0) {
        current <- input$select_num_var
        selected <- if (!is.null(current) && current %in% num_cols) current else num_cols[1]
        updateSelectInput(session, "select_num_var", choices = num_cols, selected = selected)
      } else {
        updateSelectInput(session, "select_num_var", choices = character(0))
      }
    })

    # Reactive distribution analysis
    dist_info <- reactive({
      req(input$select_num_var, data_r())
      v <- data_r()[[input$select_num_var]]
      req(is.numeric(v))
      analyze_distribution(v, var_name = input$select_num_var)
    })

    # Factual Observations UI
    output$factual_obs_list <- renderUI({
      req(dist_info())
      info <- dist_info()

      if (!info$valid) {
        return(p(class = "text-muted", info$message))
      }

      tags$ul(
        class = "list-group list-group-flush small",
        lapply(info$observations, function(ob) {
          tags$li(class = "list-group-item d-flex align-items-center", icon("check", class = "text-primary me-2"), ob)
        })
      )
    })

    # Summary Metrics Grid UI
    output$summary_metrics_grid <- renderUI({
      req(dist_info())
      info <- dist_info()
      req(info$valid)
      m <- info$metrics

      tagList(
        div(
          class = "metrics-compact-grid",
          div(class = "compact-cell", strong("Mean: "), round(m$mean, 3)),
          div(class = "compact-cell", strong("Median: "), round(m$median, 3)),
          div(class = "compact-cell", strong("Std Dev: "), round(m$sd, 3)),
          div(class = "compact-cell", strong("Variance: "), round(m$variance, 3)),
          div(class = "compact-cell", strong("Min: "), round(m$min, 3)),
          div(class = "compact-cell", strong("Q1 (25%): "), round(m$q1, 3)),
          div(class = "compact-cell", strong("Q3 (75%): "), round(m$q3, 3)),
          div(class = "compact-cell", strong("Max: "), round(m$max, 3)),
          div(class = "compact-cell", strong("IQR: "), round(m$iqr, 3)),
          div(class = "compact-cell", strong("Skewness: "), ifelse(is.na(m$skewness), "-", round(m$skewness, 3))),
          div(class = "compact-cell", strong("Kurtosis: "), ifelse(is.na(m$kurtosis), "-", round(m$kurtosis, 3))),
          div(class = "compact-cell", strong("Unique Vals: "), format_number(m$n_unique))
        )
      )
    })

    # Histogram & Density
    output$plot_hist_density <- renderPlot({
      req(dist_info(), data_r(), input$select_num_var)
      v <- data_r()[[input$select_num_var]]
      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 2)

      plot_df <- data.frame(val = clean_v)

      ggplot2::ggplot(plot_df, ggplot2::aes(x = val)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30, fill = "#93c5fd", color = "#ffffff", alpha = 0.7) +
        ggplot2::geom_density(color = "#1d4ed8", linewidth = 1.2) +
        ggplot2::labs(
          title = paste0("Density & Histogram: ", input$select_num_var),
          x = input$select_num_var,
          y = "Density"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Boxplot
    output$plot_dist_box <- renderPlot({
      req(dist_info(), data_r(), input$select_num_var)
      v <- data_r()[[input$select_num_var]]
      clean_v <- v[!is.na(v) & is.finite(v)]
      req(length(clean_v) > 2)

      plot_df <- data.frame(val = clean_v, var = input$select_num_var)

      ggplot2::ggplot(plot_df, ggplot2::aes(x = var, y = val)) +
        ggplot2::geom_boxplot(fill = "#dbeafe", color = "#1e40af", outlier.color = "#ef4444", outlier.size = 2.5) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste0("Boxplot: ", input$select_num_var),
          x = "",
          y = input$select_num_var
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })
  })
}
