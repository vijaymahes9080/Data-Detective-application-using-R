# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_correlation.R
# Description: Module 8 — Correlation Analysis (Heatmap, Pearson/Spearman, & Pairs)
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_correlation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "panel-box mb-3",
      div(
        class = "row align-items-center",
        div(
          class = "col-md-4",
          selectInput(ns("corr_method"), "Correlation Coefficient Method:", choices = c("Pearson (Linear)" = "pearson", "Spearman (Rank)" = "spearman"), selected = "pearson")
        ),
        div(
          class = "col-md-4",
          sliderInput(ns("threshold_slider"), "Highlight Correlation Threshold (|r|):", min = 0.3, max = 0.95, value = 0.70, step = 0.05)
        ),
        div(
          class = "col-md-4",
          p(class = "small text-muted mb-0",
            strong("Methodological Notice: "),
            "Correlation indicates statistical co-movement/association, not causation. It evaluates linear dependency between variables."
          )
        )
      )
    ),

    # Heatmap & Multicollinearity Row
    div(
      class = "row mb-3",
      div(
        class = "col-lg-7 col-md-12",
        div(class = "panel-box", h5(class = "panel-title", icon("braille"), " Correlation Matrix Heatmap"), plotOutput(ns("plot_heatmap"), height = "380px"))
      ),
      div(
        class = "col-lg-5 col-md-12",
        div(class = "panel-box h-100", h5(class = "panel-title", icon("triangle-exclamation"), " Multicollinearity Alerts"), DT::dataTableOutput(ns("table_alerts")))
      )
    ),

    # Complete Pairwise Table
    div(
      class = "panel-box",
      h5(class = "panel-title", icon("table-list"), " Ranked Pairwise Associations"),
      DT::dataTableOutput(ns("table_pairs"))
    )
  )
}

mod_correlation_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {

    # Reactive correlation computation
    corr_obj <- reactive({
      req(data_r())
      calculate_correlations(
        data_r(),
        method = input$corr_method,
        strong_thresh = input$threshold_slider
      )
    })

    # Heatmap
    output$plot_heatmap <- renderPlot({
      req(corr_obj())
      co <- corr_obj()

      if (!co$has_correlations) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 1, y = 1, label = "At least two varying numeric variables are required.", size = 4) +
                 ggplot2::theme_void())
      }

      mat <- co$matrix
      req(nrow(mat) >= 2)

      # Reshape
      melt_df <- as.data.frame(as.table(mat))
      names(melt_df) <- c("Var1", "Var2", "Correlation")
      melt_df$text_color <- ifelse(abs(melt_df$Correlation) > 0.65, "#ffffff", "#0f172a")

      ggplot2::ggplot(melt_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
        ggplot2::geom_tile(color = "#ffffff", linewidth = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation), color = text_color), size = 3.6, fontface = "bold", show.legend = FALSE) +
        ggplot2::scale_color_identity() +
        ggplot2::scale_fill_gradient2(low = "#dc2626", mid = "#f8fafc", high = "#1d4ed8", midpoint = 0, limits = c(-1, 1), name = "Correlation r") +
        theme_detective() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "bold", color = "#1e293b"),
          axis.text.y = ggplot2::element_text(face = "bold", color = "#1e293b"),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
    })

    # Multicollinearity Alert Table
    output$table_alerts <- DT::renderDataTable({
      req(corr_obj())
      co <- corr_obj()

      if (!co$has_correlations || nrow(co$multicollinear_pairs) == 0) {
        return(DT::datatable(
          data.frame(Status = paste0("No variable pairs exceed the selected |r| \u2265 ", input$threshold_slider, " threshold.")),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Variable_1 = co$multicollinear_pairs$variable_a,
        Variable_2 = co$multicollinear_pairs$variable_b,
        Correlation = co$multicollinear_pairs$correlation,
        Strength = co$multicollinear_pairs$strength,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 6, dom = 'tp'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Full Pairwise Table
    output$table_pairs <- DT::renderDataTable({
      req(corr_obj())
      co <- corr_obj()

      if (!co$has_correlations || nrow(co$pairs) == 0) {
        return(DT::datatable(
          data.frame(Status = "No correlation pairs available."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Variable_A = co$pairs$variable_a,
        Variable_B = co$pairs$variable_b,
        Coefficient = co$pairs$correlation,
        Strength = co$pairs$strength,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 8, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
