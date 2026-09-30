# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_correlations.R
# Description: Correlation matrix heatmap, pairwise association table & multicollinearity
# ==============================================================================

mod_correlations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "p-3",
      # Correlation Heatmap & Multicollinearity Banner
      div(
        class = "row",
        div(
          class = "col-lg-7 col-md-12",
          div(
            class = "panel-box mb-3",
            div(
              class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
              h5(class = "panel-title mb-0", icon("braille"), " Pearson Correlation Heatmap"),
              span(class = "badge bg-secondary", "Pairwise Complete Observations")
            ),
            plotOutput(ns("plot_heatmap"), height = "420px")
          )
        ),
        div(
          class = "col-lg-5 col-md-12",
          div(
            class = "panel-box mb-3 h-100",
            h5(class = "panel-title", icon("triangle-exclamation"), " Multicollinearity Alerts (|r| \u2265 0.70)"),
            p(class = "small text-muted",
              strong("Methodological Notice: "),
              "Correlation indicates statistical association, not causation. Highly correlated pairs contain overlapping linear variance. ",
              "Data Detective does not automatically remove features."
            ),
            DT::dataTableOutput(ns("table_multicollinearity"))
          )
        )
      ),

      # Interactive Scatter Plot Explorer
      div(
        class = "panel-box mt-3",
        h5(class = "panel-title", icon("circle-nodes"), " Bivariate Relationship Scatter Plot"),
        div(
          class = "row align-items-center mb-2",
          div(class = "col-md-5", selectInput(ns("scatter_x"), "Variable X (Horizontal Axis):", choices = NULL)),
          div(class = "col-md-5", selectInput(ns("scatter_y"), "Variable Y (Vertical Axis):", choices = NULL)),
          div(class = "col-md-2 mt-3", uiOutput(ns("scatter_r_badge")))
        ),
        plotOutput(ns("plot_scatter"), height = "360px")
      ),

      # Full Pairwise Correlation Table
      div(
        class = "panel-box mt-3",
        h5(class = "panel-title", icon("table-list"), " Complete Pairwise Correlation Table"),
        DT::dataTableOutput(ns("table_pairs"))
      )
    )
  )
}

mod_correlations_server <- function(id, data_r, corr_r) {
  moduleServer(id, function(input, output, session) {

    # Update variable choices for scatter plot
    observe({
      req(corr_r())
      if (corr_r()$has_correlations && length(corr_r()$numeric_columns) >= 2) {
        cols <- corr_r()$numeric_columns
        updateSelectInput(session, "scatter_x", choices = cols, selected = cols[1])
        updateSelectInput(session, "scatter_y", choices = cols, selected = cols[2])
      } else {
        updateSelectInput(session, "scatter_x", choices = character(0))
        updateSelectInput(session, "scatter_y", choices = character(0))
      }
    })

    # Heatmap Plot
    output$plot_heatmap <- renderPlot({
      req(corr_r())
      if (!corr_r()$has_correlations) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 1, y = 1, label = "Insufficient numeric variables for correlation matrix.", size = 4) +
                 ggplot2::theme_void())
      }

      mat <- corr_r()$matrix
      req(nrow(mat) >= 2)

      # Reshape matrix for ggplot
      melt_df <- as.data.frame(as.table(mat))
      names(melt_df) <- c("Var1", "Var2", "Correlation")

      ggplot2::ggplot(melt_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), color = "#0f172a", size = 3.5) +
        ggplot2::scale_fill_gradient2(low = "#ef4444", mid = "#f8fafc", high = "#3b82f6", midpoint = 0, limits = c(-1, 1), name = "Pearson r") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = ggplot2::element_text(face = "bold"),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
    })

    # Multicollinearity Alert Table
    output$table_multicollinearity <- DT::renderDataTable({
      req(corr_r())
      m_df <- corr_r()$multicollinear_pairs

      if (nrow(m_df) == 0) {
        return(DT::datatable(
          data.frame(Status = "No strong multicollinearity (|r| \u2265 0.70) detected among numeric features."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Variable_A = m_df$variable_a,
        Variable_B = m_df$variable_b,
        Correlation = m_df$pearson_r,
        Strength = m_df$strength,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 6, dom = 'tp'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Scatter r badge
    output$scatter_r_badge <- renderUI({
      req(input$scatter_x, input$scatter_y, data_r())
      x <- data_r()[[input$scatter_x]]
      y <- data_r()[[input$scatter_y]]
      req(is.numeric(x), is.numeric(y))

      valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
      if (sum(valid) >= 4) {
        r_val <- round(stats::cor(x[valid], y[valid]), 3)
        color_class <- if (abs(r_val) >= 0.7) "badge bg-danger" else if (abs(r_val) >= 0.4) "badge bg-warning" else "badge bg-primary"
        div(class = "text-center", span(class = paste0(color_class, " p-2 fs-6"), paste0("r = ", r_val)))
      }
    })

    # Scatter Plot
    output$plot_scatter <- renderPlot({
      req(input$scatter_x, input$scatter_y, data_r())
      x <- data_r()[[input$scatter_x]]
      y <- data_r()[[input$scatter_y]]
      req(is.numeric(x), is.numeric(y))

      valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
      req(sum(valid) > 2)

      plot_df <- data.frame(X = x[valid], Y = y[valid])
      # Sample if too large for smooth plotting
      if (nrow(plot_df) > 5000) {
        plot_df <- plot_df[sample(nrow(plot_df), 5000), ]
      }

      ggplot2::ggplot(plot_df, ggplot2::aes(x = X, y = Y)) +
        ggplot2::geom_point(alpha = 0.5, color = "#2563eb", size = 2) +
        ggplot2::geom_smooth(method = "lm", color = "#dc2626", se = TRUE, linetype = "dashed") +
        ggplot2::labs(
          title = paste0(input$scatter_y, " vs ", input$scatter_x),
          x = input$scatter_x,
          y = input$scatter_y
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Full Pairwise Table
    output$table_pairs <- DT::renderDataTable({
      req(corr_r())
      p_df <- corr_r()$pairs

      if (nrow(p_df) == 0) {
        return(DT::datatable(
          data.frame(Status = "No correlation pairs computed."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Variable_A = p_df$variable_a,
        Variable_B = p_df$variable_b,
        Pearson_r = p_df$pearson_r,
        Strength = p_df$strength,
        Investigation_Note = p_df$investigation_note,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 10, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
