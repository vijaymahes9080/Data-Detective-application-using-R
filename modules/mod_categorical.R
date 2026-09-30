# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_categorical.R
# Description: Categorical variable analysis, frequency bars, concentration diagnostics
# ==============================================================================

mod_categorical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "p-3",
      # Categorical Summary Table across all columns
      div(
        class = "panel-box mb-3",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap mb-2",
          h5(class = "panel-title mb-0", icon("tags"), " Categorical Variables Overview"),
          span(class = "badge bg-secondary", "Concentration Threshold: \u2265 90%")
        ),
        p(class = "small text-muted mb-2",
          strong("Important: "),
          "Heavy concentration in one class is termed 'Category concentration'. It is an empirical descriptor and is not automatically labeled as bias."
        ),
        DT::dataTableOutput(ns("table_cat_summary"))
      ),

      # Variable Drilldown Section
      div(
        class = "panel-box mt-3",
        h5(class = "panel-title", icon("chart-column"), " Category Frequency & Distribution Drilldown"),
        div(
          class = "row align-items-center mb-3",
          div(
            class = "col-md-5",
            selectInput(ns("select_cat_var"), "Select Categorical Variable:", choices = NULL, width = "100%")
          ),
          div(
            class = "col-md-7",
            uiOutput(ns("cat_concentration_alert"))
          )
        ),
        div(
          class = "row",
          div(
            class = "col-lg-7 col-md-12",
            div(class = "plot-container", plotOutput(ns("plot_cat_bars"), height = "360px"))
          ),
          div(
            class = "col-lg-5 col-md-12",
            div(
              class = "table-container",
              h6("Class Frequencies & Percentages:"),
              DT::dataTableOutput(ns("table_cat_freqs"))
            )
          )
        )
      )
    )
  )
}

mod_categorical_server <- function(id, data_r, cat_r) {
  moduleServer(id, function(input, output, session) {

    # Update categorical select choices
    observe({
      req(cat_r())
      if (cat_r()$has_categorical && length(cat_r()$categorical_columns) > 0) {
        choices <- cat_r()$categorical_columns
        current <- input$select_cat_var
        selected <- if (!is.null(current) && current %in% choices) current else choices[1]
        updateSelectInput(session, "select_cat_var", choices = choices, selected = selected)
      } else {
        updateSelectInput(session, "select_cat_var", choices = character(0))
      }
    })

    # Summary table across all categorical variables
    output$table_cat_summary <- DT::renderDataTable({
      req(cat_r())
      s_df <- cat_r()$summary

      if (nrow(s_df) == 0) {
        return(DT::datatable(
          data.frame(Status = "No categorical (text, factor, logical) variables found in dataset."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Column = s_df$column,
        Categories = s_df$n_categories,
        Dominant_Class = s_df$dominant_category,
        Dominant_Pct = paste0(s_df$dominant_pct, "% (", format_number(s_df$dominant_count), ")"),
        Rare_Classes = s_df$rare_categories_count,
        Concentration_Flag = ifelse(s_df$has_concentration, "High Concentration (\u226590%)", "Balanced/Moderate"),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 10, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Concentration alert banner
    output$cat_concentration_alert <- renderUI({
      req(input$select_cat_var, cat_r())
      s_df <- cat_r()$summary
      row <- s_df[s_df$column == input$select_cat_var, ]

      if (nrow(row) > 0 && row$has_concentration) {
        div(
          class = "alert alert-warning mb-0 p-2 small",
          icon("triangle-exclamation"),
          strong(" Category Concentration Detected: "),
          paste0("The class '", row$dominant_category, "' constitutes ", row$dominant_pct, "% of all records in '", input$select_cat_var, "'.")
        )
      } else if (nrow(row) > 0) {
        div(
          class = "alert alert-success mb-0 p-2 small",
          icon("check-circle"),
          paste0("Moderate class distribution across ", row$n_categories, " unique categories. Dominant class: '", row$dominant_category, "' (", row$dominant_pct, "%).")
        )
      }
    })

    # Bar chart of categories
    output$plot_cat_bars <- renderPlot({
      req(input$select_cat_var, data_r())
      freq_df <- get_category_frequencies(data_r(), input$select_cat_var, max_categories = 15)
      req(nrow(freq_df) > 0)

      freq_df$Category <- factor(freq_df$Category, levels = rev(freq_df$Category))

      ggplot2::ggplot(freq_df, ggplot2::aes(x = Category, y = Frequency)) +
        ggplot2::geom_col(fill = "#6366f1", width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(Percentage, "%")), hjust = -0.15, size = 3.5, color = "#374151") +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
        ggplot2::labs(
          title = paste0("Class Frequencies: ", input$select_cat_var),
          x = "",
          y = "Count"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 12),
          panel.grid.minor = ggplot2::element_blank()
        )
    })

    # Detailed frequency table
    output$table_cat_freqs <- DT::renderDataTable({
      req(input$select_cat_var, data_r())
      freq_df <- get_category_frequencies(data_r(), input$select_cat_var, max_categories = 50)
      req(nrow(freq_df) > 0)

      disp_df <- data.frame(
        Category = freq_df$Category,
        Count = format_number(freq_df$Frequency),
        Percentage = paste0(freq_df$Percentage, "%"),
        Cumulative = paste0(freq_df$Cumulative_Pct, "%"),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 8, dom = 'tp'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })
  })
}
