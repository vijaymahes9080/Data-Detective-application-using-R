# ==============================================================================
# DATA DETECTIVE: Statistical Data Quality & Exploratory Analysis Platform
# File: modules/mod_patterns.R
# Description: Pattern detection, target & leakage indicators, representation, group/date
# ==============================================================================

mod_patterns_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("pattern_tabs"),

      # ---- Subtab 1: Target Variable & Data Leakage -------------------------
      tabPanel(
        title = tagList(icon("bullseye"), " Target Analysis & Leakage Indicators"),
        div(
          class = "p-3",
          div(
            class = "panel-box mb-3",
            div(
              class = "row align-items-center",
              div(
                class = "col-md-5",
                selectInput(ns("select_target"), "Designate Optional Target / Outcome Variable:", choices = NULL, width = "100%")
              ),
              div(
                class = "col-md-7",
                div(class = "small text-muted",
                    "Selecting a target variable enables focused evaluation of feature-target redundancy and leakage heuristics.")
              )
            )
          ),
          div(
            class = "panel-box mb-3",
            div(
              class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
              h5(class = "panel-title mb-0", icon("shield-halved"), " Potential Data Leakage Indicators"),
              span(class = "badge bg-warning text-dark", "Exploratory Signal Only")
            ),
            p(class = "small text-muted",
              strong("Methodological Disclaimer: "),
              "Potential leakage indicators are investigation aids, not proof of leakage. ",
              "They highlight variables with near-identical distributions, perfect correlations, or naming resemblance. ",
              "Data Detective never states 'Data leakage confirmed'."
            ),
            DT::dataTableOutput(ns("table_leakage"))
          )
        )
      ),

      # ---- Subtab 2: Representation Indicators ------------------------------
      tabPanel(
        title = tagList(icon("scale-unbalanced"), " Potential Representation Indicators"),
        div(
          class = "p-3",
          div(
            class = "panel-box mb-3",
            div(
              class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
              h5(class = "panel-title mb-0", icon("scale-unbalanced-flip"), " Representation Diagnostics"),
              span(class = "badge bg-info text-white", "Descriptive Empirical Metrics")
            ),
            p(class = "small text-muted",
              strong("Important Ethical Notice: "),
              "This is NOT an ethical bias detector. It detects measurable statistical patterns such as severe category imbalance ",
              "(\u226590% in one group) or differential missingness rates (\u226530% divergence across sub-groups). ",
              "Wording used: 'Potential representation issue requiring further investigation'. Do not infer discrimination, unfairness, or intent."
            ),
            DT::dataTableOutput(ns("table_representation"))
          )
        )
      ),

      # ---- Subtab 3: Group Comparison ---------------------------------------
      tabPanel(
        title = tagList(icon("people-group"), " Group Comparisons"),
        div(
          class = "p-3",
          div(
            class = "panel-box mb-3",
            div(
              class = "row align-items-center mb-2",
              div(class = "col-md-5", selectInput(ns("group_var"), "Select Categorical Grouping Variable:", choices = NULL)),
              div(class = "col-md-5", selectInput(ns("group_num_var"), "Select Numeric Variable to Compare:", choices = NULL)),
              div(class = "col-md-2", div(class = "small text-muted mt-3", "Descriptive comparisons only; does not infer causality."))
            ),
            plotOutput(ns("plot_group_box"), height = "320px"),
            div(class = "mt-3", DT::dataTableOutput(ns("table_group_stats")))
          )
        )
      ),

      # ---- Subtab 4: Date / Time Analysis -----------------------------------
      tabPanel(
        title = tagList(icon("calendar-days"), " Temporal & Date Analysis"),
        div(
          class = "p-3",
          div(
            class = "panel-box mb-3",
            h5(class = "panel-title", icon("clock"), " Detected Date / Time Variables"),
            p(class = "small text-muted", "Identifies timeline bounds, day spans, and record distributions over time. Do not assume time-series meaning without domain confirmation."),
            DT::dataTableOutput(ns("table_dates"))
          ),
          div(
            class = "panel-box mt-3",
            h5(class = "panel-title", "Timeline Observation Frequency"),
            plotOutput(ns("plot_time_series"), height = "300px")
          )
        )
      )
    )
  )
}

mod_patterns_server <- function(id, data_r, profile_r) {
  moduleServer(id, function(input, output, session) {

    # Update Target Variable choices
    observe({
      req(data_r())
      cols <- c("(None Specified)" = "", names(data_r()))
      updateSelectInput(session, "select_target", choices = cols, selected = "")
    })

    # Update Grouping choices
    observe({
      req(data_r())
      cat_cols <- names(data_r())[vapply(data_r(), function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      num_cols <- names(data_r())[vapply(data_r(), function(x) is.numeric(x) && !is.logical(x), logical(1))]

      if (length(cat_cols) > 0) {
        updateSelectInput(session, "group_var", choices = cat_cols, selected = cat_cols[1])
      }
      if (length(num_cols) > 0) {
        updateSelectInput(session, "group_num_var", choices = num_cols, selected = num_cols[1])
      }
    })

    # Reactive Leakage calculation
    leakage_res <- reactive({
      req(data_r())
      target_sel <- input$select_target
      if (is.null(target_sel) || target_sel == "") {
        return(analyze_leakage_indicators(data_r(), target_col = NULL))
      }
      analyze_leakage_indicators(data_r(), target_col = target_sel)
    })

    # Leakage Table
    output$table_leakage <- DT::renderDataTable({
      req(leakage_res())
      res <- leakage_res()

      if (!res$target_specified) {
        return(DT::datatable(
          data.frame(Status = "Target variable not specified. Leakage analysis is limited. Please select a target above."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      if (nrow(res$indicators) == 0) {
        return(DT::datatable(
          data.frame(Status = "No overt leakage indicators detected relative to the selected target variable."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Feature = res$indicators$column,
        Indicator_Type = res$indicators$indicator_type,
        Severity = res$indicators$severity,
        Evidence = res$indicators$evidence,
        Investigation_Finding = res$indicators$finding,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 8, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Representation Table
    output$table_representation <- DT::renderDataTable({
      req(data_r())
      rep_res <- analyze_representation_indicators(data_r())

      if (!rep_res$has_representation_issues) {
        return(DT::datatable(
          data.frame(Status = "No severe representation disparities or differential missingness detected across categorical cohorts."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Variable = rep_res$indicators$column,
        Sub_Cohort = rep_res$indicators$category,
        Pattern_Type = rep_res$indicators$indicator_type,
        Severity = rep_res$indicators$severity,
        Evidence = rep_res$indicators$evidence,
        Finding = rep_res$indicators$finding,
        stringsAsFactors = FALSE
      )

      DT::datatable(
        disp_df,
        options = list(pageLength = 8, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Group comparison table
    group_stats_df <- reactive({
      req(input$group_var, input$group_num_var, data_r())
      compare_groups(data_r(), input$group_var, input$group_num_var)
    })

    output$table_group_stats <- DT::renderDataTable({
      req(group_stats_df())
      g_df <- group_stats_df()

      if (nrow(g_df) == 0) {
        return(DT::datatable(
          data.frame(Status = "Insufficient group data to render comparison table."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        g_df,
        options = list(pageLength = 8, dom = 'frtip'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Group comparison boxplot
    output$plot_group_box <- renderPlot({
      req(input$group_var, input$group_num_var, data_r())
      df <- data_r()
      g <- as.character(df[[input$group_var]])
      v <- df[[input$group_num_var]]

      valid <- !is.na(g) & trimws(g) != "" & !is.na(v) & is.finite(v)
      req(sum(valid) > 2)

      plot_df <- data.frame(Group = g[valid], Value = v[valid])
      # Cap to top 10 groups
      top_grps <- names(sort(table(plot_df$Group), decreasing = TRUE))[1:min(10, length(unique(plot_df$Group)))]
      plot_df <- plot_df[plot_df$Group %in% top_grps, ]

      ggplot2::ggplot(plot_df, ggplot2::aes(x = Group, y = Value, fill = Group)) +
        ggplot2::geom_boxplot(alpha = 0.7, show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste0(input$group_num_var, " across categories of ", input$group_var),
          x = input$group_var,
          y = input$group_num_var
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })

    # Date Analysis Table
    output$table_dates <- DT::renderDataTable({
      req(data_r())
      d_info <- analyze_dates(data_r())

      if (!d_info$has_dates) {
        return(DT::datatable(
          data.frame(Status = "No date or temporal columns detected in the active dataset."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      DT::datatable(
        d_info$summary,
        options = list(dom = 't'),
        rownames = FALSE,
        class = "compact stripe hover border-table"
      )
    })

    # Records over time plot
    output$plot_time_series <- renderPlot({
      req(data_r())
      d_info <- analyze_dates(data_r())
      if (!d_info$has_dates || nrow(d_info$summary) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 1, y = 1, label = "No temporal column available for timeline plot.", size = 4) +
                 ggplot2::theme_void())
      }

      date_col <- d_info$date_columns[1]
      dates <- as.Date(data_r()[[date_col]])
      valid_dates <- dates[!is.na(dates)]
      req(length(valid_dates) > 2)

      # Aggregate by month or day
      time_df <- as.data.frame(table(Date = valid_dates))
      time_df$Date <- as.Date(time_df$Date)
      time_df$Freq <- as.numeric(time_df$Freq)

      ggplot2::ggplot(time_df, ggplot2::aes(x = Date, y = Freq)) +
        ggplot2::geom_line(color = "#0284c7", linewidth = 1) +
        ggplot2::geom_area(fill = "#e0f2fe", alpha = 0.5) +
        ggplot2::labs(
          title = paste0("Daily Record Frequency over Time (", date_col, ")"),
          x = "Date",
          y = "Records Count"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
    })
  })
}
