# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_relationships.R
# Description: Module 9 — Variable Relationship Explorer (Auto Plot by Type)
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_relationships_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "panel-box mb-3",
      div(
        class = "row align-items-center",
        div(class = "col-md-5", selectInput(ns("var_x"), "Select Variable X (Independent / Horizontal):", choices = NULL, width = "100%")),
        div(class = "col-md-5", selectInput(ns("var_y"), "Select Variable Y (Dependent / Vertical):", choices = NULL, width = "100%")),
        div(class = "col-md-2", div(class = "mt-3", uiOutput(ns("pairing_type_badge"))))
      )
    ),

    # Main Visual & Relationship Description
    div(
      class = "row",
      div(
        class = "col-lg-8 col-md-12",
        div(class = "panel-box mb-3", plotOutput(ns("plot_relationship"), height = "380px"))
      ),
      div(
        class = "col-lg-4 col-md-12",
        div(
          class = "panel-box mb-3 h-100",
          h5(class = "panel-title", icon("circle-info"), " Relationship Summary"),
          uiOutput(ns("relationship_stats")),
          hr(),
          p(class = "small text-muted mb-0",
            strong("Interpretation Caution: "),
            "Visualized relationships show empirical joint distribution. They do not demonstrate direct causal mechanisms."
          )
        )
      )
    ),

    # Contingency or Group Data Table
    div(
      class = "panel-box",
      h5(class = "panel-title", icon("table"), " Tabular Relationship Data"),
      DT::dataTableOutput(ns("table_relationship_data"))
    )
  )
}

mod_relationships_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {

    # Update X and Y choices
    observe({
      req(data_r())
      cols <- names(data_r())
      if (length(cols) >= 2) {
        updateSelectInput(session, "var_x", choices = cols, selected = cols[1])
        updateSelectInput(session, "var_y", choices = cols, selected = cols[2])
      } else if (length(cols) == 1) {
        updateSelectInput(session, "var_x", choices = cols, selected = cols[1])
        updateSelectInput(session, "var_y", choices = cols, selected = cols[1])
      }
    })

    # Detect pair type
    pairing_type <- reactive({
      req(input$var_x, input$var_y, data_r())
      df <- data_r()
      req(input$var_x %in% names(df), input$var_y %in% names(df))

      tx <- detect_column_type(df[[input$var_x]])
      ty <- detect_column_type(df[[input$var_y]])

      is_num_x <- tx %in% c("numeric", "integer")
      is_num_y <- ty %in% c("numeric", "integer")
      is_date_x <- tx %in% c("date", "datetime", "date_candidate")
      is_date_y <- ty %in% c("date", "datetime", "date_candidate")
      is_cat_x <- tx %in% c("character", "factor", "logical")
      is_cat_y <- ty %in% c("character", "factor", "logical")

      if ((is_date_x && is_num_y) || (is_num_x && is_date_y)) {
        "date_num"
      } else if (is_num_x && is_num_y) {
        "num_num"
      } else if ((is_cat_x && is_num_y) || (is_num_x && is_cat_y)) {
        "cat_num"
      } else if (is_cat_x && is_cat_y) {
        "cat_cat"
      } else {
        "general"
      }
    })

    # Pairing badge
    output$pairing_type_badge <- renderUI({
      pt <- pairing_type()
      txt <- switch(
        pt,
        "num_num" = "Numeric vs Numeric (Scatter)",
        "cat_num" = "Categorical vs Numeric (Boxplot)",
        "cat_cat" = "Categorical vs Categorical (Bar)",
        "date_num" = "Date vs Numeric (Timeline)",
        "General Pair"
      )
      span(class = "badge bg-primary fs-6 p-2", txt)
    })

    # Main Plot
    output$plot_relationship <- renderPlot({
      req(input$var_x, input$var_y, data_r())
      df <- data_r()
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      vx <- df[[input$var_x]]
      vy <- df[[input$var_y]]
      pt <- pairing_type()

      valid_idx <- which(!is.na(vx) & !is.na(vy))
      req(length(valid_idx) > 1)

      sub_x <- vx[valid_idx]
      sub_y <- vy[valid_idx]

      if (pt == "num_num") {
        # Scatter plot
        p_df <- data.frame(X = as.numeric(sub_x), Y = as.numeric(sub_y))
        if (nrow(p_df) > 5000) p_df <- p_df[sample(nrow(p_df), 5000), ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = X, y = Y)) +
          ggplot2::geom_point(color = "#2563eb", alpha = 0.5, size = 2) +
          ggplot2::geom_smooth(method = "lm", color = "#ef4444", se = TRUE) +
          ggplot2::labs(title = paste0("Scatter: ", input$var_y, " vs ", input$var_x), x = input$var_x, y = input$var_y) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

      } else if (pt == "cat_num") {
        # Boxplot
        is_x_cat <- is.character(sub_x) || is.factor(sub_x) || is.logical(sub_x)
        cat_v <- if (is_x_cat) as.character(sub_x) else as.character(sub_y)
        num_v <- if (is_x_cat) as.numeric(sub_y) else as.numeric(sub_x)
        cat_name <- if (is_x_cat) input$var_x else input$var_y
        num_name <- if (is_x_cat) input$var_y else input$var_x

        p_df <- data.frame(Group = cat_v, Val = num_v)
        top_grps <- names(sort(table(p_df$Group), decreasing = TRUE))[1:min(10, length(unique(p_df$Group)))]
        p_df <- p_df[p_df$Group %in% top_grps, ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = Group, y = Val, fill = Group)) +
          ggplot2::geom_boxplot(alpha = 0.7, show.legend = FALSE) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = paste0("Boxplot: ", num_name, " by ", cat_name), x = cat_name, y = num_name) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

      } else if (pt == "cat_cat") {
        # Grouped Bar
        p_df <- data.frame(X = as.character(sub_x), Y = as.character(sub_y))
        top_x <- names(sort(table(p_df$X), decreasing = TRUE))[1:min(8, length(unique(p_df$X)))]
        top_y <- names(sort(table(p_df$Y), decreasing = TRUE))[1:min(5, length(unique(p_df$Y)))]
        p_df <- p_df[p_df$X %in% top_x & p_df$Y %in% top_y, ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = X, fill = Y)) +
          ggplot2::geom_bar(position = "dodge") +
          ggplot2::labs(title = paste0("Grouped Bar: ", input$var_y, " across ", input$var_x), x = input$var_x, y = "Count", fill = input$var_y) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

      } else if (pt == "date_num") {
        # Line Chart over time
        p_df <- data.frame(Date = as.Date(sub_x), Val = as.numeric(sub_y))
        p_df <- p_df[order(p_df$Date), ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = Date, y = Val)) +
          ggplot2::geom_line(color = "#0284c7") +
          ggplot2::geom_point(color = "#0369a1", size = 1.5, alpha = 0.6) +
          ggplot2::labs(title = paste0("Timeline: ", input$var_y, " over Date"), x = "Date", y = input$var_y) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))
      } else {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "Select variable pairing to view distribution.", size = 4) +
          ggplot2::theme_void()
      }
    })

    # Stats Summary UI
    output$relationship_stats <- renderUI({
      req(input$var_x, input$var_y, data_r())
      df <- data_r()
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      vx <- df[[input$var_x]]
      vy <- df[[input$var_y]]
      pt <- pairing_type()

      if (pt == "num_num") {
        clean_idx <- which(!is.na(vx) & !is.na(vy) & is.finite(vx) & is.finite(vy))
        r <- if (length(clean_idx) >= 4) round(stats::cor(vx[clean_idx], vy[clean_idx]), 3) else NA
        tagList(
          p(strong("Pearson r: "), ifelse(is.na(r), "N/A", as.character(r))),
          p(strong("Complete Observation Pairs: "), format_number(length(clean_idx))),
          p(strong("Linear Fit: "), ifelse(!is.na(r) && abs(r) >= 0.7, "Strong linear relationship", "Moderate/Weak relationship"))
        )
      } else if (pt == "cat_num") {
        tagList(
          p(strong("Type: "), "Category-stratified numeric values"),
          p(strong("Observations: "), format_number(sum(!is.na(vx) & !is.na(vy))))
        )
      } else {
        tagList(
          p(strong("Pairing Type: "), pt),
          p(strong("Non-missing joint pairs: "), format_number(sum(!is.na(vx) & !is.na(vy))))
        )
      }
    })

    # Relationship Table
    output$table_relationship_data <- DT::renderDataTable({
      req(input$var_x, input$var_y, data_r())
      df <- data_r()
      req(input$var_x %in% names(df), input$var_y %in% names(df))
      vx <- df[[input$var_x]]
      vy <- df[[input$var_y]]
      pt <- pairing_type()

      if (pt == "cat_cat") {
        # Contingency table
        tab <- table(X = as.character(vx), Y = as.character(vy))
        tab_df <- as.data.frame.matrix(tab)
        tab_df <- cbind(Category_X = rownames(tab_df), tab_df)
        rownames(tab_df) <- NULL

        DT::datatable(
          tab_df,
          options = list(pageLength = 8, dom = 't'),
          rownames = FALSE,
          class = "compact stripe hover border-table"
        )
      } else {
        sub_df <- data.frame(X = vx, Y = vy)
        names(sub_df) <- c(input$var_x, input$var_y)
        sub_df <- sub_df[!is.na(sub_df[[1]]) & !is.na(sub_df[[2]]), ]

        DT::datatable(
          utils::head(sub_df, 50),
          options = list(pageLength = 8, dom = 'tp'),
          rownames = TRUE,
          class = "compact stripe hover border-table"
        )
      }
    })
  })
}
