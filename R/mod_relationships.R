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
        class = "d-flex justify-content-between align-items-center mb-3 flex-wrap gap-2",
        h5(class = "panel-title mb-0", icon("circle-nodes"), " Visual Analytics & Relationship Explorer"),
        div(
          class = "d-flex align-items-center gap-2",
          span(class = "small text-muted fw-bold", "Presentation Mode:"),
          radioButtons(
            ns("visual_mode"),
            label = NULL,
            choices = c("2D View" = "2d", "3D Isometric View" = "3d"),
            selected = "2d",
            inline = TRUE
          )
        )
      ),
      div(
        class = "row align-items-center g-2",
        div(class = "col-md-4", selectInput(ns("var_x"), "Variable X (Horizontal):", choices = NULL, width = "100%")),
        div(class = "col-md-4", selectInput(ns("var_y"), "Variable Y (Vertical):", choices = NULL, width = "100%")),
        conditionalPanel(
          condition = sprintf("input['%s'] == '3d'", ns("visual_mode")),
          div(class = "col-md-4", selectInput(ns("var_z"), "Variable Z (Depth / Elevation):", choices = NULL, width = "100%"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == '2d'", ns("visual_mode")),
          div(class = "col-md-4", div(class = "mt-4", uiOutput(ns("pairing_type_badge"))))
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == '3d'", ns("visual_mode")),
        div(
          class = "row align-items-center mt-2 pt-2 border-top g-2",
          div(class = "col-md-6", sliderInput(ns("rot_azimuth"), "3D Horizontal Azimuth Angle (\u00b0):", min = 0, max = 360, value = 45, step = 5, width = "100%")),
          div(class = "col-md-6", sliderInput(ns("rot_elevation"), "3D Vertical Elevation Angle (\u00b0):", min = 10, max = 80, value = 30, step = 5, width = "100%"))
        )
      )
    ),

    # Main Visual & Relationship Description
    div(
      class = "row",
      div(
        class = "col-lg-8 col-md-12",
        div(
          class = "panel-box mb-3",
          plotOutput(ns("plot_relationship"), height = "400px")
        )
      ),
      div(
        class = "col-lg-4 col-md-12",
        div(
          class = "panel-box mb-3 h-100",
          h5(class = "panel-title", icon("circle-info"), " Visual Diagnostics & Summary"),
          uiOutput(ns("relationship_stats")),
          hr(),
          p(class = "small text-muted mb-0",
            strong("Statistical Disclaimer: "),
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

    # Update X, Y, and Z choices
    observe({
      req(data_r())
      cols <- names(data_r())
      num_cols <- cols[vapply(data_r(), is.numeric, logical(1))]

      sel_x <- if (length(num_cols) >= 1) num_cols[1] else cols[1]
      sel_y <- if (length(num_cols) >= 2) num_cols[2] else (if (length(cols) >= 2) cols[2] else cols[1])
      sel_z <- if (length(num_cols) >= 3) num_cols[3] else (if (length(cols) >= 3) cols[3] else cols[1])

      updateSelectInput(session, "var_x", choices = cols, selected = sel_x)
      updateSelectInput(session, "var_y", choices = cols, selected = sel_y)
      updateSelectInput(session, "var_z", choices = if (length(num_cols) > 0) num_cols else cols, selected = sel_z)
    })

    # Switch to numeric variables on 3D mode activation if needed
    observeEvent(input$visual_mode, {
      req(data_r())
      if (!is.null(input$visual_mode) && input$visual_mode == "3d") {
        cols <- names(data_r())
        num_cols <- cols[vapply(data_r(), is.numeric, logical(1))]
        if (length(num_cols) >= 3) {
          cur_x <- input$var_x
          cur_y <- input$var_y
          cur_z <- input$var_z
          if (!is.null(cur_x) && !is.null(cur_y) && !is.null(cur_z)) {
            if (!(cur_x %in% num_cols && cur_y %in% num_cols && cur_z %in% num_cols)) {
              updateSelectInput(session, "var_x", selected = num_cols[1])
              updateSelectInput(session, "var_y", selected = num_cols[2])
              updateSelectInput(session, "var_z", selected = num_cols[3])
            }
          }
        }
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
      span(class = "badge bg-primary text-white fs-6 p-2", txt)
    })

    # Main Plot (Supports both 2D and 3D Isometric View)
    output$plot_relationship <- renderPlot({
      req(input$var_x, input$var_y, data_r())
      df <- data_r()
      req(input$var_x %in% names(df), input$var_y %in% names(df))

      # ---- 3D Isometric Presentation Mode ----
      if (!is.null(input$visual_mode) && input$visual_mode == "3d") {
        req(input$var_z, input$var_z %in% names(df))
        theta <- if (!is.null(input$rot_azimuth)) as.numeric(input$rot_azimuth) else 45
        phi <- if (!is.null(input$rot_elevation)) as.numeric(input$rot_elevation) else 30
        return(project_3d_scatter(df, input$var_x, input$var_y, input$var_z, theta = theta, phi = phi))
      }

      # ---- 2D Bivariate Presentation Mode ----
      vx <- df[[input$var_x]]
      vy <- df[[input$var_y]]
      pt <- pairing_type()

      valid_idx <- which(!is.na(vx) & !is.na(vy))
      req(length(valid_idx) > 1)

      sub_x <- vx[valid_idx]
      sub_y <- vy[valid_idx]

      if (pt == "num_num") {
        p_df <- data.frame(X = as.numeric(sub_x), Y = as.numeric(sub_y))
        if (nrow(p_df) > 5000) p_df <- p_df[sample(nrow(p_df), 5000), ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = X, y = Y)) +
          ggplot2::geom_point(color = "#2563eb", alpha = 0.55, size = 2.2) +
          ggplot2::geom_smooth(method = "lm", color = "#ef4444", se = TRUE, linewidth = 0.8) +
          ggplot2::labs(title = paste0("2D Scatter: ", input$var_y, " vs ", input$var_x), x = input$var_x, y = input$var_y) +
          theme_detective()

      } else if (pt == "cat_num") {
        is_x_cat <- is.character(sub_x) || is.factor(sub_x) || is.logical(sub_x)
        cat_v <- if (is_x_cat) as.character(sub_x) else as.character(sub_y)
        num_v <- if (is_x_cat) as.numeric(sub_y) else as.numeric(sub_x)
        cat_name <- if (is_x_cat) input$var_x else input$var_y
        num_name <- if (is_x_cat) input$var_y else input$var_x

        p_df <- data.frame(Group = cat_v, Val = num_v)
        top_grps <- names(sort(table(p_df$Group), decreasing = TRUE))[1:min(10, length(unique(p_df$Group)))]
        p_df <- p_df[p_df$Group %in% top_grps, ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = Group, y = Val, fill = Group)) +
          ggplot2::geom_boxplot(alpha = 0.75, show.legend = FALSE, outlier.color = "#ef4444", outlier.alpha = 0.6) +
          ggplot2::scale_fill_brewer(palette = "Blues") +
          ggplot2::coord_flip() +
          ggplot2::labs(title = paste0("2D Boxplot: ", num_name, " by ", cat_name), x = cat_name, y = num_name) +
          theme_detective()

      } else if (pt == "cat_cat") {
        p_df <- data.frame(X = as.character(sub_x), Y = as.character(sub_y))
        top_x <- names(sort(table(p_df$X), decreasing = TRUE))[1:min(8, length(unique(p_df$X)))]
        top_y <- names(sort(table(p_df$Y), decreasing = TRUE))[1:min(5, length(unique(p_df$Y)))]
        p_df <- p_df[p_df$X %in% top_x & p_df$Y %in% top_y, ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = X, fill = Y)) +
          ggplot2::geom_bar(position = "dodge", alpha = 0.85) +
          ggplot2::scale_fill_brewer(palette = "Blues") +
          ggplot2::labs(title = paste0("2D Grouped Distribution: ", input$var_y, " across ", input$var_x), x = input$var_x, y = "Count", fill = input$var_y) +
          theme_detective()

      } else if (pt == "date_num") {
        p_df <- data.frame(Date = as.Date(sub_x), Val = as.numeric(sub_y))
        p_df <- p_df[order(p_df$Date), ]

        ggplot2::ggplot(p_df, ggplot2::aes(x = Date, y = Val)) +
          ggplot2::geom_line(color = "#0284c7", linewidth = 0.8) +
          ggplot2::geom_point(color = "#1e40af", size = 1.8, alpha = 0.7) +
          ggplot2::labs(title = paste0("2D Timeline: ", input$var_y, " over Date"), x = "Date", y = input$var_y) +
          theme_detective()
      } else {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "Select variable pairing to view distribution.", size = 4, color = "#94a3b8") +
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
