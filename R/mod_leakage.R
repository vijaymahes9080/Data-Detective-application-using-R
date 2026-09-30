# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_leakage.R
# Description: Module 10 — Data Leakage Indicators & Target Association Screening
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_leakage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "panel-box mb-3",
      div(
        class = "row align-items-center",
        div(
          class = "col-md-5",
          selectInput(ns("target_var"), "Designate Optional Target / Outcome Variable:", choices = NULL, width = "100%")
        ),
        div(
          class = "col-md-7",
          div(class = "small text-muted",
              "Selecting a target variable enables screening for feature-target collinearity, near-identity, and post-event information leakage.")
        )
      )
    ),

    # Notice & Indicators Table
    div(
      class = "panel-box",
      div(
        class = "d-flex justify-content-between align-items-center mb-2 flex-wrap",
        h5(class = "panel-title mb-0", icon("shield-halved"), " Potential Data Leakage Indicators"),
        span(class = "badge bg-warning text-dark", "Exploratory Signal Only")
      ),
      p(class = "small text-muted mb-3",
        strong("Methodological Disclaimer: "),
        "This module identifies exploratory indicators, never proof of leakage. ",
        "Features that embed target naming, share near-identical rows, or exhibit |r| \u2265 0.95 may cause artificial model optimism."
      ),
      DT::dataTableOutput(ns("table_leakage"))
    )
  )
}

mod_leakage_server <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(data_r())
      cols <- c("(No Target Designated)" = "", names(data_r()))
      updateSelectInput(session, "target_var", choices = cols, selected = "")
    })

    leakage_obj <- reactive({
      req(data_r())
      t_sel <- input$target_var
      if (is.null(t_sel) || t_sel == "" || !(t_sel %in% names(data_r()))) return(list(target_specified = FALSE, indicators = data.frame()))
      detect_leakage_indicators(data_r(), target_col = t_sel)
    })

    output$table_leakage <- DT::renderDataTable({
      req(leakage_obj())
      lo <- leakage_obj()

      if (!lo$target_specified) {
        return(DT::datatable(
          data.frame(Status = "Target variable not designated. Please designate an outcome variable above to activate leakage screening."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      if (nrow(lo$indicators) == 0) {
        return(DT::datatable(
          data.frame(Status = "No overt leakage indicators detected relative to the selected target variable."),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      disp_df <- data.frame(
        Feature = lo$indicators$column,
        Indicator_Type = lo$indicators$indicator_type,
        Severity = lo$indicators$severity,
        Evidence = lo$indicators$evidence,
        Investigation_Reason = lo$indicators$reason,
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
