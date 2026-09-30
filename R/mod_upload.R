# ==============================================================================
# DATA DETECTIVE: Automated Dataset Investigation Platform
# File: R/mod_upload.R
# Description: Module 1 — Data Upload, Ingestion, Delimiter Detection & Samples
# Shinylive / WebAssembly compatible
# ==============================================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "upload-container p-4 bg-white border rounded shadow-sm",
      div(
        class = "text-center mb-4",
        h3(class = "fw-bold text-primary", icon("cloud-arrow-up"), " Upload Your Dataset"),
        p(class = "text-muted", "Support for CSV, TSV, and plain tabular text. Local in-browser processing via Shinylive / R.")
      ),

      # File Input Control
      fileInput(
        ns("file"),
        label = tags$strong("Select Dataset File:"),
        accept = c(".csv", ".tsv", ".txt", ".rds"),
        width = "100%",
        placeholder = "Browse for .csv or .rds..."
      ),

      # Sample Datasets Selector
      div(
        class = "mt-3 pt-3 border-top",
        p(class = "small text-muted mb-2", tags$strong("Or test immediately with built-in benchmark datasets:")),
        div(
          class = "d-flex gap-2 flex-wrap",
          actionButton(ns("load_clean"), "Clean HR Sample", icon = icon("check"), class = "btn btn-sm btn-outline-success"),
          actionButton(ns("load_messy"), "Messy Anomaly Sample", icon = icon("triangle-exclamation"), class = "btn btn-sm btn-outline-warning"),
          actionButton(ns("load_sales"), "Realistic Sales Sample", icon = icon("chart-line"), class = "btn btn-sm btn-outline-primary")
        )
      ),

      # Upload Metadata & Validation Status Display
      uiOutput(ns("upload_status"))
    )
  )
}

mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploaded_df <- reactiveVal(NULL)
    metadata <- reactiveVal(list())
    validation_res <- reactiveVal(NULL)

    # Function to parse file safely
    parse_dataset <- function(path, name, size) {
      ext <- tolower(tools::file_ext(name))
      delim <- ","
      df <- NULL

      tryCatch({
        if (ext == "rds") {
          df <- readRDS(path)
        } else {
          delim <- detect_delimiter(path)
          # Use base R utils::read.table / read.csv for maximum Shinylive/webR stability
          df <- utils::read.csv(
            path,
            sep = delim,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            na.strings = c("", "NA", "N/A", "null", "NULL", "NaN")
          )
        }
      }, error = function(e) {
        df <<- NULL
      })

      if (is.null(df)) {
        validation_res(list(valid = FALSE, message = "Could not parse file. Please verify CSV encoding and formatting."))
        return()
      }

      val <- validate_dataset(df)
      if (!val$valid) {
        validation_res(list(valid = FALSE, message = paste(val$errors, collapse = " | ")))
        return()
      }

      # Repair column names if duplicated or empty
      orig_names <- names(df)
      clean_names <- orig_names
      empty_idx <- which(is.na(clean_names) | trimws(clean_names) == "")
      if (length(empty_idx) > 0) clean_names[empty_idx] <- paste0("V_", empty_idx)
      if (any(duplicated(clean_names))) clean_names <- make.unique(clean_names, sep = "_")
      names(df) <- clean_names

      meta <- list(
        filename = name,
        filesize = format_bytes(size),
        rows = nrow(df),
        cols = ncol(df),
        delimiter = delim,
        warnings = val$warnings
      )

      metadata(meta)
      validation_res(list(valid = TRUE, message = "Dataset parsed and validated successfully."))
      uploaded_df(df)
    }

    # Upload handler
    observeEvent(input$file, {
      req(input$file)
      f <- input$file
      parse_dataset(f$datapath, f$name, f$size)
    })

    # Sample dataset loaders
    observeEvent(input$load_clean, {
      p <- file.path("sample_data", "sample_clean.csv")
      if (file.exists(p)) parse_dataset(p, "sample_clean.csv", file.info(p)$size)
    })

    observeEvent(input$load_messy, {
      p <- file.path("sample_data", "sample_messy.csv")
      if (file.exists(p)) parse_dataset(p, "sample_messy.csv", file.info(p)$size)
    })

    observeEvent(input$load_sales, {
      p <- file.path("sample_data", "sample_sales.csv")
      if (file.exists(p)) parse_dataset(p, "sample_sales.csv", file.info(p)$size)
    })

    # Status UI Output
    output$upload_status <- renderUI({
      req(validation_res())
      v <- validation_res()
      m <- metadata()

      if (!v$valid) {
        div(class = "alert alert-danger mt-3", icon("triangle-exclamation"), strong(" Error: "), v$message)
      } else {
        div(
          class = "alert alert-success mt-3",
          div(class = "d-flex justify-content-between align-items-center mb-1",
              strong(paste0("\u2713 ", m$filename)),
              span(class = "badge bg-success", "Validated")),
          div(class = "small",
              paste0("Size: ", m$filesize, " | Rows: ", format_number(m$rows), " | Columns: ", format_number(m$cols), " | Delimiter: '", m$delimiter, "'")),
          if (length(m$warnings) > 0) div(class = "small text-warning mt-1", paste(m$warnings, collapse = " | "))
        )
      }
    })

    return(list(
      data = uploaded_df,
      metadata = metadata
    ))
  })
}
