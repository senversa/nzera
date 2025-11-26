# app/shiny_app.R
#
# Minimal Shiny app for interacting with nzera models.
#
# Assumes the nzera package is installed or this project
# is loaded (e.g. via devtools::load_all()).

library(shiny)
library(DT)
library(nzera)

ui <- fluidPage(
  titlePanel("Risk Modelling Demo"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "model_id",
        "Select model:",
        choices = NULL, # populated in server
        width = "100%"
      ),
      uiOutput("model_info"),
      fileInput(
        "data_file",
        "Upload input data (CSV):",
        accept = c(".csv"),
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      actionButton("run_btn", "Run model", class = "btn-primary"),
      hr(),
      verbatimTextOutput("run_status"),
      helpText(
        "This interface uses model metadata and schemas defined in the ",
        "nzera package. Uploaded data are validated against the ",
        "selected model's requirements before execution."
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Schema",
          h4("Input schema"),
          DTOutput("schema_table")
        ),
        tabPanel(
          "Results",
          h4("Model output"),
          DTOutput("results_table")
        ),
        tabPanel(
          "Raw Input",
          h4("Uploaded data"),
          DTOutput("input_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # --- 1. Populate model list from framework -------------------------------

  all_models <- reactive({
    m <- nzera::list_models()
    # Optionally filter out deprecated models
    m[is.na(m$status) | m$status != "deprecated", , drop = FALSE]
  })

  observe({
    m <- all_models()
    updateSelectInput(
      session,
      "model_id",
      choices = setNames(m$id, paste0(m$id, " - ", m$title)),
      selected = if (nrow(m) > 0) m$id[1] else NULL
    )
  })

  # --- 2. Metadata and schema display --------------------------------------

  model_meta <- reactive({
    req(input$model_id)
    get_model_metadata(input$model_id)
  })

  model_schema <- reactive({
    req(input$model_id)
    get_model_schema(input$model_id)
  })

  output$model_info <- renderUI({
    meta <- model_meta()
    tagList(
      strong("Model ID: "),
      meta$id,
      br(),
      strong("Title: "),
      meta$title,
      br(),
      strong("Version: "),
      meta$version,
      br(),
      strong("Status: "),
      meta$status,
      br(),
      strong("Owners: "),
      paste(meta$owners, collapse = ", ")
    )
  })

  output$schema_table <- renderDT({
    DT::datatable(
      model_schema(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # --- 3. Handle file upload ------------------------------------------------

  uploaded_data <- reactive({
    file <- input$data_file
    if (is.null(file)) {
      return(NULL)
    }

    df <- tryCatch(
      {
        readr::read_csv(file$datapath, show_col_types = FALSE)
      },
      error = function(e) {
        showNotification(paste("Error reading CSV:", e$message), type = "error")
        return(NULL)
      }
    )
    df
  })

  output$input_table <- renderDT({
    df <- uploaded_data()
    if (is.null(df)) {
      return(NULL)
    }
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  # --- 4. Run model with validation ----------------------------------------

  model_results <- eventReactive(input$run_btn, {
    req(input$model_id)
    df <- uploaded_data()
    if (is.null(df)) {
      showNotification(
        "Please upload a CSV file before running the model.",
        type = "warning"
      )
      return(NULL)
    }

    schema <- model_schema()

    # Validate input
    ok <- tryCatch(
      {
        validate_input(df, schema)
        TRUE
      },
      error = function(e) {
        showNotification(
          paste("Input validation failed:", e$message),
          type = "error",
          duration = 10
        )
        FALSE
      }
    )

    if (!ok) {
      return(NULL)
    }

    # Run model
    res <- tryCatch(
      {
        run_model(input$model_id, df)
      },
      error = function(e) {
        showNotification(
          paste("Model run failed:", e$message),
          type = "error",
          duration = 10
        )
        NULL
      }
    )

    res
  })

  output$run_status <- renderText({
    req(input$run_btn)
    if (is.null(model_results())) {
      "Model run failed or no results available."
    } else {
      "Model executed successfully."
    }
  })

  output$results_table <- renderDT({
    res <- model_results()
    if (is.null(res)) {
      return(NULL)
    }
    DT::datatable(res, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)
