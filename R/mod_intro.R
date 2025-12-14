# INTRODUCTION MODULE
# Setup wizard and help documentation for the Document Classification System

#' Introduction Module UI
#'
#' Generates the user interface for the introduction/setup tab.
#' Shows upload interface if files missing, or data overview if ready.
#'
#' @param id Module namespace ID
#' @return Shiny UI
#' @export
mod_intro_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    load_app_css(),

    # Header
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   color: white; padding: 40px 30px; border-radius: 10px;
                   margin-bottom: 30px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          shiny::h1(
            shiny::icon("layer-group"),
            " Document Classification System",
            style = "margin: 0 0 15px 0; font-weight: 600;"
          ),
          shiny::p(
            "A powerful multi-schema document tagging and analysis platform",
            style = "font-size: 18px; margin: 0; opacity: 0.95;"
          )
        )
      )
    ),

    # Dynamic content area
    shiny::uiOutput(ns("main_content"))
  )
}

#' Introduction Module Server
#'
#' Server logic for the introduction module.
#'
#' @param id Module namespace ID
#' @param .dir Reactive or static path to project directory
#' @export
mod_intro_server <- function(id, .dir) {
  shiny::moduleServer(id, function(input, output, session) {

    # Make input reactive if not already
    dir_r <- if (shiny::is.reactive(.dir)) .dir else shiny::reactive(.dir)

    # Check project setup status
    setup_status <- shiny::reactive({
      check_project_setup(dir_r())
    })

    # Reactive trigger for refresh
    refresh_trigger <- shiny::reactiveVal(0)

    # Main content switcher
    output$main_content <- shiny::renderUI({
      # Force refresh
      refresh_trigger()

      status <- setup_status()
      current_dir <- dir_r()

      if (status$is_ready) {
        render_overview_interface(session$ns, current_dir)
      } else {
        render_setup_interface(session$ns, status, current_dir)
      }
    })

    # Handle document upload
    shiny::observeEvent(input$upload_documents, {
      shiny::req(input$upload_documents)

      file_info <- input$upload_documents
      target_path <- file.path(dir_r(), "Documents.parquet")

      tryCatch({
        # Copy uploaded file to project directory
        file.copy(file_info$datapath, target_path, overwrite = TRUE)

        # Validate the file
        validate_documents_file(target_path)

        shiny::showNotification(
          "Documents.parquet uploaded successfully!",
          type = "message",
          duration = 3
        )

        # Refresh display
        refresh_trigger(refresh_trigger() + 1)

      }, error = function(e) {
        shiny::showNotification(
          paste("Upload failed:", e$message),
          type = "error",
          duration = 5
        )

        # Clean up invalid file
        if (file.exists(target_path)) {
          file.remove(target_path)
        }
      })
    })

    # Handle schema upload
    shiny::observeEvent(input$upload_schema, {
      shiny::req(input$upload_schema)

      file_info <- input$upload_schema
      target_path <- file.path(dir_r(), "Schema.csv")

      tryCatch({
        # Copy uploaded file to project directory
        file.copy(file_info$datapath, target_path, overwrite = TRUE)

        # Validate the schema
        validate_schema_file(dir_r())

        shiny::showNotification(
          "Schema.csv uploaded successfully!",
          type = "message",
          duration = 3
        )

        # Refresh display
        refresh_trigger(refresh_trigger() + 1)

      }, error = function(e) {
        shiny::showNotification(
          paste("Upload failed:", e$message),
          type = "error",
          duration = 5
        )

        # Clean up invalid file
        if (file.exists(target_path)) {
          file.remove(target_path)
        }
      })
    })

    # Refresh button
    shiny::observeEvent(input$refresh_status, {
      refresh_trigger(refresh_trigger() + 1)
      shiny::showNotification("Status refreshed", type = "message", duration = 2)
    })
  })
}

# ===== UI RENDERING FUNCTIONS =====

#' Render Setup Interface
#'
#' Shows file upload interface when project is not ready.
#'
#' @param ns Namespace function
#' @param status Setup status list
#' @param dir_path Directory path (character)
#' @return Shiny UI
#' @keywords internal
render_setup_interface <- function(ns, status, dir_path) {
  shiny::tagList(
    # Setup instructions
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "well",
          style = "background-color: #fff3cd; border-left: 4px solid #ffc107;",
          shiny::h3(
            shiny::icon("exclamation-triangle"),
            " Project Setup Required",
            style = "color: #856404; margin-top: 0;"
          ),
          shiny::p(
            "To get started, please upload your project files:",
            style = "font-size: 15px; color: #856404;"
          )
        )
      )
    ),

    # Upload panels
    shiny::fluidRow(
      # Documents upload
      shiny::column(
        6,
        shiny::wellPanel(
          style = if (status$has_documents) "border: 2px solid #28a745;" else "border: 2px solid #dc3545;",
          shiny::div(
            style = "text-align: center;",
            shiny::h4(
              if (status$has_documents) shiny::icon("check-circle", style = "color: #28a745;") else shiny::icon("times-circle", style = "color: #dc3545;"),
              " Documents",
              style = "margin-top: 0;"
            ),
            if (status$has_documents) {
              shiny::div(
                shiny::p(
                  shiny::icon("check"),
                  " Documents.parquet found",
                  style = "color: #28a745; font-weight: 500;"
                ),
                shiny::p(
                  paste(get_document_count(dir_path), "documents loaded"),
                  style = "color: #666; font-size: 13px;"
                )
              )
            } else {
              shiny::div(
                shiny::p(
                  "Upload your Documents.parquet file",
                  style = "color: #666; margin-bottom: 20px;"
                ),
                shiny::fileInput(
                  ns("upload_documents"),
                  label = NULL,
                  accept = ".parquet",
                  buttonLabel = "Choose File",
                  placeholder = "No file selected"
                ),
                shiny::div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: left;",
                  shiny::strong("Required format:"), shiny::br(),
                  "- Parquet file", shiny::br(),
                  "- Columns: DocID, HTML", shiny::br(),
                  "- No duplicate DocIDs"
                )
              )
            }
          )
        )
      ),

      # Schema upload
      shiny::column(
        6,
        shiny::wellPanel(
          style = if (status$has_schema) "border: 2px solid #28a745;" else "border: 2px solid #dc3545;",
          shiny::div(
            style = "text-align: center;",
            shiny::h4(
              if (status$has_schema) shiny::icon("check-circle", style = "color: #28a745;") else shiny::icon("times-circle", style = "color: #dc3545;"),
              " Schema",
              style = "margin-top: 0;"
            ),
            if (status$has_schema) {
              shiny::div(
                shiny::p(
                  shiny::icon("check"),
                  " Schema.csv found",
                  style = "color: #28a745; font-weight: 500;"
                ),
                shiny::p(
                  paste(length(read_schema(dir_path)), "schemas configured"),
                  style = "color: #666; font-size: 13px;"
                )
              )
            } else {
              shiny::div(
                shiny::p(
                  "Upload your Schema.csv file",
                  style = "color: #666; margin-bottom: 20px;"
                ),
                shiny::fileInput(
                  ns("upload_schema"),
                  label = NULL,
                  accept = c(".csv", "text/csv"),
                  buttonLabel = "Choose File",
                  placeholder = "No file selected"
                ),
                shiny::div(
                  style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; text-align: left;",
                  shiny::strong("Required format:"), shiny::br(),
                  "- CSV file", shiny::br(),
                  "- Columns: Schema, SchemaName, Class, Value", shiny::br(),
                  "- At least one schema defined"
                )
              )
            }
          )
        )
      )
    ),

    # Refresh button
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          style = "text-align: center; margin-top: 20px;",
          shiny::actionButton(
            ns("refresh_status"),
            "Refresh Status",
            icon = shiny::icon("sync"),
            class = "btn-primary btn-lg"
          )
        )
      )
    ),

    # Help section
    render_help_section()
  )
}

#' Render Overview Interface
#'
#' Shows project overview and schema when files are ready.
#'
#' @param ns Namespace function
#' @param dir_path Directory path (character)
#' @return Shiny UI
#' @keywords internal
render_overview_interface <- function(ns, dir_path) {
  # Load schema and data
  schema <- read_schema(dir_path)
  progress <- get_progress_stats(dir_path)

  shiny::tagList(
    # Success banner
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "well",
          style = "background-color: #d4edda; border-left: 4px solid #28a745;",
          shiny::h3(
            shiny::icon("check-circle"),
            " Project Ready!",
            style = "color: #155724; margin-top: 0;"
          ),
          shiny::p(
            "All required files are in place. You can start classifying documents using the tabs above.",
            style = "font-size: 15px; color: #155724; margin-bottom: 0;"
          )
        )
      )
    ),

    # Quick stats
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::div(
          class = "metric-box metric-total",
          shiny::h3(format(progress$total_documents, big.mark = ","), style = "margin: 0;"),
          shiny::p("Total Documents", style = "margin: 5px 0 0 0; color: #666;")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box metric-classified",
          shiny::h3(format(progress$classified_documents, big.mark = ","), style = "margin: 0;"),
          shiny::p("Classified", style = "margin: 5px 0 0 0; color: #666;")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box",
          style = "border-left: 4px solid #17a2b8;",
          shiny::h3(length(schema), style = "margin: 0;"),
          shiny::p("Schemas", style = "margin: 5px 0 0 0; color: #666;")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box metric-percent",
          shiny::h3(paste0(progress$percentage_complete, "%"), style = "margin: 0;"),
          shiny::p("Complete", style = "margin: 5px 0 0 0; color: #666;")
        )
      )
    ),

    # Schema display
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h3("Classification Schemas", style = "margin-top: 0;"),
          render_schema_display(schema)
        )
      )
    ),

    # Help section
    render_help_section()
  )
}

#' Render Schema Display
#'
#' Creates nicely formatted display of schema structure.
#'
#' @param schema Schema list
#' @return Shiny UI
#' @keywords internal
render_schema_display <- function(schema) {
  schema_panels <- lapply(names(schema), function(schema_id) {
    sch <- schema[[schema_id]]

    # Count total values across all classes
    total_values <- sum(sapply(sch$classes, length))

    shiny::div(
      class = "well",
      style = "background-color: #f8f9fa; margin-bottom: 15px;",

      # Schema header
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;
                 margin-bottom: 15px; padding-bottom: 10px; border-bottom: 2px solid #dee2e6;",
        shiny::div(
          shiny::h4(
            shiny::icon("layer-group", style = "color: #3498db;"),
            " ", sch$name,
            style = "margin: 0; display: inline-block;"
          ),
          shiny::span(
            paste("Schema", schema_id),
            style = "color: #666; font-size: 14px; margin-left: 10px;"
          )
        ),
        shiny::div(
          shiny::span(
            paste(length(sch$classes), "classes,", total_values, "values"),
            style = "color: #666; font-size: 14px;"
          )
        )
      ),

      # Classes
      shiny::div(
        style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 15px;",
        lapply(names(sch$classes), function(class_name) {
          values <- sch$classes[[class_name]]

          shiny::div(
            style = "background-color: white; padding: 15px; border-radius: 5px;
                     border-left: 3px solid #3498db;",
            shiny::h5(
              shiny::icon("tags"),
              " ", class_name,
              style = "margin: 0 0 10px 0; color: #2c3e50;"
            ),
            shiny::div(
              style = "max-height: 150px; overflow-y: auto;",
              shiny::tags$ul(
                style = "margin: 0; padding-left: 20px; list-style-type: disc;",
                lapply(values, function(v) {
                  shiny::tags$li(
                    v,
                    style = "font-size: 13px; color: #666; margin-bottom: 5px;"
                  )
                })
              )
            )
          )
        })
      )
    )
  })

  shiny::tagList(schema_panels)
}

#' Render Help Section
#'
#' Creates comprehensive help documentation.
#'
#' @return Shiny UI
#' @keywords internal
render_help_section <- function() {
  shiny::fluidRow(
    shiny::column(
      12,
      shiny::wellPanel(
        shiny::h3(
          shiny::icon("question-circle"),
          " How to Use This System",
          style = "margin-top: 0; color: #2c3e50;"
        ),

        shiny::div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px;",

          # Classification Tab
          shiny::div(
            class = "well",
            style = "background-color: #e3f2fd;",
            shiny::h4(
              shiny::icon("check-square", style = "color: #1976d2;"),
              " Classification",
              style = "margin-top: 0; color: #1976d2;"
            ),
            shiny::tags$ul(
              style = "margin: 0; padding-left: 20px;",
              shiny::tags$li("Main workflow for tagging documents"),
              shiny::tags$li("Filter by All, Classified, Unclassified, or Marked documents"),
              shiny::tags$li("Switch between Single and Multi-select modes"),
              shiny::tags$li("Navigate with Prev/Next buttons or search by DocID"),
              shiny::tags$li("Add notes to any document"),
              shiny::tags$li("Save classifications at any time (no validation blocking)")
            )
          ),

          # Overview Tab
          shiny::div(
            class = "well",
            style = "background-color: #fff3e0;",
            shiny::h4(
              shiny::icon("chart-bar", style = "color: #f57c00;"),
              " Overview",
              style = "margin-top: 0; color: #f57c00;"
            ),
            shiny::tags$ul(
              style = "margin: 0; padding-left: 20px;",
              shiny::tags$li("View key metrics and progress statistics"),
              shiny::tags$li("Analyze classification distributions by schema"),
              shiny::tags$li("Track user activity and contributions"),
              shiny::tags$li("View classification timeline"),
              shiny::tags$li("Refresh data anytime with the Refresh button")
            )
          ),

          # Browser Tab
          shiny::div(
            class = "well",
            style = "background-color: #e8f5e9;",
            shiny::h4(
              shiny::icon("search", style = "color: #388e3c;"),
              " Browser",
              style = "margin-top: 0; color: #388e3c;"
            ),
            shiny::tags$ul(
              style = "margin: 0; padding-left: 20px;",
              shiny::tags$li("Advanced filtering by Schema, Class, and Value"),
              shiny::tags$li("Filter documents with notes"),
              shiny::tags$li("Mark/unmark documents for batch operations"),
              shiny::tags$li("Read-only document viewer with classifications"),
              shiny::tags$li("Search within filtered results")
            )
          ),

          # Export Tab
          shiny::div(
            class = "well",
            style = "background-color: #f3e5f5;",
            shiny::h4(
              shiny::icon("download", style = "color: #7b1fa2;"),
              " Export Data",
              style = "margin-top: 0; color: #7b1fa2;"
            ),
            shiny::tags$ul(
              style = "margin: 0; padding-left: 20px;",
              shiny::tags$li("Export current state or full history"),
              shiny::tags$li("Choose CSV or Parquet format"),
              shiny::tags$li("Include or exclude notes from export"),
              shiny::tags$li("Use timeline slider to view historical states"),
              shiny::tags$li("Manage exported files")
            )
          )
        ),

        # Key features
        shiny::hr(),
        shiny::h4("Key Features", style = "color: #2c3e50;"),
        shiny::div(
          style = "background-color: #eceff1; padding: 20px; border-radius: 5px;",
          shiny::div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 15px;",

            shiny::div(
              shiny::h5(
                shiny::icon("layer-group", style = "color: #3498db;"),
                " Multi-Schema Support"
              ),
              shiny::p(
                "Classify documents using multiple independent schemas simultaneously.",
                style = "font-size: 13px; color: #666; margin: 0;"
              )
            ),

            shiny::div(
              shiny::h5(
                shiny::icon("history", style = "color: #9c27b0;"),
                " Complete Audit Trail"
              ),
              shiny::p(
                "SQLite append-only logging tracks every change with timestamps and user IDs.",
                style = "font-size: 13px; color: #666; margin: 0;"
              )
            ),

            shiny::div(
              shiny::h5(
                shiny::icon("users", style = "color: #ff9800;"),
                " Multi-User Ready"
              ),
              shiny::p(
                "Track contributions by user and enable team collaboration.",
                style = "font-size: 13px; color: #666; margin: 0;"
              )
            ),

            shiny::div(
              shiny::h5(
                shiny::icon("bolt", style = "color: #f44336;"),
                " Flexible & Fast"
              ),
              shiny::p(
                "No rigid validation rules. Arrow/Parquet for efficient data handling.",
                style = "font-size: 13px; color: #666; margin: 0;"
              )
            )
          )
        ),

        # Tips
        shiny::hr(),
        shiny::h4("Pro Tips", style = "color: #2c3e50;"),
        shiny::div(
          style = "background-color: #fff9c4; padding: 15px; border-radius: 5px; border-left: 4px solid #fbc02d;",
          shiny::tags$ul(
            style = "margin: 0; padding-left: 20px;",
            shiny::tags$li("Use the ", shiny::strong("Marked"), " filter to work on specific document batches"),
            shiny::tags$li("Switch to ", shiny::strong("Multi-select"), " mode for faster tagging with multiple values"),
            shiny::tags$li("Add ", shiny::strong("notes"), " to documents that need special attention or review"),
            shiny::tags$li("Check the ", shiny::strong("Overview"), " tab regularly to track your progress"),
            shiny::tags$li("Use the ", shiny::strong("Timeline"), " feature to review classification history"),
            shiny::tags$li("Export data regularly to maintain backups")
          )
        )
      )
    )
  )
}
