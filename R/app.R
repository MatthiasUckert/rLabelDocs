# MAIN APPLICATION
# Entry point for the Document Classification System

#' Launch Classification App
#'
#' Main function to launch the document classification Shiny application.
#' Currently includes only the Classification tab. Overview and Browser tabs
#' will be added in subsequent development phases.
#'
#' @param .dir Path to project directory containing:
#'   - Documents.parquet (required)
#'   - Schema.xlsx or Schema.csv (required)
#'   - ClassificationDetails.parquet (created if missing)
#' @param .user_id User identifier for audit trail
#' @param .port Port number for Shiny server (default: random)
#' @param .launch_browser Whether to launch browser automatically (default: TRUE)
#'
#' @return Shiny app object
#'
#' @examples
#' \dontrun{
#' # Launch app with default settings
#' classification_app(
#'   .dir = "path/to/project/",
#'   .user_id = "analyst1"
#' )
#'
#' # Launch on specific port without browser
#' classification_app(
#'   .dir = "path/to/project/",
#'   .user_id = "analyst1",
#'   .port = 8888,
#'   .launch_browser = FALSE
#' )
#' }
#'
#' @export
classification_app <- function(.dir, .user_id = "user", .port = NULL, .launch_browser = TRUE) {

  # ===== VALIDATION =====
  message("Validating project directory...")
  validate_project_directory(.dir)
  validate_user_id(.user_id)

  # ===== SCHEMA CHANGE DETECTION =====
  schema_check <- check_schema_changes(.dir)
  if (schema_check$changed) {
    warning(
      "\n", schema_check$message,
      "\nTo update the schema hash and suppress this warning, run:",
      "\n  update_schema_hash('", .dir, "')",
      call. = FALSE
    )
  }

  # ===== LOAD SCHEMA =====
  message("Loading schema...")
  schema <- read_schema(.dir)

  message("\nApp ready to launch!")
  message("User: ", .user_id)
  message("Schemas: ", paste(sapply(schema, function(x) x$name), collapse = ", "))
  message("\n")

  # ===== UI =====
  ui <- shiny::navbarPage(
    title = "Document Classification System",
    id = "main_nav",

    # Tab 1: Classification
    shiny::tabPanel(
      "Classification",
      icon = shiny::icon("check-square"),
      mod_classification_ui("classification")
    )

    # TODO: Add Overview tab in next phase
    # TODO: Add Browser tab in next phase
  )

  # ===== SERVER =====
  server <- function(input, output, session) {
    # Call classification module server
    mod_classification_server(
      "classification",
      .dir = .dir,
      .user_id = .user_id,
      schema = schema
    )

    # TODO: Add Overview module server in next phase
    # TODO: Add Browser module server in next phase
  }

  # ===== LAUNCH APP =====
  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(
      port = .port,
      launch.browser = .launch_browser
    )
  )
}

#' Quick Start Function
#'
#' Simplified wrapper for common use case.
#'
#' @param project_dir Path to project directory
#' @param user User identifier
#'
#' @return Shiny app object
#' @export
start_classification <- function(project_dir, user = Sys.getenv("USER")) {
  classification_app(.dir = project_dir, .user_id = user)
}
