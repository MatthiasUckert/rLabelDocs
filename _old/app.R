#' Main Classification App with Tabs
#' @param .dir Path to classification directory
#' @param .user_id User ID
#' @export
classification_app <- function(.dir, .user_id = "user") {
  # Validate and load basic data
  check_input_directory(.dir)
  schema <- read_schema(.dir)

  ui <- shiny::navbarPage(
    title = "Document Classification System",
    id = "main_nav",

    # Tab 1: Classification
    shiny::tabPanel(
      "Classification",
      icon = shiny::icon("check-square"),
      mod_classification_ui("classification")
    ),

    # Tab 2: Overview
    shiny::tabPanel(
      "Overview",
      icon = shiny::icon("chart-line"),
      mod_overview_ui("overview")
    ),

    # Tab 3: Browser
    shiny::tabPanel(
      "Browser",
      icon = shiny::icon("search"),
      mod_browser_ui("browser")
    )
  )

  server <- function(input, output, session) {
    # Call module servers
    mod_classification_server("classification", .dir, .user_id, schema)
    mod_overview_server("overview", .dir)
    mod_browser_server("browser", .dir, schema)
  }

  shiny::shinyApp(
    ui = ui,
    server = server,
    options = list(
      launch.browser = TRUE
    )
  )
}

#' Classification Module UI
#' @param id Module namespace ID
#' @export
mod_classification_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .classification-group { margin-bottom: 20px; }
        .classification-title { color: #2c3e50; margin-bottom: 10px; }
        .classification-buttons { margin-bottom: 15px; }
        .classification-btn { margin: 3px; }
        .selected-btn { background-color: #3498db !important; color: white !important; border-color: #3498db !important; }
        .document-content { height: 800px; overflow-y: auto; padding: 20px; border: 1px solid #ddd; background: white; }
        .sidebar { background-color: #f8f9fa; padding: 20px; }
        .filter-panel { background-color: #e9ecef; padding: 15px; margin-bottom: 20px; border-radius: 5px; }
      "))
    ),
    shiny::fluidRow(
      # Left Sidebar (4 columns)
      shiny::column(
        4,
        shiny::div(
          class = "sidebar",
          # Filter Panel
          shiny::div(
            class = "filter-panel",
            shiny::h5("Document Filter"),
            shiny::selectInput(
              ns("doc_filter"),
              label = NULL,
              choices = c("All Documents" = "all", "Unclassified" = "unclassified", "Classified" = "classified"),
              selected = "unclassified"
            )
          ),

          # Search Panel
          shiny::div(
            class = "filter-panel",
            shiny::h5("Document Search"),
            shiny::textInput(
              ns("search_doc_id"),
              label = "Jump to Document ID:",
              placeholder = "Enter document ID...",
              value = ""
            ),
            shiny::div(
              style = "margin-top: 10px;",
              shiny::actionButton(
                inputId = ns("search_btn"),
                label = "Go to Document",
                class = "btn-primary",
                style = "width: 100%; margin-bottom: 5px;"
              ),
              shiny::actionButton(
                inputId = ns("clear_search_btn"),
                label = "Clear",
                class = "btn-secondary",
                style = "width: 100%;"
              )
            )
          ),

          # Document Info
          shiny::wellPanel(
            shiny::h5("Document Info"),
            shiny::verbatimTextOutput(ns("doc_info"))
          ),
          shiny::wellPanel(
            shiny::h4("Classification"),
            shiny::uiOutput(ns("classification_ui")),
            shiny::h5("Actions"),
            shiny::div(
              style = "text-align: center;",
              shiny::actionButton(ns("save_btn"), "Save & Next", class = "btn-success", style = "width: 50%;"),
              shiny::actionButton(ns("prev_btn"), "Prev.", class = "btn-warning", style = "width: 24%;"),
              shiny::actionButton(ns("next_btn"), "Next", class = "btn-info", style = "width: 24%;")
            )
          ),
        )
      ),

      # Right Panel - Document Display (8 columns)
      shiny::column(
        8,
        shiny::wellPanel(
          shiny::h4("Document Content"),
          shiny::div(
            class = "document-content",
            shiny::htmlOutput(ns("document_display"))
          )
        )
      )
    )
  )
}

#' Classification Module Server
#' @param id Module namespace ID
#' @param .dir Path to classification directory
#' @param .user_id User ID
#' @param schema Classification schema
#' @export
mod_classification_server <- function(id, .dir, .user_id, schema) {
  shiny::moduleServer(id, function(input, output, session) {
    # State management
    values <- shiny::reactiveValues(
      current_doc_id = NULL,
      current_doc = NULL,
      filtered_doc_ids = NULL,
      current_index = 1,
      pending_search = NULL
    )

    # Selections for classifications
    selections <- shiny::reactiveValues()

    # Initialize selections
    shiny::observe({
      for (class_name in names(schema)) {
        if (is.null(selections[[class_name]])) {
          selections[[class_name]] <- NULL
        }
      }
    })

    # Get filtered document list based on dropdown
    get_filtered_documents <- shiny::reactive({
      filter_type <- input$doc_filter

      switch(filter_type,
        "all" = get_docids(.dir, "All"),
        "unclassified" = get_docids(.dir, "Unclassified"),
        "classified" = get_docids(.dir, "Classified")
      )
    })

    # Update document list when filter changes
    shiny::observeEvent(get_filtered_documents(), {
      values$filtered_doc_ids <- get_filtered_documents()
      values$current_index <- 1

      if (length(values$filtered_doc_ids) > 0) {
        values$current_doc_id <- values$filtered_doc_ids[1]
        load_current_document()
      } else {
        values$current_doc_id <- NULL
        values$current_doc <- NULL
      }
    })

    # Load current document
    load_current_document <- function() {
      if (!is.null(values$current_doc_id)) {
        values$current_doc <- read_single_document(.dir, values$current_doc_id)

        # Load existing classifications for this document
        existing_df <- read_classification(.dir, values$current_doc_id)
        if (nrow(existing_df) > 0) {
          schema_cols <- names(schema)
          existing_cols <- intersect(schema_cols, names(existing_df))
          if (length(existing_cols) > 0) {
            existing <- as.list(existing_df[1, existing_cols, drop = FALSE])
            for (class_name in names(schema)) {
              selections[[class_name]] <- existing[[class_name]]
            }
          }
        } else {
          # No existing classification, reset all selections
          for (class_name in names(schema)) {
            selections[[class_name]] <- NULL
          }
        }
      }
    }

    # Document info display
    output$doc_info <- shiny::renderText({
      format_document_info(
        .dir = .dir,
        .doc_id = values$current_doc_id,
        .current_index = values$current_index,
        .total_filtered = length(values$filtered_doc_ids %||% character()),
        .filter_type = input$doc_filter,
        .schema = schema
      )
    })

    # Document display
    output$document_display <- shiny::renderUI({
      if (is.null(values$current_doc) || nrow(values$current_doc) == 0) {
        shiny::tags$div(
          style = "text-align: center; color: #999; padding: 50px;",
          shiny::tags$h3("No document loaded"),
          shiny::tags$p(paste("Filter:", input$doc_filter))
        )
      } else {
        shiny::HTML(values$current_doc$HTML[1])
      }
    })

    # Classification UI with highlighting
    output$classification_ui <- shiny::renderUI({
      current_selections <- shiny::reactiveValuesToList(selections)

      button_groups <- lapply(names(schema), function(class_name) {
        class_buttons <- lapply(schema[[class_name]], function(value) {
          button_id <- paste0("btn_", class_name, "_", gsub("[^A-Za-z0-9]", "_", value))

          is_selected <- !is.null(current_selections[[class_name]]) &&
            current_selections[[class_name]] == value

          btn_class <- if (is_selected) {
            "btn btn-primary classification-btn selected-btn"
          } else {
            "btn btn-outline-primary classification-btn"
          }

          shiny::actionButton(
            inputId = session$ns(button_id),
            label = value,
            class = btn_class
          )
        })

        shiny::div(
          class = "classification-group",
          shiny::h5(class_name, class = "classification-title"),
          shiny::div(class = "classification-buttons", class_buttons)
        )
      })

      shiny::tagList(button_groups)
    })

    # Handle button clicks
    shiny::observe({
      for (class_name in names(schema)) {
        for (value in schema[[class_name]]) {
          local({
            local_class <- class_name
            local_value <- value
            button_id <- paste0("btn_", local_class, "_", gsub("[^A-Za-z0-9]", "_", local_value))

            shiny::observeEvent(input[[button_id]], {
              selections[[local_class]] <- local_value
              shiny::showNotification(paste("Selected:", local_class, "=", local_value),
                type = "default", duration = 1
              )
            })
          })
        }
      }
    })

    # Check if all classifications are complete
    all_classifications_complete <- shiny::reactive({
      current_selections <- shiny::reactiveValuesToList(selections)
      all(names(schema) %in% names(current_selections)) &&
        all(sapply(current_selections[names(schema)], function(x) !is.null(x)))
    })

    # Save button - requires ALL classifications
    shiny::observeEvent(input$save_btn, {
      if (!all_classifications_complete()) {
        missing_classes <- setdiff(names(schema), names(shiny::reactiveValuesToList(selections)))
        shiny::showNotification(
          paste("Please complete ALL classifications. Missing:", paste(missing_classes, collapse = ", ")),
          type = "warning", duration = 3
        )
        return()
      }

      current_selections <- shiny::reactiveValuesToList(selections)
      to_save <- current_selections[names(schema)]

      tryCatch(
        {
          save_classification(.dir, values$current_doc_id, .user_id, to_save)
          shiny::showNotification("All classifications saved!", type = "default", duration = 1)
          move_to_next()
        },
        error = function(e) {
          shiny::showNotification(paste("Error:", e$message), type = "error")
        }
      )
    })

    # Skip and Previous buttons
    shiny::observeEvent(input$next_btn, {
      move_to_next()
    })

    shiny::observeEvent(input$prev_btn, {
      if (values$current_index > 1) {
        values$current_index <- values$current_index - 1
        values$current_doc_id <- values$filtered_doc_ids[values$current_index]
        load_current_document()
      }
    })

    # Move to next document
    move_to_next <- function() {
      if (values$current_index < length(values$filtered_doc_ids)) {
        values$current_index <- values$current_index + 1
        values$current_doc_id <- values$filtered_doc_ids[values$current_index]
        load_current_document()
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Complete!",
          paste("All", input$doc_filter, "documents processed!"),
          footer = shiny::modalButton("OK")
        ))
      }
    }

    # Document search functionality
    shiny::observeEvent(input$search_btn, {
      perform_document_search()
    })

    # Main search function
    perform_document_search <- function() {
      search_id <- trimws(input$search_doc_id)

      if (search_id == "") {
        shiny::showNotification("Please enter a document ID to search for", type = "warning", duration = 3)
        return()
      }

      # Check if document exists in current filtered list
      if (search_id %in% values$filtered_doc_ids) {
        # Found in current filter - jump to it
        found_index <- which(values$filtered_doc_ids == search_id)[1]
        values$current_index <- found_index
        values$current_doc_id <- search_id
        load_current_document()
        shiny::showNotification(paste("Found document at position", found_index, "of", length(values$filtered_doc_ids)),
          type = "message", duration = 3
        )
      } else {
        # Not in current filter - check if it exists elsewhere
        all_docs <- get_docids(.dir, "All")
        classified_docs <- get_docids(.dir, "Classified")
        unclassified_docs <- get_docids(.dir, "Unclassified")

        if (search_id %in% all_docs) {
          # Document exists but not in current filter
          current_filter <- input$doc_filter

          if (current_filter == "unclassified" && search_id %in% classified_docs) {
            shiny::showModal(shiny::modalDialog(
              title = "Document Found in Different Filter",
              paste("Document", search_id, "is classified. Switch to 'Classified' or 'All Documents' filter to view it."),
              footer = shiny::tagList(
                shiny::actionButton(session$ns("switch_to_classified"), "Switch to Classified", class = "btn-primary"),
                shiny::modalButton("Cancel")
              )
            ))
          } else if (current_filter == "classified" && search_id %in% unclassified_docs) {
            shiny::showModal(shiny::modalDialog(
              title = "Document Found in Different Filter",
              paste("Document", search_id, "is unclassified. Switch to 'Unclassified' or 'All Documents' filter to view it."),
              footer = shiny::tagList(
                shiny::actionButton(session$ns("switch_to_unclassified"), "Switch to Unclassified", class = "btn-primary"),
                shiny::modalButton("Cancel")
              )
            ))
          } else {
            shiny::showModal(shiny::modalDialog(
              title = "Document Found in Different Filter",
              paste("Document", search_id, "exists but is not visible in the current filter. Switch to 'All Documents' to view it."),
              footer = shiny::tagList(
                shiny::actionButton(session$ns("switch_to_all"), "Switch to All Documents", class = "btn-primary"),
                shiny::modalButton("Cancel")
              )
            ))
          }
        } else {
          shiny::showNotification(paste("Document ID", search_id, "not found in the system"),
            type = "error", duration = 4
          )
        }
      }
    }

    # Handle filter switching from search modals
    shiny::observeEvent(input$switch_to_classified, {
      shiny::updateSelectInput(session, "doc_filter", selected = "classified")
      shiny::removeModal()
      values$pending_search <- trimws(input$search_doc_id)
    })

    shiny::observeEvent(input$switch_to_unclassified, {
      shiny::updateSelectInput(session, "doc_filter", selected = "unclassified")
      shiny::removeModal()
      values$pending_search <- trimws(input$search_doc_id)
    })

    shiny::observeEvent(input$switch_to_all, {
      shiny::updateSelectInput(session, "doc_filter", selected = "all")
      shiny::removeModal()
      values$pending_search <- trimws(input$search_doc_id)
    })

    # Handle pending search after filter change
    shiny::observe({
      if (!is.null(values$pending_search) && values$pending_search != "") {
        search_id <- values$pending_search

        if (search_id %in% values$filtered_doc_ids) {
          found_index <- which(values$filtered_doc_ids == search_id)[1]
          values$current_index <- found_index
          values$current_doc_id <- search_id
          load_current_document()
          shiny::showNotification(paste("Found document at position", found_index, "of", length(values$filtered_doc_ids)),
            type = "message", duration = 3
          )
          values$pending_search <- NULL
        }
      }
    })

    # Clear search field
    shiny::observeEvent(input$clear_search_btn, {
      shiny::updateTextInput(session, "search_doc_id", value = "")
      shiny::showNotification("Search cleared", type = "message", duration = 1)
    })
  })
}

#' Overview Module UI
#' @param id Module namespace ID
#' @export
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .overview-panel { background-color: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 8px; border: 1px solid #dee2e6; }
        .metric-box { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px; }
        .metric-value { font-size: 2.5em; font-weight: bold; margin: 0; }
        .metric-label { font-size: 1.1em; margin: 5px 0 0 0; opacity: 0.9; }
        .chart-container { background: white; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6; }
      "))
    ),
    shiny::h2("Classification Overview", style = "color: #2c3e50; margin-bottom: 30px;"),

    # Top Row - Key Metrics
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::div(
          class = "metric-box",
          shiny::h3(class = "metric-value", shiny::textOutput(ns("total_docs"))),
          shiny::p(class = "metric-label", "Total Documents")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box",
          shiny::h3(class = "metric-value", shiny::textOutput(ns("classified_docs"))),
          shiny::p(class = "metric-label", "Classified")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box",
          shiny::h3(class = "metric-value", shiny::textOutput(ns("remaining_docs"))),
          shiny::p(class = "metric-label", "Remaining")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box",
          shiny::h3(class = "metric-value", shiny::textOutput(ns("completion_rate"))),
          shiny::p(class = "metric-label", "% Complete")
        )
      )
    ),

    # Second Row - Progress Chart and User Activity
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::div(
          class = "overview-panel",
          shiny::h4("Classification Progress", style = "color: #34495e; margin-bottom: 15px;"),
          shiny::div(
            class = "chart-container",
            shiny::plotOutput(ns("progress_chart"), height = "300px")
          )
        )
      ),
      shiny::column(
        6,
        shiny::div(
          class = "overview-panel",
          shiny::h4("User Activity", style = "color: #34495e; margin-bottom: 15px;"),
          shiny::div(
            class = "chart-container",
            shiny::plotOutput(ns("user_activity_chart"), height = "300px")
          )
        )
      )
    ),

    # Third Row - Classification Distributions
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "overview-panel",
          shiny::h4("Classification Distributions", style = "color: #34495e; margin-bottom: 15px;"),
          shiny::uiOutput(ns("classification_charts"))
        )
      )
    ),

    # Fourth Row - Timeline and Recent Activity
    shiny::fluidRow(
      shiny::column(
        7,
        shiny::div(
          class = "overview-panel",
          shiny::h4("Classification Timeline", style = "color: #34495e; margin-bottom: 15px;"),
          shiny::div(
            class = "chart-container",
            shiny::plotOutput(ns("timeline_chart"), height = "300px")
          )
        )
      ),
      shiny::column(
        5,
        shiny::div(
          class = "overview-panel",
          shiny::h4("Quick Stats", style = "color: #34495e; margin-bottom: 15px;"),
          shiny::verbatimTextOutput(ns("quick_stats"))
        )
      )
    ),

    # Fifth Row - Recent Classifications Table
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "overview-panel",
          shiny::h4("Recent Classifications", style = "color: #34495e; margin-bottom: 15px;"),
          shiny::div(
            class = "chart-container",
            DT::dataTableOutput(ns("recent_classifications"))
          )
        )
      )
    )
  )
}

#' Overview Module Server (FIXED VERSION)
#' @param id Module namespace ID
#' @param .dir Path to classification directory
#' @export
mod_overview_server <- function(id, .dir) {
  shiny::moduleServer(id, function(input, output, session) {
    # Get progress stats reactively
    progress_stats <- shiny::reactive({
      get_progress_stats(.dir)
    })

    # Render the key metrics
    output$total_docs <- shiny::renderText({
      progress_stats()$total_documents
    })

    output$classified_docs <- shiny::renderText({
      progress_stats()$classified_documents
    })

    output$remaining_docs <- shiny::renderText({
      progress_stats()$total_documents - progress_stats()$classified_documents
    })

    output$completion_rate <- shiny::renderText({
      paste0(progress_stats()$percentage_complete, "%")
    })

    # Progress chart
    output$progress_chart <- shiny::renderPlot({
      plot_progress_overview(.dir)
    })

    # User activity chart
    output$user_activity_chart <- shiny::renderPlot({
      plot_user_activity(.dir)
    })

    # Classification timeline
    output$timeline_chart <- shiny::renderPlot({
      plot_classification_timeline(.dir)
    })

    # Classification distribution charts for each class
    output$classification_charts <- shiny::renderUI({
      schema <- read_schema(.dir)

      if (length(schema) == 0) {
        return(shiny::tags$p("No classification schema available", style = "color: #999;"))
      }

      chart_outputs <- lapply(names(schema), function(class_name) {
        chart_id <- paste0("dist_chart_", gsub("[^A-Za-z0-9]", "_", class_name))

        shiny::column(
          6,
          shiny::div(
            class = "chart-container",
            shiny::h5(paste(class_name, "Distribution"), style = "color: #34495e; margin-bottom: 10px;"),
            shiny::plotOutput(session$ns(chart_id), height = "250px")
          )
        )
      })

      # Arrange in rows of 2
      chart_rows <- list()
      for (i in seq(1, length(chart_outputs), by = 2)) {
        row_charts <- chart_outputs[i:min(i + 1, length(chart_outputs))]
        chart_rows <- append(chart_rows, list(shiny::fluidRow(row_charts)), length(chart_rows))
      }

      shiny::tagList(chart_rows)
    })

    # Render individual distribution charts
    shiny::observe({
      schema <- read_schema(.dir)

      for (class_name in names(schema)) {
        local({
          local_class <- class_name
          chart_id <- paste0("dist_chart_", gsub("[^A-Za-z0-9]", "_", local_class))

          output[[chart_id]] <- shiny::renderPlot({
            plot_classification_distribution(.dir, local_class)
          })
        })
      }
    })

    # Quick stats
    output$quick_stats <- shiny::renderText({
      counts <- get_filter_counts(.dir)
      progress <- get_progress_stats(.dir)

      # Get classification stats
      stats_df <- get_classification_stats(.dir)

      paste0(
        "Filter Counts:\n",
        "* All Documents: ", counts$all, "\n",
        "* Classified: ", counts$classified, "\n",
        "* Unclassified: ", counts$unclassified, "\n\n",
        "Progress:\n",
        "* Total: ", progress$total_documents, " documents\n",
        "* Completed: ", progress$classified_documents, " (", progress$percentage_complete, "%)\n",
        "* Remaining: ", progress$total_documents - progress$classified_documents, "\n\n",
        "Classification Combinations:\n",
        "* Unique combinations: ", nrow(stats_df)
      )
    })

    # Recent classifications table
    output$recent_classifications <- DT::renderDataTable(
      {
        classifications_file <- file.path(.dir, "Classifications.parquet")
        if (file.exists(classifications_file)) {
          classifications <- arrow::read_parquet(classifications_file)
          if (nrow(classifications) > 0) {
            classifications %>%
              dplyr::arrange(dplyr::desc(Timestamp)) %>%
              dplyr::slice_head(n = 20) %>%
              dplyr::select(DocID, UserID, Timestamp, dplyr::everything())
          } else {
            data.frame(Message = "No classifications yet")
          }
        } else {
          data.frame(Message = "No classifications file found")
        }
      },
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

#' Browser Module UI
#' @param id Module namespace ID
#' @export
mod_browser_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .browser-sidebar { background-color: #f8f9fa; padding: 20px; }
        .filter-section { background-color: #e9ecef; padding: 15px; margin-bottom: 20px; border-radius: 5px; }
        .document-list { max-height: 400px; overflow-y: auto; }
        .document-viewer { height: 800px; overflow-y: auto; padding: 20px; border: 1px solid #ddd; background: white; }
        .nav-controls { text-align: center; margin: 15px 0; }
      "))
    ),
    shiny::h2("Document Browser"),
    shiny::fluidRow(
      # Left Sidebar (4 columns)
      shiny::column(
        4,
        shiny::div(
          class = "browser-sidebar",
          # Classification Filters
          shiny::div(
            class = "filter-section",
            shiny::h5("Classification Filters"),
            shiny::uiOutput(ns("classification_filters")),
            shiny::actionButton(ns("apply_filters"), "Apply Filters", class = "btn-primary", style = "width: 100%; margin-top: 10px;"),
            shiny::actionButton(ns("clear_filters"), "Clear All", class = "btn-secondary", style = "width: 100%; margin-top: 5px;")
          ),

          # Document Info
          shiny::wellPanel(
            shiny::h5("Document Info"),
            shiny::verbatimTextOutput(ns("doc_info"))
          ),

          # Document List
          shiny::wellPanel(
            shiny::h5("Filtered Documents"),
            shiny::div(
              class = "document-list",
              DT::dataTableOutput(ns("document_table"))
            )
          )
        )
      ),

      # Right Panel - Document Viewer (8 columns)
      shiny::column(
        8,
        shiny::wellPanel(
          shiny::h4("Document Viewer"),
          # Navigation Controls
          shiny::div(
            class = "nav-controls",
            shiny::actionButton(ns("first_btn"), "First", class = "btn-info"),
            shiny::actionButton(ns("prev_btn"), "Previous", class = "btn-warning"),
            shiny::actionButton(ns("next_btn"), "Next", class = "btn-warning"),
            shiny::actionButton(ns("last_btn"), "Last", class = "btn-info")
          ),
          # Document Content
          shiny::div(
            class = "document-viewer",
            shiny::htmlOutput(ns("document_display"))
          )
        )
      )
    )
  )
}

#' Browser Module Server
#' @param id Module namespace ID
#' @param .dir Path to classification directory
#' @param schema Classification schema
#' @export
mod_browser_server <- function(id, .dir, schema) {
  shiny::moduleServer(id, function(input, output, session) {
    # State management
    values <- shiny::reactiveValues(
      current_doc_id = NULL,
      current_doc = NULL,
      filtered_doc_ids = NULL,
      current_index = 1,
      classification_filters = list(),
      initialized = FALSE
    )

    # Initialize ONLY ONCE
    shiny::observe({
      if (!values$initialized) {
        all_classified <- get_docids(.dir, "Classified")

        values$filtered_doc_ids <- all_classified
        if (length(all_classified) > 0) {
          values$current_index <- 1
          values$current_doc_id <- all_classified[1]
          load_current_document()
        }
        values$initialized <- TRUE
      }
    })

    # Load current document
    load_current_document <- function() {
      if (!is.null(values$current_doc_id)) {
        values$current_doc <- read_single_document(.dir, values$current_doc_id)
      }
    }

    # Classification filters
    output$classification_filters <- shiny::renderUI({
      classification_values <- get_classification_values(.dir)

      if (length(classification_values) == 0) {
        return(shiny::tags$p("No classifications available", style = "color: #999;"))
      }

      filter_inputs <- lapply(names(classification_values), function(class_name) {
        choices <- c("All" = "All", stats::setNames(classification_values[[class_name]], classification_values[[class_name]]))
        shiny::div(
          style = "margin-bottom: 10px;",
          shiny::selectInput(
            inputId = session$ns(paste0("filter_", class_name)),
            label = class_name,
            choices = choices,
            selected = "All"
          )
        )
      })

      shiny::tagList(filter_inputs)
    })

    # Document info
    output$doc_info <- shiny::renderText({
      if (is.null(values$current_doc_id)) {
        "No document selected"
      } else {
        paste0(
          "Document: ", values$current_doc_id, "\n",
          "Position: ", values$current_index, " of ", length(values$filtered_doc_ids %||% character()), "\n",
          "Total filtered: ", length(values$filtered_doc_ids %||% character())
        )
      }
    })

    # Document table
    output$document_table <- DT::renderDataTable(
      {
        if (is.null(values$filtered_doc_ids) || length(values$filtered_doc_ids) == 0) {
          data.frame(Message = "No documents available")
        } else {
          data.frame(
            Index = seq_along(values$filtered_doc_ids),
            DocID = values$filtered_doc_ids,
            Current = ifelse(values$filtered_doc_ids == values$current_doc_id, "->", ""),
            stringsAsFactors = FALSE
          )
        }
      },
      options = list(pageLength = 10, scrollX = TRUE),
      selection = "single"
    )

    # Document display
    output$document_display <- shiny::renderUI({
      if (is.null(values$current_doc) || nrow(values$current_doc) == 0) {
        shiny::tags$div(
          style = "text-align: center; color: #999; padding: 50px;",
          shiny::tags$h3("No document loaded"),
          shiny::tags$p("Current doc ID:", values$current_doc_id)
        )
      } else {
        shiny::HTML(values$current_doc$HTML[1])
      }
    })

    # Button events
    shiny::observeEvent(input$apply_filters, {
      # Get filter values
      classification_values <- get_classification_values(.dir)
      filters <- list()

      for (class_name in names(classification_values)) {
        filter_input_id <- paste0("filter_", class_name)
        if (!is.null(input[[filter_input_id]])) {
          filters[[class_name]] <- input[[filter_input_id]]
        }
      }

      # Apply filters
      new_filtered_ids <- filter_documents_by_classification(.dir, filters)

      values$filtered_doc_ids <- new_filtered_ids
      values$current_index <- 1

      if (length(new_filtered_ids) > 0) {
        values$current_doc_id <- new_filtered_ids[1]
        load_current_document()
      } else {
        values$current_doc_id <- NULL
        values$current_doc <- NULL
      }

      shiny::showNotification(paste("Found", length(new_filtered_ids), "documents"), type = "message", duration = 2)
    })

    shiny::observeEvent(input$clear_filters, {
      all_classified <- get_docids(.dir, "Classified")
      values$filtered_doc_ids <- all_classified
      values$current_index <- 1

      if (length(all_classified) > 0) {
        values$current_doc_id <- all_classified[1]
        load_current_document()
      }

      shiny::showNotification(paste("Showing all", length(all_classified), "documents"), type = "message", duration = 2)
    })

    shiny::observeEvent(input$next_btn, {
      if (length(values$filtered_doc_ids) > 0 && values$current_index < length(values$filtered_doc_ids)) {
        values$current_index <- values$current_index + 1
        values$current_doc_id <- values$filtered_doc_ids[values$current_index]
        load_current_document()
        shiny::showNotification(paste("Document", values$current_index, "of", length(values$filtered_doc_ids)), type = "message", duration = 1)
      }
    })

    shiny::observeEvent(input$prev_btn, {
      if (length(values$filtered_doc_ids) > 0 && values$current_index > 1) {
        values$current_index <- values$current_index - 1
        values$current_doc_id <- values$filtered_doc_ids[values$current_index]
        load_current_document()
        shiny::showNotification(paste("Document", values$current_index, "of", length(values$filtered_doc_ids)), type = "message", duration = 1)
      }
    })

    shiny::observeEvent(input$first_btn, {
      if (length(values$filtered_doc_ids) > 0) {
        values$current_index <- 1
        values$current_doc_id <- values$filtered_doc_ids[1]
        load_current_document()
        shiny::showNotification("First document", type = "message", duration = 1)
      }
    })

    shiny::observeEvent(input$last_btn, {
      if (length(values$filtered_doc_ids) > 0) {
        values$current_index <- length(values$filtered_doc_ids)
        values$current_doc_id <- values$filtered_doc_ids[values$current_index]
        load_current_document()
        shiny::showNotification("Last document", type = "message", duration = 1)
      }
    })
  })
}

# Launch the Modular Classification App

if (FALSE) {
  classification_app(
    .dir = "inst/extdata/TestClassification/",
    .user_id = "Matthias"
  )
}
