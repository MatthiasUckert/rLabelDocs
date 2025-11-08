# BROWSER MODULE
# Advanced filtering and document browsing

#' Browser Module UI
#'
#' Generates the user interface for the browser/filtering tab.
#'
#' @param id Module namespace ID
#' @return Shiny UI
#' @export
mod_browser_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    browser_css(),

    shiny::fluidRow(
      # ===== LEFT SIDEBAR - FILTERS (3 columns) =====
      shiny::column(
        3,
        shiny::div(
          class = "sidebar",

          shiny::h4("Filter Documents", style = "margin-top: 0;"),
          shiny::p(
            style = "font-size: 13px; color: #666; margin-bottom: 20px;",
            "Showing documents with at least one classification. Use filters to narrow the list."
          ),

          # Schema Filter
          shiny::div(
            class = "filter-section",
            shiny::h5("Schema"),
            shiny::uiOutput(ns("schema_filter_ui"))
          ),

          # Class Filter (dynamic based on schema)
          shiny::div(
            class = "filter-section",
            shiny::h5("Class"),
            shiny::uiOutput(ns("class_filter_ui"))
          ),

          # Value Filter (dynamic based on class)
          shiny::div(
            class = "filter-section",
            shiny::h5("Value"),
            shiny::uiOutput(ns("value_filter_ui"))
          ),

          # Notes Filter
          shiny::div(
            class = "filter-section",
            shiny::h5("Notes"),
            shiny::checkboxInput(
              ns("notes_filter"),
              "Show only documents with notes",
              value = FALSE
            )
          ),

          # Action Buttons
          shiny::div(
            style = "margin-top: 20px;",
            shiny::actionButton(
              ns("apply_filter_btn"),
              "Apply Filter",
              icon = shiny::icon("filter"),
              class = "btn-primary",
              style = "width: 100%; margin-bottom: 10px;"
            ),
            shiny::actionButton(
              ns("reset_filter_btn"),
              "Reset All",
              icon = shiny::icon("times"),
              class = "btn-secondary",
              style = "width: 100%;"
            )
          ),

          # Filter Results Summary
          shiny::wellPanel(
            shiny::h5("Filter Results"),
            shiny::verbatimTextOutput(ns("filter_summary"))
          )
        )
      ),

      # ===== MIDDLE - DOCUMENT LIST (3 columns) =====
      shiny::column(
        3,
        shiny::div(
          class = "document-list-panel",
          shiny::h4("Documents", style = "margin-top: 0;"),

          # Marking controls
          shiny::div(
            style = "margin-bottom: 10px; padding: 10px; background-color: #e3f2fd; border-radius: 5px;",
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
              shiny::div(
                style = "font-size: 13px; font-weight: 500; color: #1976d2;",
                shiny::textOutput(ns("marked_count_text"), inline = TRUE)
              ),
              shiny::actionButton(
                ns("clear_marks_browser"),
                "Clear Marks",
                icon = shiny::icon("times"),
                class = "btn-secondary btn-sm"
              )
            ),
            shiny::div(
              style = "display: flex; gap: 5px;",
              shiny::actionButton(
                ns("mark_all_visible"),
                "Mark All Visible",
                icon = shiny::icon("check-square"),
                class = "btn-info btn-sm",
                style = "flex: 1;"
              ),
              shiny::actionButton(
                ns("unmark_all_visible"),
                "Unmark All Visible",
                icon = shiny::icon("square"),
                class = "btn-secondary btn-sm",
                style = "flex: 1;"
              )
            )
          ),

          # Search box
          shiny::textInput(
            ns("doc_search"),
            label = NULL,
            placeholder = "Search documents...",
            width = "100%"
          ),

          # Document list with checkboxes
          shiny::div(
            class = "doc-list-container",
            shiny::uiOutput(ns("document_list_ui"))
          )
        )
      ),

      # ===== RIGHT - DOCUMENT VIEWER (6 columns) =====
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::div(
            class = "viewer-header",
            shiny::h4("Document Viewer", style = "display: inline-block; margin: 0;"),
            shiny::div(
              style = "float: right;",
              shiny::textOutput(ns("viewer_doc_id"), inline = TRUE)
            )
          ),

          shiny::hr(),

          # Document content
          shiny::div(
            class = "document-viewer",
            shiny::htmlOutput(ns("document_content"))
          ),

          # Classifications for this document
          shiny::hr(),
          shiny::h5("Classifications"),
          shiny::uiOutput(ns("document_classifications")),

          # Document Notes
          shiny::hr(),
          shiny::h5("Document Notes"),
          shiny::uiOutput(ns("document_notes_display"))
        )
      )
    )
  )
}

#' Browser Module Server
#'
#' Server logic for the browser module.
#'
#' @param id Module namespace ID
#' @param .dir Reactive or static path to project directory
#' @param schema Reactive or static schema list
#' @param marked_docs ReactiveValues object with marked document IDs
#' @export
mod_browser_server <- function(id, .dir, schema, marked_docs = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Make inputs reactive if they're not already
    dir_r <- if (shiny::is.reactive(.dir)) .dir else shiny::reactive(.dir)
    schema_r <- if (shiny::is.reactive(schema)) schema else shiny::reactive(schema)

    # ===== STATE MANAGEMENT =====
    values <- shiny::reactiveValues(
      filtered_docs = NULL,
      selected_doc_id = NULL,
      filter_applied = FALSE
    )

    # ===== MARKED COUNT DISPLAY =====
    output$marked_count_text <- shiny::renderText({
      if (is.null(marked_docs)) {
        return("Marking disabled")
      }
      count <- length(marked_docs$ids)
      if (count == 0) {
        "No marked documents"
      } else if (count == 1) {
        "1 document marked"
      } else {
        paste(count, "documents marked")
      }
    })

    # ===== INITIALIZE WITH CLASSIFIED DOCUMENTS =====
    shiny::observe({
      # Only initialize once
      if (is.null(values$filtered_docs) && !values$filter_applied) {
        # Get documents with at least one classification (fully or partially classified)
        classified_docs <- get_docids(dir_r(), "Classified")
        values$filtered_docs <- classified_docs

        # Select first document if available
        if (length(classified_docs) > 0) {
          values$selected_doc_id <- classified_docs[1]
        }
      }
    })

    # ===== SCHEMA FILTER UI =====
    output$schema_filter_ui <- shiny::renderUI({
      sch <- schema_r()

      choices <- c("-- All Schemas --" = "")
      for (sid in names(sch)) {
        choices[sch[[sid]]$name] <- sid
      }

      shiny::selectInput(
        session$ns("schema_select"),
        label = NULL,
        choices = choices,
        selected = ""
      )
    })

    # ===== CLASS FILTER UI (dynamic based on schema) =====
    output$class_filter_ui <- shiny::renderUI({
      sch <- schema_r()
      selected_schema <- input$schema_select

      if (is.null(selected_schema) || selected_schema == "") {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "Select a schema first"
        ))
      }

      classes <- names(sch[[selected_schema]]$classes)
      choices <- c("-- All Classes --" = "", classes)

      shiny::selectInput(
        session$ns("class_select"),
        label = NULL,
        choices = choices,
        selected = ""
      )
    })

    # ===== VALUE FILTER UI (dynamic based on class) =====
    output$value_filter_ui <- shiny::renderUI({
      sch <- schema_r()
      selected_schema <- input$schema_select
      selected_class <- input$class_select

      if (is.null(selected_schema) || selected_schema == "" ||
          is.null(selected_class) || selected_class == "") {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "Select schema and class first"
        ))
      }

      values_list <- sch[[selected_schema]]$classes[[selected_class]]

      shiny::selectizeInput(
        session$ns("value_select"),
        label = NULL,
        choices = values_list,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select values (optional)...",
          plugins = list("remove_button")
        )
      )
    })

    # ===== APPLY FILTER =====
    shiny::observeEvent(input$apply_filter_btn, {
      apply_filters()
    })

    apply_filters <- function() {
      all_class <- read_all_classifications(dir_r())

      if (nrow(all_class) == 0) {
        values$filtered_docs <- character(0)
        values$filter_applied <- TRUE
        shiny::showNotification("No classifications found", type = "warning", duration = 2)
        return()
      }

      filtered <- all_class

      # Apply schema filter
      if (!is.null(input$schema_select) && input$schema_select != "") {
        filtered <- filtered %>%
          dplyr::filter(Schema == as.integer(input$schema_select))
      }

      # Apply class filter
      if (!is.null(input$class_select) && input$class_select != "") {
        filtered <- filtered %>%
          dplyr::filter(Class == input$class_select)
      }

      # Apply value filter
      if (!is.null(input$value_select) && length(input$value_select) > 0) {
        filtered <- filtered %>%
          dplyr::filter(Value %in% input$value_select)
      }

      # Get unique document IDs
      if (nrow(filtered) > 0) {
        doc_ids <- unique(filtered$DocID)

        # Apply notes filter
        if (!is.null(input$notes_filter) && input$notes_filter) {
          docs_with_notes <- get_docids_with_notes(dir_r())
          doc_ids <- doc_ids[doc_ids %in% docs_with_notes]
        }

        values$filtered_docs <- doc_ids

        # Select first document
        if (length(doc_ids) > 0) {
          values$selected_doc_id <- doc_ids[1]
        }

        shiny::showNotification(
          paste("Found", length(doc_ids), "documents"),
          type = "message",
          duration = 2
        )
      } else {
        values$filtered_docs <- character(0)
        values$selected_doc_id <- NULL
        shiny::showNotification("No documents match the filter", type = "warning", duration = 2)
      }

      values$filter_applied <- TRUE
    }

    # ===== RESET FILTER =====
    shiny::observeEvent(input$reset_filter_btn, {
      shiny::updateSelectInput(session, "schema_select", selected = "")

      # Reset to classified documents (at least one classification)
      classified_docs <- get_docids(dir_r(), "Classified")
      values$filtered_docs <- classified_docs
      values$filter_applied <- FALSE

      # Select first document if available
      if (length(classified_docs) > 0) {
        values$selected_doc_id <- classified_docs[1]
      } else {
        values$selected_doc_id <- NULL
      }

      shiny::showNotification("Showing all classified documents", type = "message", duration = 2)
    })

    # ===== FILTER SUMMARY =====
    output$filter_summary <- shiny::renderText({
      if (is.null(values$filtered_docs) || length(values$filtered_docs) == 0) {
        return("No documents found")
      }

      # Check if filters are active
      has_filters <- (!is.null(input$schema_select) && input$schema_select != "") ||
        (!is.null(input$class_select) && input$class_select != "") ||
        (!is.null(input$value_select) && length(input$value_select) > 0) ||
        (!is.null(input$notes_filter) && input$notes_filter)

      sch <- schema_r()

      filter_text <- paste0(
        "Showing: ", length(values$filtered_docs), " documents\n\n"
      )

      if (!has_filters) {
        filter_text <- paste0(filter_text, "Filter: All classified documents\n")
      } else {
        if (!is.null(input$schema_select) && input$schema_select != "") {
          schema_name <- sch[[input$schema_select]]$name
          filter_text <- paste0(filter_text, "Schema: ", schema_name, "\n")
        }

        if (!is.null(input$class_select) && input$class_select != "") {
          filter_text <- paste0(filter_text, "Class: ", input$class_select, "\n")
        }

        if (!is.null(input$value_select) && length(input$value_select) > 0) {
          filter_text <- paste0(
            filter_text,
            "Values: ", paste(input$value_select, collapse = ", "), "\n"
          )
        }

        if (!is.null(input$notes_filter) && input$notes_filter) {
          filter_text <- paste0(filter_text, "Notes: Only documents with notes\n")
        }
      }

      filter_text
    })

    # ===== MARKING FUNCTIONALITY =====

    # Handle individual checkbox clicks
    shiny::observeEvent(input$doc_checkbox, {
      if (!is.null(marked_docs)) {
        checkbox_info <- input$doc_checkbox
        doc_id <- checkbox_info$id
        is_checked <- checkbox_info$checked

        if (is_checked) {
          marked_docs$ids <- unique(c(marked_docs$ids, doc_id))
        } else {
          marked_docs$ids <- setdiff(marked_docs$ids, doc_id)
        }
      }
    })

    # Mark all visible documents
    shiny::observeEvent(input$mark_all_visible, {
      if (!is.null(marked_docs)) {
        docs <- get_displayed_docs()
        if (length(docs) > 0) {
          marked_docs$ids <- unique(c(marked_docs$ids, docs))
          shiny::showNotification(
            paste("Marked", length(docs), "documents"),
            type = "message",
            duration = 2
          )
        }
      }
    })

    # Unmark all visible documents
    shiny::observeEvent(input$unmark_all_visible, {
      if (!is.null(marked_docs)) {
        docs <- get_displayed_docs()
        if (length(docs) > 0) {
          marked_docs$ids <- setdiff(marked_docs$ids, docs)
          shiny::showNotification(
            paste("Unmarked", length(docs), "documents"),
            type = "message",
            duration = 2
          )
        }
      }
    })

    # Clear all marks
    shiny::observeEvent(input$clear_marks_browser, {
      if (!is.null(marked_docs)) {
        count <- length(marked_docs$ids)
        marked_docs$ids <- character(0)
        shiny::showNotification(
          paste("Cleared", count, "marks"),
          type = "message",
          duration = 2
        )
      }
    })

    # ===== DOCUMENT NOTES DISPLAY =====
    output$document_notes_display <- shiny::renderUI({
      if (is.null(values$selected_doc_id)) {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "Select a document to view notes"
        ))
      }

      note_df <- read_note(dir_r(), values$selected_doc_id)

      if (nrow(note_df) == 0) {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "No notes for this document"
        ))
      }

      # Display note with metadata
      shiny::div(
        class = "notes-display",
        shiny::div(
          style = "font-size: 11px; color: #666; margin-bottom: 10px;",
          shiny::strong("Last edited by: "), note_df$UserID[1], " | ",
          shiny::strong("Timestamp: "), format(note_df$Timestamp[1], "%Y-%m-%d %H:%M")
        ),
        shiny::div(
          style = "background-color: #fffbea; padding: 15px; border-radius: 5px; border-left: 3px solid #f39c12; white-space: pre-wrap;",
          note_df$NoteText[1]
        )
      )
    })

    # ===== DOCUMENT LIST UI WITH CHECKBOXES =====
    get_displayed_docs <- shiny::reactive({
      if (is.null(values$filtered_docs)) {
        return(character(0))
      }

      docs <- values$filtered_docs

      # Apply search filter
      if (!is.null(input$doc_search) && input$doc_search != "") {
        search_term <- tolower(trimws(input$doc_search))
        docs <- docs[grepl(search_term, tolower(docs))]
      }

      docs
    })

    output$document_list_ui <- shiny::renderUI({
      docs <- get_displayed_docs()

      if (length(docs) == 0) {
        return(shiny::div(
          style = "text-align: center; color: #999; padding: 20px;",
          if (!is.null(input$doc_search) && input$doc_search != "") {
            "No documents match your search"
          } else {
            "No documents found"
          }
        ))
      }

      # Get marked status
      marked_ids <- if (!is.null(marked_docs)) marked_docs$ids else character(0)

      # Create document items with checkboxes
      doc_items <- lapply(docs, function(doc_id) {
        is_selected <- identical(values$selected_doc_id, doc_id)
        is_marked <- doc_id %in% marked_ids

        shiny::div(
          class = if (is_selected) "doc-item doc-item-selected" else "doc-item",
          style = "display: flex; align-items: center; gap: 10px;",

          # Checkbox
          if (!is.null(marked_docs)) {
            shiny::tags$input(
              type = "checkbox",
              checked = if (is_marked) NA else NULL,
              onclick = sprintf(
                "Shiny.setInputValue('%s', {id: '%s', checked: this.checked}, {priority: 'event'})",
                session$ns("doc_checkbox"),
                doc_id
              ),
              style = "cursor: pointer; width: 16px; height: 16px; flex-shrink: 0;"
            )
          } else {
            NULL
          },

          # Document ID (clickable)
          shiny::div(
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              session$ns("doc_clicked"),
              doc_id
            ),
            style = "flex: 1; cursor: pointer;",
            doc_id
          )
        )
      })

      shiny::div(doc_items)
    })

    # Handle document selection
    shiny::observeEvent(input$doc_clicked, {
      values$selected_doc_id <- input$doc_clicked
    })

    # ===== DOCUMENT VIEWER =====
    output$viewer_doc_id <- shiny::renderText({
      if (is.null(values$selected_doc_id)) {
        "No document selected"
      } else {
        paste("Document:", values$selected_doc_id)
      }
    })

    output$document_content <- shiny::renderUI({
      if (is.null(values$selected_doc_id)) {
        return(shiny::div(
          style = "text-align: center; color: #999; padding: 50px;",
          shiny::h4("No document selected"),
          shiny::p("Select a document from the list to view")
        ))
      }

      doc_data <- read_single_document(dir_r(), values$selected_doc_id)

      if (nrow(doc_data) == 0) {
        return(shiny::div(
          style = "text-align: center; color: #999; padding: 50px;",
          shiny::h4("Document not found")
        ))
      }

      shiny::HTML(doc_data$HTML[1])
    })

    # ===== DOCUMENT CLASSIFICATIONS =====
    output$document_classifications <- shiny::renderUI({
      if (is.null(values$selected_doc_id)) {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "Select a document to view classifications"
        ))
      }

      class_df <- read_classification(dir_r(), values$selected_doc_id)

      if (nrow(class_df) == 0) {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "No classifications for this document"
        ))
      }

      sch <- schema_r()
      class_nested <- normalize_to_nested(class_df, sch)

      # Build classification display
      class_displays <- lapply(names(class_nested), function(schema_id) {
        schema_classes <- class_nested[[schema_id]]
        schema_name <- sch[[schema_id]]$name

        # Filter out empty classes
        non_empty <- names(schema_classes)[sapply(schema_classes, length) > 0]

        if (length(non_empty) == 0) {
          return(NULL)
        }

        class_items <- lapply(non_empty, function(class_name) {
          values <- schema_classes[[class_name]]

          shiny::div(
            style = "margin-bottom: 10px;",
            shiny::strong(class_name, ":"),
            shiny::span(
              style = "margin-left: 10px;",
              paste(values, collapse = ", ")
            )
          )
        })

        shiny::div(
          class = "classification-display",
          shiny::h6(schema_name, style = "color: #3498db; margin-bottom: 10px;"),
          class_items
        )
      })

      # Remove NULL entries
      class_displays <- class_displays[!sapply(class_displays, is.null)]

      if (length(class_displays) == 0) {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "No classifications for this document"
        ))
      }

      shiny::tagList(class_displays)
    })

  })
}

#' CSS for Browser Interface
#'
#' Returns CSS styling for browser components.
#'
#' @return HTML head tag with CSS
#' @keywords internal
browser_css <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* Sidebar styling */
      .sidebar {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        height: 100%;
      }

      .filter-section {
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 1px solid #dee2e6;
      }

      .filter-section:last-of-type {
        border-bottom: none;
      }

      .filter-section h5 {
        margin-bottom: 10px;
        color: #2c3e50;
        font-size: 14px;
        font-weight: 600;
      }

      /* Document list panel */
      .document-list-panel {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        height: 100%;
      }

      .doc-list-container {
        height: 600px;
        overflow-y: auto;
        margin-top: 15px;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        background: white;
      }

      .doc-item {
        padding: 12px 15px;
        border-bottom: 1px solid #e9ecef;
        transition: background-color 0.2s;
      }

      .doc-item:hover {
        background-color: #e3f2fd;
      }

      .doc-item-selected {
        background-color: #2196f3 !important;
        color: white;
        font-weight: 500;
      }

      .doc-item-selected input[type='checkbox'] {
        filter: brightness(0) invert(1);
      }

      .doc-item:last-child {
        border-bottom: none;
      }

      /* Document viewer */
      .document-viewer {
        height: 600px;
        overflow-y: auto;
        padding: 20px;
        border: 1px solid #ddd;
        background: white;
        border-radius: 5px;
      }

      .viewer-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      /* Classification display */
      .classification-display {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
        border-left: 3px solid #3498db;
      }

      /* Well panels */
      .well {
        background-color: white;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 20px;
        margin-bottom: 15px;
      }
    "))
  )
}
