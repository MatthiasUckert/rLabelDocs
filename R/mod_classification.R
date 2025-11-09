# CLASSIFICATION MODULE
# Main module for document classification workflow

#' Classification Module UI
#'
#' Generates the user interface for the classification tab.
#'
#' @param id Module namespace ID
#' @return Shiny UI
#' @export
mod_classification_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    load_app_css(),
    shiny::fluidRow(

      # ===== LEFT SIDEBAR (3 columns)
      shiny::column(
        3,
        shiny::div(
          class = "sidebar",

          # --- Filter Panel (custom button group with 4 buttons now)
          shiny::div(
            class = "filter-panel",
            shiny::h5("Document Filter"),
            shiny::div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 5px;",
              shiny::actionButton(
                ns("filter_all"),
                "All Documents",
                class = "filter-btn",
                style = "background-color: #ffffff; border: 1px solid #ced4da;"
              ),
              shiny::actionButton(
                ns("filter_classified"),
                "Classified",
                class = "filter-btn",
                style = "background-color: #ffffff; border: 1px solid #ced4da;"
              ),
              shiny::actionButton(
                ns("filter_unclassified"),
                "Unclassified",
                class = "filter-btn filter-btn-active",
                style = "background-color: #007bff; color: white; border: 1px solid #007bff;"
              ),
              shiny::actionButton(
                ns("filter_marked"),
                "Marked",
                class = "filter-btn",
                style = "background-color: #ffffff; border: 1px solid #ced4da;"
              )
            ),
            # Marked count display
            shiny::div(
              style = "margin-top: 10px; text-align: center; font-size: 12px; color: #666;",
              shiny::textOutput(ns("marked_count_display"))
            )
          ),

          # --- Search Panel ---
          shiny::div(
            class = "filter-panel",
            shiny::h5("Document Search"),
            shiny::textInput(
              ns("search_doc_id"),
              label = NULL,
              placeholder = "Enter document ID...",
              value = ""
            ),
            # Search buttons on one line
            shiny::div(
              style = "margin-top: 10px; display: flex; gap: 5px;",
              shiny::actionButton(
                inputId = ns("search_btn"),
                label = "Go to Document",
                class = "btn-primary",
                style = "flex: 1;"
              ),
              shiny::actionButton(
                inputId = ns("clear_search_btn"),
                label = "Clear",
                class = "btn-secondary",
                style = "flex: 1;"
              )
            )
          ),

          # --- Document Info (simplified) ---
          shiny::wellPanel(
            shiny::h5("Document Info"),
            shiny::verbatimTextOutput(ns("doc_info"))
          ),

          # --- Classification Panel ---
          shiny::wellPanel(
            shiny::h4("Classification"),

            # Selection Mode Toggle
            shiny::div(
              class = "selection-mode-panel",
              shiny::radioButtons(
                ns("selection_mode"),
                label = "Selection Mode:",
                choices = c("Single" = "single", "Multi" = "multi"),
                selected = "multi",
                inline = TRUE
              ),
              shiny::div(
                style = "font-size: 12px; color: #856404; margin-top: 5px;",
                shiny::textOutput(ns("selection_mode_help"))
              )
            ),

            # Schema Tabs (dynamic)
            shiny::uiOutput(ns("schema_tabs_ui")),

            # Actions
            shiny::h5("Actions", style = "margin-top: 20px;"),
            shiny::div(
              class = "action-buttons",
              style = "text-align: center;",
              shiny::actionButton(
                ns("save_btn"),
                "Save",
                class = "btn-success",
                style = "width: 32%;"
              ),
              shiny::actionButton(
                ns("prev_btn"),
                "Prev",
                class = "btn-warning",
                style = "width: 32%;"
              ),
              shiny::actionButton(
                ns("next_btn"),
                "Next",
                class = "btn-info",
                style = "width: 32%;"
              )
            )
          ),
          # --- Notes Panel ---
          shiny::wellPanel(
            shiny::h5("Document Notes"),
            shiny::div(
              class = "notes-section",
              shiny::textAreaInput(
                ns("document_notes"),
                label = NULL,
                placeholder = "Add notes for this document...",
                value = "",
                width = "100%",
                height = "100px",
                resize = "vertical"
              ),
              shiny::div(
                class = "notes-info",
                style = "font-size: 11px; color: #666; margin-top: 5px;",
                shiny::textOutput(ns("notes_metadata"))
              )
            )
          ),
        )
      ),

      # ===== RIGHT PANEL (9 columns)
      shiny::column(
        9,
        shiny::wellPanel(
          # Document header with ID
          shiny::div(
            class = "viewer-header",
            shiny::h4("Document Content", style = "display: inline-block; margin: 0;"),
            shiny::div(
              style = "float: right;",
              shiny::textOutput(ns("document_header_id"), inline = TRUE)
            )
          ),
          shiny::hr(),

          # Document content
          shiny::div(
            class = "document-viewer",
            shiny::htmlOutput(ns("document_display"))
          ),

          # Classifications display
          shiny::hr(),
          shiny::h5("Classifications"),
          shiny::uiOutput(ns("document_classifications"))
        )
      )
    )
  )
}

#' Classification Module Server
#'
#' Server logic for the classification module.
#'
#' @param id Module namespace ID
#' @param .dir Reactive or static path to project directory
#' @param .user_id Reactive or static user ID
#' @param schema Reactive or static schema list
#' @param marked_docs ReactiveValues object with marked document IDs
#' @export
mod_classification_server <- function(id, .dir, .user_id, schema, marked_docs = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Make inputs reactive if they're not already
    dir_r <- if (shiny::is.reactive(.dir)) .dir else shiny::reactive(.dir)
    user_id_r <- if (shiny::is.reactive(.user_id)) .user_id else shiny::reactive(.user_id)
    schema_r <- if (shiny::is.reactive(schema)) schema else shiny::reactive(schema)

    # ===== STATE MANAGEMENT
    values <- shiny::reactiveValues(
      current_doc_id = NULL,
      current_doc = NULL,
      filtered_doc_ids = NULL,
      current_index = 1,
      pending_search = NULL,
      doc_filter = "unclassified", # Track current filter
      current_note = NULL
    )

    # ===== MARKED COUNT DISPLAY
    output$marked_count_display <- shiny::renderText({
      if (is.null(marked_docs)) {
        return("")
      }
      count <- length(marked_docs$ids)
      if (count == 0) {
        "No marked documents"
      } else if (count == 1) {
        "1 marked document"
      } else {
        paste(count, "marked documents")
      }
    })

    # ===== FILTER BUTTON HANDLERS
    shiny::observeEvent(input$filter_all, {
      values$doc_filter <- "all"
      update_filter_buttons("all")
    })

    shiny::observeEvent(input$filter_classified, {
      values$doc_filter <- "classified"
      update_filter_buttons("classified")
    })

    shiny::observeEvent(input$filter_unclassified, {
      values$doc_filter <- "unclassified"
      update_filter_buttons("unclassified")
    })

    shiny::observeEvent(input$filter_marked, {
      if (is.null(marked_docs) || length(marked_docs$ids) == 0) {
        shiny::showNotification(
          "No documents marked. Mark documents in the Overview tab first.",
          type = "warning",
          duration = 3
        )
        return()
      }
      values$doc_filter <- "marked"
      update_filter_buttons("marked")
    })

    # Update button styling
    update_filter_buttons <- function(active) {
      # Reset all buttons
      shiny::updateActionButton(session, "filter_all", label = "All Documents", icon = NULL)
      shiny::updateActionButton(session, "filter_classified", label = "Classified", icon = NULL)
      shiny::updateActionButton(session, "filter_unclassified",
        label = "Unclassified",
        icon = NULL
      )
      shiny::updateActionButton(session, "filter_marked",
        label = "Marked",
        icon = NULL
      )

      # Highlight active button using JavaScript
      session$sendCustomMessage("updateFilterButtons", active)
    }

    # Selections stored as nested list: selections$"1"$DocClasses = c("val1", "val2")
    selections <- shiny::reactiveValues()

    # Initialize selections structure based on schema
    shiny::observe({
      sch <- schema_r()
      for (schema_id in names(sch)) {
        if (is.null(selections[[schema_id]])) {
          selections[[schema_id]] <- list()
        }
        for (class_name in names(sch[[schema_id]]$classes)) {
          selections[[schema_id]][[class_name]] <- selections[[schema_id]][[class_name]] %||% character(0)
        }
      }
    })

    # ===== GET FILTERED DOCUMENTS
    get_filtered_documents <- shiny::reactive({
      filter_type <- values$doc_filter

      switch(filter_type,
        "all" = get_docids(dir_r(), "All"),
        "unclassified" = get_docids(dir_r(), "Unclassified"),
        "classified" = get_docids(dir_r(), "Classified"),
        "marked" = marked_docs$ids %||% character(0)
      )
    })

    # ===== UPDATE DOCUMENT LIST WHEN FILTER CHANGES
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

    # ===== LOAD CURRENT DOCUMENT
    load_current_document <- function() {
      if (!is.null(values$current_doc_id)) {
        # Load document content
        values$current_doc <- read_single_document(dir_r(), values$current_doc_id)

        # Load existing classifications
        class_df <- read_classification(dir_r(), values$current_doc_id)
        class_nested <- normalize_to_nested(class_df, schema_r())

        # Update selections
        for (schema_id in names(class_nested)) {
          for (class_name in names(class_nested[[schema_id]])) {
            selections[[schema_id]][[class_name]] <- class_nested[[schema_id]][[class_name]]
          }
        }

        # Load note
        note_df <- read_note(dir_r(), values$current_doc_id)
        if (nrow(note_df) > 0) {
          values$current_note <- note_df
          shiny::updateTextAreaInput(
            session,
            "document_notes",
            value = note_df$NoteText[1]
          )
        } else {
          values$current_note <- NULL
          shiny::updateTextAreaInput(
            session,
            "document_notes",
            value = ""
          )
        }
      }
    }

    # ===== DOCUMENT HEADER ID
    output$document_header_id <- shiny::renderText({
      values$current_doc_id %||% "No document selected"
    })

    # ===== DOCUMENT INFO DISPLAY (SIMPLIFIED)
    output$doc_info <- shiny::renderText({
      format_document_info_simple(
        .current_index = values$current_index,
        .total_filtered = length(values$filtered_doc_ids %||% character()),
        .dir = dir_r()
      )
    })

    # ===== SELECTION MODE HELP TEXT
    output$selection_mode_help <- shiny::renderText({
      format_selection_mode(input$selection_mode)
    })

    # ===== DOCUMENT DISPLAY
    output$document_display <- shiny::renderUI({
      if (is.null(values$current_doc) || nrow(values$current_doc) == 0) {
        shiny::tags$div(
          style = "text-align: center; color: #999; padding: 50px;",
          shiny::tags$h3("No document loaded"),
          shiny::tags$p(paste("Filter:", values$doc_filter))
        )
      } else {
        shiny::HTML(values$current_doc$HTML[1])
      }
    })

    # ===== DOCUMENT CLASSIFICATIONS DISPLAY
    output$document_classifications <- shiny::renderUI({
      if (is.null(values$current_doc_id)) {
        return(shiny::div(
          style = "color: #999; font-style: italic;",
          "No document selected"
        ))
      }

      # Get current selections
      current_selections <- shiny::reactiveValuesToList(selections)
      sch <- schema_r()

      # Build classification display
      generate_classification_display(current_selections, sch)
    })

    output$notes_metadata <- shiny::renderText({
      note <- values$current_note
      if (!is.null(note) && nrow(note) > 0) {
        paste0(
          "Last edited by ", note$UserID[1],
          " on ", format_timestamp(note$Timestamp[1], include_seconds = FALSE)
        )
      } else {
        ""
      }
    })

    # ===== SCHEMA TABS UI (DYNAMIC)
    output$schema_tabs_ui <- shiny::renderUI({
      sch <- schema_r()
      current_selections <- shiny::reactiveValuesToList(selections)
      mode <- input$selection_mode

      # Calculate completion for each schema
      completion <- calculate_schema_completion(current_selections, sch)

      # Create tab panels
      tab_panels <- lapply(seq_len(nrow(completion)), function(i) {
        schema_id <- completion$schema_id[i]

        # Generate tab label with badge
        tab_label <- generate_schema_tab_badge(
          schema_id,
          completion$schema_name[i],
          completion$completed_classes[i],
          completion$total_classes[i]
        )

        # Generate tab content with mode parameter
        tab_content <- generate_schema_tab_content(
          session$ns,
          schema_id,
          sch[[schema_id]],
          current_selections[[schema_id]] %||% list(),
          mode # Pass the mode parameter
        )

        shiny::tabPanel(
          title = tab_label,
          value = get_schema_tab_id(schema_id),
          tab_content
        )
      })

      # Preserve currently active tab when re-rendering
      current_tab <- input$schema_tabs %||% get_schema_tab_id(completion$schema_id[1])

      do.call(shiny::tabsetPanel, c(
        list(
          id = session$ns("schema_tabs"),
          selected = current_tab # Preserve active tab
        ),
        tab_panels
      ))
    })

    # ===== HANDLE DROPDOWN CHANGES
    shiny::observe({
      sch <- schema_r()
      mode <- input$selection_mode

      for (schema_id in names(sch)) {
        for (class_name in names(sch[[schema_id]]$classes)) {
          local({
            local_schema <- schema_id
            local_class <- class_name

            dropdown_id <- paste0("dropdown_", local_schema, "_", local_class)

            shiny::observeEvent(input[[dropdown_id]], {
              new_value <- input[[dropdown_id]]

              if (mode == "single") {
                # Single mode: store as character vector (could be empty string)
                selections[[local_schema]][[local_class]] <- if (is.null(new_value) || length(new_value) == 0 || identical(new_value, "")) {
                  character(0)
                } else {
                  new_value
                }
              } else {
                # Multi mode: store as character vector (could be empty)
                selections[[local_schema]][[local_class]] <- new_value %||% character(0)
              }

              # Optional: Show notification for feedback (safe check for vector)
              if (!is.null(new_value) && length(new_value) > 0 && !identical(new_value, "")) {
                shiny::showNotification(
                  paste("Updated:", local_class),
                  type = "default",
                  duration = 1
                )
              }
            })
          })
        }
      }
    })

    # ===== SAVE BUTTON
    shiny::observeEvent(input$save_btn, {
      current_selections <- shiny::reactiveValuesToList(selections)

      tryCatch(
        {
          # Save classifications
          save_classification(
            dir_r(),
            values$current_doc_id,
            user_id_r(),
            current_selections
          )

          # Save note
          note_text <- input$document_notes
          save_note(
            dir_r(),
            values$current_doc_id,
            user_id_r(),
            note_text
          )

          # Update current note metadata
          if (!is.null(note_text) && nchar(trimws(note_text)) > 0) {
            values$current_note <- data.frame(
              DocID = values$current_doc_id,
              UserID = user_id_r(),
              Timestamp = Sys.time(),
              NoteText = note_text,
              stringsAsFactors = FALSE
            )
          } else {
            values$current_note <- NULL
          }

          shiny::showNotification(
            "Classification and notes saved!",
            type = "message",
            duration = 2
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error saving:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # ===== NAVIGATION BUTTONS
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

    move_to_next <- function() {
      if (values$current_index < length(values$filtered_doc_ids)) {
        values$current_index <- values$current_index + 1
        values$current_doc_id <- values$filtered_doc_ids[values$current_index]
        load_current_document()
      } else {
        shiny::showNotification(
          paste("Reached end of", values$doc_filter, "documents"),
          type = "warning",
          duration = 2
        )
      }
    }

    # ===== DOCUMENT SEARCH
    shiny::observeEvent(input$search_btn, {
      perform_document_search()
    })

    perform_document_search <- function() {
      search_id <- trimws(input$search_doc_id)

      if (search_id == "") {
        shiny::showNotification(
          "Please enter a document ID",
          type = "warning",
          duration = 2
        )
        return()
      }

      # Check if in current filter
      if (search_id %in% values$filtered_doc_ids) {
        found_index <- which(values$filtered_doc_ids == search_id)[1]
        values$current_index <- found_index
        values$current_doc_id <- search_id
        load_current_document()

        shiny::showNotification(
          paste("Found at position", found_index, "of", length(values$filtered_doc_ids)),
          type = "message",
          duration = 2
        )
      } else {
        # Check if exists in other filters
        all_docs <- get_docids(dir_r(), "All")

        if (search_id %in% all_docs) {
          show_filter_switch_modal(search_id)
        } else {
          shiny::showNotification(
            paste("Document", search_id, "not found"),
            type = "error",
            duration = 3
          )
        }
      }
    }

    show_filter_switch_modal <- function(doc_id) {
      classified_docs <- get_docids(dir_r(), "Classified")
      current_filter <- values$doc_filter

      # Check if in marked list
      is_marked <- doc_id %in% (marked_docs$ids %||% character(0))

      if (doc_id %in% classified_docs && current_filter == "unclassified") {
        target_filter <- "classified"
        message <- "Document is classified. Switch to 'Classified' filter?"
      } else if (!doc_id %in% classified_docs && current_filter == "classified") {
        target_filter <- "unclassified"
        message <- "Document is unclassified. Switch to 'Unclassified' filter?"
      } else if (is_marked && current_filter != "marked") {
        target_filter <- "marked"
        message <- "Document is marked. Switch to 'Marked' filter?"
      } else {
        target_filter <- "all"
        message <- "Document exists. Switch to 'All Documents' filter?"
      }

      shiny::showModal(shiny::modalDialog(
        title = "Document in Different Filter",
        message,
        footer = shiny::tagList(
          shiny::actionButton(
            session$ns("switch_filter_yes"),
            "Yes, Switch",
            class = "btn-primary",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s')",
              session$ns("target_filter"), target_filter
            )
          ),
          shiny::modalButton("Cancel")
        )
      ))

      values$pending_search <- doc_id
    }

    shiny::observeEvent(input$target_filter, {
      values$doc_filter <- input$target_filter
      update_filter_buttons(input$target_filter)
      shiny::removeModal()
    })

    # Handle pending search after filter change
    shiny::observe({
      pending <- values$pending_search
      if (!is.null(pending) && pending != "") {
        if (pending %in% values$filtered_doc_ids) {
          found_index <- which(values$filtered_doc_ids == pending)[1]
          values$current_index <- found_index
          values$current_doc_id <- pending
          load_current_document()

          shiny::showNotification(
            paste("Found at position", found_index),
            type = "message",
            duration = 2
          )

          values$pending_search <- NULL
        }
      }
    })

    # Clear search
    shiny::observeEvent(input$clear_search_btn, {
      shiny::updateTextInput(session, "search_doc_id", value = "")
      shiny::showNotification("Search cleared", type = "message", duration = 1)
    })
  })
}
