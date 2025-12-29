# UI HELPERS
# This file contains functions for generating dynamic UI components

#' Generate Classification Dropdown
#'
#' Creates a classification dropdown. Always uses multi-select internally
#' to preserve selections when switching modes. Mode only affects placeholder text.
#'
#' @param ns Namespace function from module
#' @param schema_id Schema ID
#' @param class_name Class name
#' @param values Available values for this class
#' @param selected_values Currently selected values
#' @param mode "single" or "multi" (affects hint only, not behavior)
#' @return Shiny selectizeInput
#' @keywords internal
generate_classification_dropdown <- function(ns, schema_id, class_name, values, selected_values, mode = "multi") {
  # Create unique dropdown ID
  dropdown_id <- paste0("dropdown_", schema_id, "_", class_name)

  # Always use multi-select - mode only affects placeholder text
  placeholder <- if (mode == "single") {
    "Select one value..."
  } else {
    "Select one or more..."
  }

  shiny::selectizeInput(
    inputId = ns(dropdown_id),
    label = NULL,
    choices = values,
    selected = selected_values,  # Keep ALL selections regardless of mode
    multiple = TRUE,
    width = "100%",
    options = list(
      placeholder = placeholder,
      plugins = list("remove_button")
    )
  )
}

#' Generate Multi-Selection Warning
#'
#' Creates warning badge when in single mode but multiple values selected.
#'
#' @param selections_nested Nested list of current selections
#' @param schema Schema list
#' @param mode Current selection mode
#' @return Shiny UI element or NULL
#' @export
generate_multiselect_warning <- function(selections_nested, schema, mode) {
  if (mode != "single") return(NULL)

  # Find classes with >1 selection
  warnings <- list()

  for (schema_id in names(selections_nested)) {
    schema_classes <- selections_nested[[schema_id]]
    if (is.null(schema_classes)) next

    for (class_name in names(schema_classes)) {
      values <- schema_classes[[class_name]]
      if (length(values) > 1) {
        schema_name <- schema[[schema_id]]$name
        warnings[[length(warnings) + 1]] <- paste0(schema_name, " -> ", class_name)
      }
    }
  }

  if (length(warnings) == 0) return(NULL)

  shiny::div(
    class = "alert alert-warning",
    style = "padding: 8px 12px; margin-bottom: 10px; font-size: 13px;",
    shiny::icon("exclamation-triangle"),
    " Single mode active, but multiple values selected in: ",
    shiny::strong(paste(warnings, collapse = ", "))
  )
}


#' Generate Schema Tab Content
#'
#' Creates the classification interface for a single schema with dropdowns.
#'
#' @param ns Namespace function from module
#' @param schema_id Schema ID
#' @param schema Schema definition (from read_schema())
#' @param current_selections Named list of currently selected values
#' @param mode "single" or "multi"
#' @return Shiny tag list with classification UI
#' @keywords internal
generate_schema_tab_content <- function(ns, schema_id, schema, current_selections, mode = "multi") {
  class_groups <- lapply(names(schema$classes), function(class_name) {
    # Get values for this class
    values <- schema$classes[[class_name]]

    # Get current selections for this class (using %||% for NULL handling)
    selected_values <- current_selections[[class_name]] %||% character(0)

    # Generate dropdown
    dropdown <- generate_classification_dropdown(
      ns,
      schema_id,
      class_name,
      values,
      selected_values,
      mode
    )

    # Create class group
    shiny::div(
      class = "classification-group",
      shiny::h5(class_name, class = "classification-title"),
      dropdown
    )
  })

  shiny::tagList(class_groups)
}

#' Generate Schema Tab Badge
#'
#' Creates completion badge for schema tab label.
#'
#' @param schema_id Schema ID
#' @param schema_name Schema name
#' @param completed_classes Number of completed classes
#' @param total_classes Total number of classes
#' @return Character string for tab title
#' @keywords internal
generate_schema_tab_badge <- function(schema_id, schema_name, completed_classes, total_classes) {
  if (completed_classes == total_classes && total_classes > 0) {
    icon <- "\u2713"
  } else {
    icon <- "\u26A0"
  }

  paste0(schema_name, " (", completed_classes, "/", total_classes, ") ", icon)
}

#' Generate Classification Display
#'
#' Creates a read-only display of classifications (same style as Browser tab).
#'
#' @param classification_nested Nested list of classifications
#' @param schema Schema list
#' @return Shiny UI for classification display
#' @export
generate_classification_display <- function(classification_nested, schema) {
  # Build classification display
  class_displays <- lapply(names(classification_nested), function(schema_id) {
    schema_classes <- classification_nested[[schema_id]]
    schema_name <- schema[[schema_id]]$name

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
    return(render_empty_state("No classifications for this document"))
  }

  shiny::tagList(class_displays)
}

#' Render Note Display
#'
#' Creates consistent read-only display of document notes with metadata.
#'
#' @param note_df Data frame with note data (DocID, UserID, Timestamp, NoteText)
#' @param empty_message Message to show when no note exists
#' @return Shiny UI element
#' @export
render_note_display <- function(note_df, empty_message = "No notes for this document") {
  if (is.null(note_df) || nrow(note_df) == 0) {
    return(render_empty_state(empty_message))
  }

  shiny::div(
    class = "notes-display",
    shiny::div(
      style = "font-size: 11px; color: #666; margin-bottom: 10px;",
      shiny::strong("Last edited by: "), note_df$UserID[1], " | ",
      shiny::strong("Timestamp: "), format_timestamp(note_df$Timestamp[1], include_seconds = FALSE)
    ),
    shiny::div(
      style = "background-color: #fffbea; padding: 15px; border-radius: 5px; border-left: 3px solid #f39c12; white-space: pre-wrap;",
      note_df$NoteText[1]
    )
  )
}

#' Render Empty State
#'
#' Creates consistent empty state UI element.
#'
#' @param message Message to display
#' @param icon Optional icon name (fontawesome)
#' @return Shiny UI element
#' @export
render_empty_state <- function(message, icon = NULL) {
  shiny::div(
    style = "color: #999; font-style: italic; text-align: center; padding: 15px;",
    if (!is.null(icon)) shiny::icon(icon),
    message
  )
}

#' Generate Filter Dropdown Choices with Counts
#'
#' Creates choices list for document filter dropdown with document counts.
#'
#' @param .dir Path to project directory
#' @return Named character vector for selectInput choices
#' @export
generate_filter_choices <- function(.dir) {
  counts <- get_filter_counts(.dir)

  c(
    "all" = paste0("All Documents (", counts$all, ")"),
    "classified" = paste0("Classified (", counts$classified, ")"),
    "unclassified" = paste0("Unclassified (", counts$unclassified, ")")
  )
}

#' Get Schema Tab ID
#'
#' Generates consistent tab ID for a schema.
#'
#' @param schema_id Schema ID
#' @return Character tab ID
#' @keywords internal
get_schema_tab_id <- function(schema_id) {
  paste0("schema_", schema_id)
}


#' Format Selection Mode Label
#'
#' Creates descriptive text for selection mode.
#'
#' @param mode "single" or "multi"
#' @return Character description
#' @keywords internal
format_selection_mode <- function(mode) {
  if (mode == "single") {
    "Single mode: Recommended one value per class (multiple still allowed)"
  } else {
    "Multi mode: Select multiple values per class"
  }
}



#' Load Application CSS
#'
#' Embeds CSS styles directly in the application.
#'
#' @return HTML head tag with embedded CSS
#' @export
load_app_css <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
/* ==========================================
   DOCUMENT CLASSIFICATION SYSTEM - STYLES
   ========================================== */

/* ==========================================
   SHARED STYLES
   ========================================== */

/* Well panels */
.well {
  background-color: white;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  padding: 20px;
  margin-bottom: 20px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.05);
}

.well h4, .well h5 {
  margin-top: 0;
  margin-bottom: 15px;
  color: #2c3e50;
  font-weight: 600;
}

/* Document viewer header */
.viewer-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

/* Classification display (read-only) */
.classification-display {
  background-color: #f8f9fa;
  padding: 15px;
  border-radius: 5px;
  margin-bottom: 10px;
  border-left: 3px solid #3498db;
}

/* ==========================================
   CLASSIFICATION TAB STYLES
   ========================================== */

/* Sidebar styling */
.sidebar {
  background-color: #f8f9fa;
  padding: 20px;
  border-radius: 5px;
}

/* Filter panel */
.filter-panel {
  background-color: #e9ecef;
  padding: 10px;
  margin-bottom: 10px;
  border-radius: 5px;
}

/* Filter buttons */
.filter-btn {
  font-weight: 500;
  transition: all 0.2s;
}

.filter-btn:hover {
  opacity: 0.8;
}

.filter-btn-active {
  background-color: #007bff !important;
  color: white !important;
  border-color: #007bff !important;
}

/* Selection mode toggle */
.selection-mode-panel {
  background-color: #fff3cd;
  padding: 10px;
  margin-bottom: 10px;
  border-radius: 5px;
  border: 1px solid #ffc107;
}

/* Classification groups and dropdowns */
.classification-group {
  margin-bottom: 15px;
}

.classification-title {
  color: #2c3e50;
  margin-bottom: 10px;
  font-weight: 600;
  font-size: 14px;
}

/* Selectize styling */
.selectize-input {
  border: 1px solid #ced4da;
  border-radius: 4px;
  padding: 6px 8px;
  font-size: 14px;
}

.selectize-input.focus {
  border-color: #80bdff;
  box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25);
}

.selectize-dropdown {
  border: 1px solid #ced4da;
  border-radius: 4px;
  font-size: 14px;
}

/* Tags in multi-select */
.selectize-input .item {
  background-color: #3498db;
  color: white;
  border: none;
  padding: 2px 8px;
  margin: 2px;
  border-radius: 3px;
}

.selectize-input .remove {
  border-left: 1px solid rgba(255,255,255,0.3);
  padding-left: 5px;
  margin-left: 5px;
}

/* Notes section styling */
.notes-section {
  margin-top: 10px;
}

.notes-section textarea {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  font-size: 13px;
  line-height: 1.5;
  resize: vertical !important;
  min-height: 100px;
  max-height: 400px;
}

.notes-info {
  font-size: 11px;
  color: #666;
  font-style: italic;
}

/* Notes display (read-only) */
.notes-display {
  background-color: #fffbea;
  padding: 15px;
  border-radius: 5px;
  margin-top: 15px;
  border-left: 3px solid #f39c12;
}

.notes-display .note-metadata {
  font-size: 11px;
  color: #666;
  margin-bottom: 10px;
  border-bottom: 1px solid #f0e6c8;
  padding-bottom: 8px;
}

.notes-display .note-text {
  white-space: pre-wrap;
  word-wrap: break-word;
  color: #2c3e50;
}

/* Document content viewer - Classification tab uses this */
.document-content {
  height: 600px;
  overflow-y: auto;
  padding: 20px;
  border: 1px solid #ddd;
  background: white;
  border-radius: 5px;
}

/* Schema tabs */
.nav-tabs .nav-link.active {
  font-weight: 600;
  background-color: #ffffff !important;
  border-color: #dee2e6 #dee2e6 #fff !important;
}

/* Action buttons */
.action-buttons {
  margin-top: 15px;
}

.action-buttons .btn {
  margin: 2px;
}

/* ==========================================
   BROWSER TAB STYLES
   ========================================== */

/* Filter section */
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
  cursor: pointer;
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

/* Document viewer - Browser tab uses this */
.document-viewer {
  height: 600px;
  overflow-y: auto;
  padding: 20px;
  border: 1px solid #ddd;
  background: white;
  border-radius: 5px;
}

/* ==========================================
   OVERVIEW TAB STYLES
   ========================================== */

/* Metric boxes */
.metric-box {
  background: white;
  padding: 20px;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  text-align: center;
  margin-bottom: 20px;
}

.metric-box h3 {
  font-size: 36px;
  font-weight: bold;
  color: #2c3e50;
}

.metric-total { border-left: 4px solid #3498db; }
.metric-classified { border-left: 4px solid #27ae60; }
.metric-remaining { border-left: 4px solid #e74c3c; }
.metric-percent { border-left: 4px solid #f39c12; }
    ")),
    # JavaScript for filter button updates
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('updateFilterButtons', function(active) {
        // Remove active class from all filter buttons
        $('.filter-btn').removeClass('filter-btn-active');
        $('.filter-btn').css({
          'background-color': '#ffffff',
          'color': '#000000',
          'border': '1px solid #ced4da'
        });

        // Add active class to the clicked button
        var buttonId = '#classification-filter_' + active;
        $(buttonId).addClass('filter-btn-active');
        $(buttonId).css({
          'background-color': '#007bff',
          'color': 'white',
          'border': '1px solid #007bff'
        });
      });
    "))
  )
}
