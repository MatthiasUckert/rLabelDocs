# UI HELPERS
# This file contains functions for generating dynamic UI components

#' Generate Classification Button
#'
#' Creates a classification button with proper styling and ID.
#'
#' @param ns Namespace function from module
#' @param schema_id Schema ID
#' @param class_name Class name
#' @param value Value label
#' @param is_selected Logical, whether button should appear selected
#' @return Shiny button tag
#' @keywords internal
generate_classification_button <- function(ns, schema_id, class_name, value, is_selected = FALSE) {
  # Create unique button ID
  button_id <- paste0("btn_", schema_id, "_", class_name, "_", gsub("[^A-Za-z0-9]", "_", value))

  # Determine button class
  if (is_selected) {
    btn_class <- "btn btn-primary classification-btn selected-btn"
  } else {
    btn_class <- "btn btn-outline-primary classification-btn"
  }

  # Add checkmark if selected
  label <- if (is_selected) paste(value, "\u2713") else value

  shiny::actionButton(
    inputId = ns(button_id),
    label = label,
    class = btn_class
  )
}

#' Generate Schema Tab Content
#'
#' Creates the classification interface for a single schema.
#'
#' @param ns Namespace function from module
#' @param schema_id Schema ID
#' @param schema Schema definition (from read_schema())
#' @param current_selections Named list of currently selected values
#' @return Shiny tag list with classification UI
#' @keywords internal
generate_schema_tab_content <- function(ns, schema_id, schema, current_selections) {
  class_groups <- lapply(names(schema$classes), function(class_name) {
    # Get values for this class
    values <- schema$classes[[class_name]]

    # Get current selections for this class
    selected_values <- current_selections[[class_name]] %||% character(0)

    # Generate buttons
    buttons <- lapply(values, function(value) {
      is_selected <- value %in% selected_values
      generate_classification_button(ns, schema_id, class_name, value, is_selected)
    })

    # Create class group
    shiny::div(
      class = "classification-group",
      shiny::h5(class_name, class = "classification-title"),
      shiny::div(
        class = "classification-buttons",
        buttons
      )
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

#' Generate Filter Dropdown Choices
#'
#' Creates choices list for document filter dropdown with counts.
#'
#' @param .dir Path to project directory
#' @return Named character vector for selectInput choices
#' @export
generate_filter_choices <- function(.dir) {
  counts <- get_filter_counts(.dir)

  c(
    "All Documents" = "all",
    "Unclassified" = "unclassified",
    "Classified" = "classified"
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

#' CSS for Classification Interface
#'
#' Returns CSS styling for classification components.
#'
#' @return HTML head tag with CSS
#' @keywords internal
classification_css <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* Classification groups and buttons */
      .classification-group {
        margin-bottom: 20px;
      }

      .classification-title {
        color: #2c3e50;
        margin-bottom: 10px;
        font-weight: 600;
      }

      .classification-buttons {
        margin-bottom: 15px;
      }

      .classification-btn {
        margin: 3px;
        font-size: 14px;
      }

      .selected-btn {
        background-color: #3498db !important;
        color: white !important;
        border-color: #3498db !important;
        font-weight: 600;
      }

      /* Document content viewer */
      .document-content {
        height: 800px;
        overflow-y: auto;
        padding: 20px;
        border: 1px solid #ddd;
        background: white;
        border-radius: 5px;
      }

      /* Sidebar styling */
      .sidebar {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
      }

      /* Filter panel */
      .filter-panel {
        background-color: #e9ecef;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
      }

      /* Selection mode toggle */
      .selection-mode-panel {
        background-color: #fff3cd;
        padding: 10px;
        margin-bottom: 15px;
        border-radius: 5px;
        border: 1px solid #ffc107;
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

      /* Well panels */
      .well {
        background-color: white;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
      }
    "))
  )
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
    "Single-select mode: Click to select one value per class"
  } else {
    "Multi-select mode: Click to select multiple values per class"
  }
}
