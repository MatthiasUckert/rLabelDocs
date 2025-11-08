# UI HELPERS
# This file contains functions for generating dynamic UI components

#' Generate Classification Dropdown
#'
#' Creates a classification dropdown (single or multi-select based on mode).
#'
#' @param ns Namespace function from module
#' @param schema_id Schema ID
#' @param class_name Class name
#' @param values Available values for this class
#' @param selected_values Currently selected values
#' @param mode "single" or "multi"
#' @return Shiny input element (selectInput or selectizeInput)
#' @keywords internal
generate_classification_dropdown <- function(ns, schema_id, class_name, values, selected_values, mode = "multi") {
  # Create unique dropdown ID
  dropdown_id <- paste0("dropdown_", schema_id, "_", class_name)

  if (mode == "single") {
    # Single-select dropdown
    shiny::selectInput(
      inputId = ns(dropdown_id),
      label = NULL,
      choices = c("-- Select --" = "", values),
      selected = if (length(selected_values) > 0) selected_values[1] else "",
      width = "100%"
    )
  } else {
    # Multi-select dropdown with tags
    shiny::selectizeInput(
      inputId = ns(dropdown_id),
      label = NULL,
      choices = values,
      selected = selected_values,
      multiple = TRUE,
      width = "100%",
      options = list(
        placeholder = "Select one or more...",
        plugins = list("remove_button")
      )
    )
  }
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
    return(shiny::div(
      style = "color: #999; font-style: italic;",
      "No classifications for this document"
    ))
  }

  shiny::tagList(class_displays)
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

#' Load External CSS
#'
#' Loads the external CSS file for the application.
#'
#' @return HTML head tag with CSS link
#' @export
load_app_css <- function() {
  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
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

#' Format Selection Mode Label
#'
#' Creates descriptive text for selection mode.
#'
#' @param mode "single" or "multi"
#' @return Character description
#' @keywords internal
format_selection_mode <- function(mode) {
  if (mode == "single") {
    "Single-select mode: Choose one value per class"
  } else {
    "Multi-select mode: Choose multiple values per class"
  }
}
