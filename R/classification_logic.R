# CLASSIFICATION LOGIC
# This file contains business logic for:
# - Converting between data formats
# - Calculating completion status
# - Managing classification state

#' Convert Normalized Classifications to Nested List
#'
#' Converts database format (one row per value) to UI format (nested list).
#'
#' @param classification_df Data frame from read_classification()
#' @param schema Schema list from read_schema()
#' @return Nested list structure:
#'   list(
#'     "1" = list(
#'       DocClasses = c("Value1", "Value2"),
#'       Amendment = c("Amended")
#'     ),
#'     "2" = list(...)
#'   )
#' @export
normalize_to_nested <- function(classification_df, schema) {
  result <- list()

  # Initialize empty structure based on schema
  for (schema_id in names(schema)) {
    result[[schema_id]] <- list()
    for (class_name in names(schema[[schema_id]]$classes)) {
      result[[schema_id]][[class_name]] <- character(0)
    }
  }

  # If no classifications, return empty structure
  if (nrow(classification_df) == 0) {
    return(result)
  }

  # Populate with actual values
  for (i in seq_len(nrow(classification_df))) {
    schema_id <- as.character(classification_df$Schema[i])
    class_name <- classification_df$Class[i]
    value <- classification_df$Value[i]

    # Only add if schema/class exists in schema definition
    if (schema_id %in% names(result) && class_name %in% names(result[[schema_id]])) {
      result[[schema_id]][[class_name]] <- c(
        result[[schema_id]][[class_name]],
        value
      )
    }
  }

  # Remove duplicates in each vector
  for (schema_id in names(result)) {
    for (class_name in names(result[[schema_id]])) {
      result[[schema_id]][[class_name]] <- unique(result[[schema_id]][[class_name]])
    }
  }

  return(result)
}

#' Calculate Schema Completion Status
#'
#' Determines completion status for each schema based on classification data.
#'
#' @param classification_nested Nested list from normalize_to_nested()
#' @param schema Schema list from read_schema()
#' @return Data frame with columns: schema_id, completed_classes, total_classes, all_complete
#' @export
calculate_schema_completion <- function(classification_nested, schema) {
  results <- list()

  for (schema_id in names(schema)) {
    total_classes <- length(schema[[schema_id]]$classes)

    # Count classes with at least one value
    completed_classes <- 0
    if (schema_id %in% names(classification_nested)) {
      for (class_name in names(schema[[schema_id]]$classes)) {
        if (class_name %in% names(classification_nested[[schema_id]])) {
          values <- classification_nested[[schema_id]][[class_name]]
          if (length(values) > 0 && !all(is.na(values)) && !all(values == "")) {
            completed_classes <- completed_classes + 1
          }
        }
      }
    }

    results[[length(results) + 1]] <- data.frame(
      schema_id = schema_id,
      schema_name = schema[[schema_id]]$name,
      completed_classes = completed_classes,
      total_classes = total_classes,
      all_complete = completed_classes == total_classes,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(results)
}

#' Check if Document is Classified
#'
#' Determines if a document has any classifications.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to check
#' @return Logical TRUE if document has any classifications
#' @export
is_document_classified <- function(.dir, .doc_id) {
  classified_ids <- get_docids(.dir, "Classified")
  .doc_id %in% classified_ids
}

#' Format Document Info Display (Simplified)
#'
#' Creates simplified formatted text for document info panel.
#' Only shows position and overall progress.
#'
#' @param .current_index Current position in filtered list
#' @param .total_filtered Total documents in filtered list
#' @param .dir Path to project directory
#' @return Character string with formatted info
#' @export
format_document_info_simple <- function(.current_index, .total_filtered, .dir) {
  # Get progress stats
  progress <- get_progress_stats(.dir)

  # Build simple info string
  paste0(
    "Position: ", .current_index, " of ", .total_filtered, "\n",
    "Progress: ", progress$classified_documents, "/", progress$total_documents,
    " (", progress$percentage_complete, "%)"
  )
}

#' Get Filter Counts
#'
#' Returns document counts for each filter type.
#'
#' @param .dir Path to project directory
#' @return Named list with counts for: all, classified, unclassified
#' @export
get_filter_counts <- function(.dir) {
  all_ids <- get_docids(.dir, "All")
  classified_ids <- get_docids(.dir, "Classified")
  unclassified_ids <- get_docids(.dir, "Unclassified")

  list(
    all = length(all_ids),
    classified = length(classified_ids),
    unclassified = length(unclassified_ids)
  )
}
