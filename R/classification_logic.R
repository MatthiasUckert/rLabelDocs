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

#' Get Overall Progress Statistics
#'
#' Calculates project-wide classification progress.
#'
#' @param .dir Path to project directory
#' @return List with components:
#'   - total_documents: Total document count
#'   - classified_documents: Number with any classification
#'   - unclassified_documents: Number with no classification
#'   - percentage_complete: Percentage classified (0-100)
#' @export
get_progress_stats <- function(.dir) {
  total_docs <- get_document_count(.dir)

  file_class <- file.path(.dir, "ClassificationDetails.parquet")
  if (!file.exists(file_class) || total_docs == 0) {
    return(list(
      total_documents = total_docs,
      classified_documents = 0,
      unclassified_documents = total_docs,
      percentage_complete = 0
    ))
  }

  classified_docs <- arrow::open_dataset(file_class) %>%
    dplyr::select(DocID) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    nrow()

  list(
    total_documents = total_docs,
    classified_documents = classified_docs,
    unclassified_documents = total_docs - classified_docs,
    percentage_complete = round(classified_docs / total_docs * 100, 1)
  )
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

#' Format Document Info Display (Full version - kept for backwards compatibility)
#'
#' Creates formatted text for document info panel.
#'
#' @param .dir Path to project directory
#' @param .doc_id Current document ID (can be NULL)
#' @param .current_index Current position in filtered list
#' @param .total_filtered Total documents in filtered list
#' @param .filter_type Current filter type ("all", "classified", "unclassified")
#' @param schema Schema list from read_schema()
#' @param classification_nested Current classification state (nested list)
#' @return Character string with formatted info
#' @export
format_document_info <- function(.dir, .doc_id, .current_index, .total_filtered,
                                 .filter_type, schema, classification_nested) {
  if (is.null(.doc_id)) {
    return(paste0(
      "No documents available\n",
      "Filter: ", .filter_type
    ))
  }

  # Get progress stats
  progress <- get_progress_stats(.dir)

  # Calculate schema completion
  completion <- calculate_schema_completion(classification_nested, schema)

  # Format completion status
  completion_text <- sapply(seq_len(nrow(completion)), function(i) {
    status_icon <- if (completion$all_complete[i]) "\u2713" else "\u26A0"
    paste0(
      "  Schema ", completion$schema_id[i], ": ",
      completion$schema_name[i], " ",
      status_icon, " ",
      completion$completed_classes[i], "/", completion$total_classes[i], " classes"
    )
  })

  # Build complete info string
  paste0(
    "Document: ", .doc_id, "\n",
    "Position: ", .current_index, " of ", .total_filtered, "\n",
    "Progress: ", progress$classified_documents, "/", progress$total_documents,
    " (", progress$percentage_complete, "%)\n\n",
    "Classification Status:\n",
    paste(completion_text, collapse = "\n")
  )
}

#' Validate Classifications List
#'
#' Checks that classifications list matches schema structure.
#'
#' @param classifications_list Nested list of classifications
#' @param schema Schema list from read_schema()
#' @return List with valid=TRUE/FALSE and message
#' @keywords internal
validate_classifications_list <- function(classifications_list, schema) {
  # Check structure
  if (!is.list(classifications_list)) {
    return(list(
      valid = FALSE,
      message = "Classifications must be a list"
    ))
  }

  # Check that all schema IDs are valid
  for (schema_id in names(classifications_list)) {
    if (!schema_id %in% names(schema)) {
      return(list(
        valid = FALSE,
        message = paste("Invalid schema ID:", schema_id)
      ))
    }

    schema_classes <- classifications_list[[schema_id]]
    if (!is.list(schema_classes)) {
      return(list(
        valid = FALSE,
        message = paste("Schema", schema_id, "classifications must be a list")
      ))
    }

    # Check that all classes are valid
    for (class_name in names(schema_classes)) {
      if (!class_name %in% names(schema[[schema_id]]$classes)) {
        return(list(
          valid = FALSE,
          message = paste("Invalid class name:", class_name, "in schema", schema_id)
        ))
      }

      # Check that all values are valid
      selected_values <- schema_classes[[class_name]]
      allowed_values <- schema[[schema_id]]$classes[[class_name]]

      for (value in selected_values) {
        if (!value %in% allowed_values) {
          return(list(
            valid = FALSE,
            message = paste(
              "Invalid value:", value,
              "for class", class_name,
              "in schema", schema_id
            )
          ))
        }
      }
    }
  }

  return(list(
    valid = TRUE,
    message = "Classifications valid"
  ))
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
