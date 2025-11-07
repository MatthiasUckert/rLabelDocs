# DATA I/O FUNCTIONS
# This file handles all data reading and writing operations for:
# - Schema files (Excel/CSV)
# - Documents (Parquet)
# - Classifications (Parquet)

#' Read Schema File
#'
#' Reads and parses the classification schema from Excel or CSV format.
#'
#' @param .dir Path to project directory
#' @return Nested list structure organized by schema ID:
#'   list(
#'     "1" = list(
#'       name = "Production",
#'       classes = list(
#'         DocClasses = c("Value1", "Value2"),
#'         Amendment = c("Amended", "Original")
#'       )
#'     ),
#'     "2" = list(...)
#'   )
#' @export
read_schema <- function(.dir) {
  # Try Excel first, then CSV
  file_xlsx <- file.path(.dir, "Schema.xlsx")
  file_csv <- file.path(.dir, "Schema.csv")

  if (file.exists(file_xlsx)) {
    schema_df <- readxl::read_excel(file_xlsx)
  } else if (file.exists(file_csv)) {
    schema_df <- readr::read_csv(file_csv, show_col_types = FALSE)
  } else {
    stop("Schema file not found. Expected Schema.xlsx or Schema.csv in: ", .dir, call. = FALSE)
  }

  # Validate required columns
  required_cols <- c("Schema", "Class", "Value")
  if (!all(required_cols %in% names(schema_df))) {
    missing <- setdiff(required_cols, names(schema_df))
    stop("Schema file missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Add SchemaName if missing
  if (!"SchemaName" %in% names(schema_df)) {
    schema_df$SchemaName <- paste("Schema", schema_df$Schema)
  }

  # Convert to nested list structure
  schema_list <- list()

  for (schema_id in unique(schema_df$Schema)) {
    schema_subset <- schema_df[schema_df$Schema == schema_id, ]

    # Get schema name (use first occurrence)
    schema_name <- schema_subset$SchemaName[1]

    # Build classes list
    classes_list <- list()
    for (class_name in unique(schema_subset$Class)) {
      class_values <- schema_subset$Value[schema_subset$Class == class_name]
      classes_list[[class_name]] <- class_values
    }

    schema_list[[as.character(schema_id)]] <- list(
      name = schema_name,
      classes = classes_list
    )
  }

  return(schema_list)
}

#' Read Single Document
#'
#' Retrieves a single document by DocID using Arrow lazy loading.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to retrieve
#' @return Data frame with DocID and HTML columns, or empty data frame if not found
#' @export
read_single_document <- function(.dir, .doc_id) {
  file_docs <- file.path(.dir, "Documents.parquet")

  if (!file.exists(file_docs)) {
    stop("Documents.parquet not found in: ", .dir, call. = FALSE)
  }

  arrow::open_dataset(file_docs) %>%
    dplyr::filter(DocID == .doc_id) %>%
    dplyr::collect()
}

#' Get All Document IDs
#'
#' Retrieves list of document IDs, optionally filtered by classification status.
#'
#' @param .dir Path to project directory
#' @param .type Type filter: "All", "Classified", or "Unclassified"
#' @return Character vector of document IDs
#' @export
get_docids <- function(.dir, .type = c("All", "Classified", "Unclassified")) {
  .type <- match.arg(.type)

  file_docs <- file.path(.dir, "Documents.parquet")
  if (!file.exists(file_docs)) {
    stop("Documents.parquet not found in: ", .dir, call. = FALSE)
  }

  # Get all document IDs
  all_ids <- arrow::open_dataset(file_docs) %>%
    dplyr::select(DocID) %>%
    dplyr::collect() %>%
    dplyr::pull(DocID)

  if (.type == "All") {
    return(all_ids)
  }

  # Get classified document IDs
  file_class <- file.path(.dir, "ClassificationDetails.parquet")
  if (!file.exists(file_class)) {
    classified_ids <- character(0)
  } else {
    classified_ids <- arrow::open_dataset(file_class) %>%
      dplyr::select(DocID) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull(DocID)
  }

  if (.type == "Classified") {
    return(all_ids[all_ids %in% classified_ids])
  } else {
    return(all_ids[!all_ids %in% classified_ids])
  }
}

#' Get Document Count
#'
#' Returns total number of documents without loading all data.
#'
#' @param .dir Path to project directory
#' @return Integer count of documents
#' @export
get_document_count <- function(.dir) {
  file_docs <- file.path(.dir, "Documents.parquet")

  if (!file.exists(file_docs)) {
    return(0)
  }

  arrow::open_dataset(file_docs) %>%
    nrow()
}

#' Read Classifications for Document
#'
#' Loads all classification values for a specific document.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to load classifications for
#' @return Data frame with columns: DocID, UserID, Timestamp, Schema, Class, Value
#'   Returns empty data frame if no classifications exist
#' @export
read_classification <- function(.dir, .doc_id) {
  file_class <- file.path(.dir, "ClassificationDetails.parquet")

  if (!file.exists(file_class)) {
    return(data.frame(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character()),
      Schema = integer(),
      Class = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }

  arrow::open_dataset(file_class) %>%
    dplyr::filter(DocID == .doc_id) %>%
    dplyr::collect()
}

#' Read All Classifications
#'
#' Loads all classifications from the database.
#'
#' @param .dir Path to project directory
#' @return Data frame with all classification records
#' @export
read_all_classifications <- function(.dir) {
  file_class <- file.path(.dir, "ClassificationDetails.parquet")

  if (!file.exists(file_class)) {
    return(data.frame(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character()),
      Schema = integer(),
      Class = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }

  arrow::read_parquet(file_class)
}

#' Save Classification
#'
#' Saves classification values for a document in normalized format.
#' Replaces any existing classifications for this document.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID being classified
#' @param .user_id User making the classification
#' @param .classifications_list Nested list structure:
#'   list(
#'     "1" = list(
#'       DocClasses = c("Value1", "Value2"),
#'       Amendment = c("Amended")
#'     ),
#'     "2" = list(...)
#'   )
#' @return Invisible NULL
#' @export
save_classification <- function(.dir, .doc_id, .user_id, .classifications_list) {
  file_class <- file.path(.dir, "ClassificationDetails.parquet")

  # Convert nested list to normalized data frame
  new_records <- list()
  timestamp <- Sys.time()

  for (schema_id in names(.classifications_list)) {
    schema_classes <- .classifications_list[[schema_id]]

    for (class_name in names(schema_classes)) {
      values <- schema_classes[[class_name]]

      # Skip if no values selected
      if (length(values) == 0 || all(is.na(values)) || all(values == "")) {
        next
      }

      # Create one row per value
      for (value in values) {
        new_records[[length(new_records) + 1]] <- data.frame(
          DocID = .doc_id,
          UserID = .user_id,
          Timestamp = timestamp,
          Schema = as.integer(schema_id),
          Class = class_name,
          Value = value,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Combine all new records
  if (length(new_records) == 0) {
    new_df <- data.frame(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character()),
      Schema = integer(),
      Class = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
  } else {
    new_df <- dplyr::bind_rows(new_records)
  }

  # Load existing classifications
  if (file.exists(file_class)) {
    existing_df <- arrow::read_parquet(file_class)

    # Remove old classifications for this document
    existing_df <- existing_df[existing_df$DocID != .doc_id, ]

    # Combine with new
    combined_df <- dplyr::bind_rows(existing_df, new_df)
  } else {
    combined_df <- new_df
  }

  # Write back
  arrow::write_parquet(combined_df, file_class)

  invisible(NULL)
}

#' Initialize Classification File
#'
#' Creates empty ClassificationDetails.parquet if it doesn't exist.
#'
#' @param .dir Path to project directory
#' @return Invisible NULL
#' @keywords internal
initialize_classification_file <- function(.dir) {
  file_class <- file.path(.dir, "ClassificationDetails.parquet")

  if (!file.exists(file_class)) {
    empty_df <- data.frame(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character()),
      Schema = integer(),
      Class = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )

    arrow::write_parquet(empty_df, file_class)
  }

  invisible(NULL)
}


# NOTES FUNCTIONS - Add these to R/data_io.R at the end of the file

# ===== NOTES FUNCTIONS =====

#' Initialize Notes File
#'
#' Creates empty Notes.parquet if it doesn't exist.
#'
#' @param .dir Path to project directory
#' @return Invisible NULL
#' @keywords internal
initialize_notes_file <- function(.dir) {
  file_notes <- file.path(.dir, "Notes.parquet")

  if (!file.exists(file_notes)) {
    empty_df <- data.frame(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character()),
      NoteText = character(),
      stringsAsFactors = FALSE
    )

    arrow::write_parquet(empty_df, file_notes)
  }

  invisible(NULL)
}

#' Read Note for Document
#'
#' Loads the note for a specific document.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to load note for
#' @return Data frame with columns: DocID, UserID, Timestamp, NoteText
#'   Returns empty data frame if no note exists
#' @export
read_note <- function(.dir, .doc_id) {
  file_notes <- file.path(.dir, "Notes.parquet")

  if (!file.exists(file_notes)) {
    return(data.frame(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character()),
      NoteText = character(),
      stringsAsFactors = FALSE
    ))
  }

  arrow::open_dataset(file_notes) %>%
    dplyr::filter(DocID == .doc_id) %>%
    dplyr::collect()
}

#' Save Note for Document
#'
#' Saves or updates the note for a document. If note_text is empty or NULL,
#' removes the note for this document.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID
#' @param .user_id User making the note
#' @param .note_text Note text (character)
#' @return Invisible NULL
#' @export
save_note <- function(.dir, .doc_id, .user_id, .note_text) {
  file_notes <- file.path(.dir, "Notes.parquet")

  # Initialize file if it doesn't exist
  initialize_notes_file(.dir)

  # Load existing notes
  existing_df <- arrow::read_parquet(file_notes)

  # Remove old note for this document
  existing_df <- existing_df[existing_df$DocID != .doc_id, ]

  # If note text is not empty, add new note
  if (!is.null(.note_text) && nchar(trimws(.note_text)) > 0) {
    new_note <- data.frame(
      DocID = .doc_id,
      UserID = .user_id,
      Timestamp = Sys.time(),
      NoteText = .note_text,
      stringsAsFactors = FALSE
    )

    combined_df <- dplyr::bind_rows(existing_df, new_note)
  } else {
    # Empty note = remove entry
    combined_df <- existing_df
  }

  # Write back
  arrow::write_parquet(combined_df, file_notes)

  invisible(NULL)
}

#' Get Document IDs with Notes
#'
#' Returns list of document IDs that have notes.
#'
#' @param .dir Path to project directory
#' @return Character vector of document IDs
#' @export
get_docids_with_notes <- function(.dir) {
  file_notes <- file.path(.dir, "Notes.parquet")

  if (!file.exists(file_notes)) {
    return(character(0))
  }

  arrow::open_dataset(file_notes) %>%
    dplyr::select(DocID) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull(DocID)
}

#' Read All Notes
#'
#' Loads all notes from the database.
#'
#' @param .dir Path to project directory
#' @return Data frame with all note records
#' @export
read_all_notes <- function(.dir) {
  file_notes <- file.path(.dir, "Notes.parquet")

  if (!file.exists(file_notes)) {
    return(data.frame(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character()),
      NoteText = character(),
      stringsAsFactors = FALSE
    ))
  }

  arrow::read_parquet(file_notes)
}
