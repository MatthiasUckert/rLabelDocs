# DATA I/O FUNCTIONS - SQLite Implementation (REFACTORED)
# This file handles all data reading and writing operations using SQLite
# with dplyr/pipe syntax instead of raw SQL queries

#' Initialize SQLite Database
#'
#' Creates SQLite database with classification_log and note_log tables.
#' Includes proper indexes for performance.
#'
#' @param .dir Path to project directory
#' @return Invisible NULL
#' @keywords internal
initialize_database <- function(.dir) {
  db_file <- file.path(.dir, "classification_data.db")

  # Connect to database (creates file if doesn't exist)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)

  # Create classification_log table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS classification_log (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      doc_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      timestamp DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
      schema INTEGER NOT NULL,
      class TEXT NOT NULL,
      value TEXT NOT NULL,
      session_id TEXT NOT NULL
    )
  ")

  # Create note_log table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS note_log (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      doc_id TEXT NOT NULL,
      user_id TEXT NOT NULL,
      timestamp DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
      note_text TEXT,
      session_id TEXT NOT NULL
    )
  ")

  # Create indexes for performance
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_class_log_doc_time
    ON classification_log(doc_id, timestamp DESC)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_note_log_doc_time
    ON note_log(doc_id, timestamp DESC)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_class_session
    ON classification_log(session_id)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_note_session
    ON note_log(session_id)
  ")

  DBI::dbDisconnect(con)
  invisible(NULL)
}

#' Get Database Connection
#'
#' Helper function to get SQLite connection.
#'
#' @param .dir Path to project directory
#' @return DBI connection object
#' @keywords internal
get_db_connection <- function(.dir) {
  db_file <- file.path(.dir, "classification_data.db")
  DBI::dbConnect(RSQLite::SQLite(), db_file)
}

#' Get Documents Database Connection
#'
#' Helper function to get SQLite connection for documents.
#'
#' @param .dir Path to project directory
#' @return DBI connection object
#' @keywords internal
get_documents_db_connection <- function(.dir) {
  db_file <- file.path(.dir, "Documents.db")
  if (!file.exists(db_file)) {
    stop("Documents.db not found. Run convert_documents_to_sqlite() first.", call. = FALSE)
  }
  DBI::dbConnect(RSQLite::SQLite(), db_file)
}

#' Execute Function with Documents Database Connection
#'
#' Wrapper that handles documents database connection lifecycle.
#'
#' @param .dir Path to project directory
#' @param .func Function to execute with connection
#' @return Result of .func
#' @keywords internal
with_documents_db_connection <- function(.dir, .func) {
  con <- get_documents_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .func(con)
}


# ===== SCHEMA FUNCTIONS =====

#' Read Schema File
#'
#' Reads and parses the classification schema from CSV format.
#'
#' @param .dir Path to project directory
#' @return Nested list structure organized by schema ID
#' @export
read_schema <- function(.dir) {
  # Read CSV schema file
  file_csv <- file.path(.dir, "Schema.csv")

  if (!file.exists(file_csv)) {
    stop("Schema file not found. Expected Schema.csv in: ", .dir, call. = FALSE)
  }

  schema_df <- readr::read_csv(file_csv, show_col_types = FALSE)

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
    schema_name <- schema_subset$SchemaName[1]

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

# ===== DOCUMENT FUNCTIONS =====

#' Read Single Document
#'
#' Retrieves a single document by DocID from SQLite.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to retrieve
#' @return Data frame with DocID and HTML columns
#' @export
read_single_document <- function(.dir, .doc_id) {
  with_documents_db_connection(.dir, function(con) {
    result <- con %>%
      dplyr::tbl("documents") %>%
      dplyr::filter(doc_id == .doc_id) %>%
      dplyr::collect()

    if (nrow(result) == 0) {
      return(data.frame(
        DocID = character(),
        HTML = character(),
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      DocID = result$doc_id,
      HTML = result$html,
      stringsAsFactors = FALSE
    )
  })
}


#' Get Document Count
#'
#' Returns total number of documents.
#'
#' @param .dir Path to project directory
#' @return Integer count of documents
#' @export
get_document_count <- function(.dir) {
  db_file <- file.path(.dir, "Documents.db")

  if (!file.exists(db_file)) {
    return(0L)
  }

  with_documents_db_connection(.dir, function(con) {
    con %>%
      dplyr::tbl("documents") %>%
      dplyr::count() %>%
      dplyr::collect() %>%
      dplyr::pull(n)
  })
}

# ===== CLASSIFICATION FUNCTIONS =====

#' Read Classifications for Document
#'
#' Loads the latest classification values for a specific document from SQLite.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to load classifications for
#' @return Data frame with columns: DocID, UserID, Timestamp, Schema, Class, Value
#' @export
read_classification <- function(.dir, .doc_id) {
  with_db_connection(.dir, function(con) {
    # Get latest classifications for this document using dplyr
    result <- con %>%
      dplyr::tbl("classification_log") %>%
      dplyr::filter(doc_id == .doc_id) %>%
      dplyr::group_by(doc_id) %>%
      dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(schema, class, value, user_id, timestamp) %>%
      dplyr::arrange(schema, class, value) %>%
      dplyr::collect()

    if (nrow(result) == 0) {
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

    # Add DocID column and standardize column names
    result$DocID <- .doc_id
    result <- result[, c("DocID", "user_id", "timestamp", "schema", "class", "value")]
    names(result) <- c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value")

    # Convert timestamp to POSIXct
    result$Timestamp <- as.POSIXct(result$Timestamp, origin = "1970-01-01")

    return(result)
  })
}

#' Read All Classifications
#'
#' Loads the latest classifications for all documents from SQLite.
#'
#' @param .dir Path to project directory
#' @return Data frame with all latest classification records
#' @export
read_all_classifications <- function(.dir) {
  with_db_connection(.dir, function(con) {
    # Get latest classifications for all documents using dplyr
    result <- con %>%
      dplyr::tbl("classification_log") %>%
      dplyr::group_by(doc_id) %>%
      dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(doc_id, user_id, timestamp, schema, class, value) %>%
      dplyr::arrange(doc_id, schema, class) %>%
      dplyr::collect()

    if (nrow(result) == 0) {
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

    # Standardize column names
    names(result) <- c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value")

    # Convert timestamp to POSIXct
    result$Timestamp <- as.POSIXct(result$Timestamp, origin = "1970-01-01")

    return(result)
  })
}

#' Save Classification
#'
#' Saves classification values for a document in SQLite append-only log.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID being classified
#' @param .user_id User making the classification
#' @param .classifications_list Nested list structure
#' @return Invisible NULL
#' @export
save_classification <- function(.dir, .doc_id, .user_id, .classifications_list) {
  with_db_connection(.dir, function(con) {
    # Generate session ID for this save operation
    session_id <- generate_session_id()
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Build data frame with all classification values
    records <- list()

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
          records[[length(records) + 1]] <- data.frame(
            doc_id = .doc_id,
            user_id = .user_id,
            timestamp = timestamp,
            schema = as.integer(schema_id),
            class = class_name,
            value = value,
            session_id = session_id,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # Insert records into database
    if (length(records) > 0) {
      records_df <- dplyr::bind_rows(records)
      DBI::dbAppendTable(con, "classification_log", records_df)
    }

    invisible(NULL)
  })
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

  # Get all document IDs from Documents.db
  all_ids <- with_documents_db_connection(.dir, function(con) {
    con %>%
      dplyr::tbl("documents") %>%
      dplyr::select(doc_id) %>%
      dplyr::collect() %>%
      dplyr::pull(doc_id)
  })

  if (.type == "All") {
    return(all_ids)
  }

  # Get classified document IDs from classification_data.db
  classified_ids <- with_db_connection(.dir, function(con) {
    con %>%
      dplyr::tbl("classification_log") %>%
      dplyr::distinct(doc_id) %>%
      dplyr::collect() %>%
      dplyr::pull(doc_id)
  })

  if (.type == "Classified") {
    return(all_ids[all_ids %in% classified_ids])
  } else {
    return(all_ids[!all_ids %in% classified_ids])
  }
}

# ===== NOTES FUNCTIONS (SQLite-based) =====

#' Read Note for Document
#'
#' Loads the latest note for a specific document from SQLite.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to load note for
#' @return Data frame with columns: DocID, UserID, Timestamp, NoteText
#' @export
read_note <- function(.dir, .doc_id) {
  with_db_connection(.dir, function(con) {
    # Get latest note for this document using dplyr
    result <- con %>%
      dplyr::tbl("note_log") %>%
      dplyr::filter(doc_id == .doc_id) %>%
      dplyr::collect() %>%
      dplyr::arrange(dplyr::desc(timestamp)) %>%
      dplyr::slice(1) %>%
      dplyr::select(user_id, timestamp, note_text)

    if (nrow(result) == 0 || is.na(result$note_text[1])) {
      return(data.frame(
        DocID = character(),
        UserID = character(),
        Timestamp = as.POSIXct(character()),
        NoteText = character(),
        stringsAsFactors = FALSE
      ))
    }

    # Add DocID and standardize column names
    result$DocID <- .doc_id
    result <- result[, c("DocID", "user_id", "timestamp", "note_text")]
    names(result) <- c("DocID", "UserID", "Timestamp", "NoteText")

    # Convert timestamp to POSIXct
    result$Timestamp <- as.POSIXct(result$Timestamp, origin = "1970-01-01")

    return(result)
  })
}

#' Save Note for Document
#'
#' Saves or updates the note for a document in SQLite append-only log.
#' If note_text is empty or NULL, saves NULL to indicate deletion.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID
#' @param .user_id User making the note
#' @param .note_text Note text (character)
#' @return Invisible NULL
#' @export
save_note <- function(.dir, .doc_id, .user_id, .note_text) {
  with_db_connection(.dir, function(con) {
    # Generate session ID
    session_id <- generate_session_id()
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Prepare note text (NULL if empty)
    note_value <- if (is.null(.note_text) || nchar(trimws(.note_text)) == 0) {
      NA_character_
    } else {
      .note_text
    }

    # Insert note record
    note_record <- data.frame(
      doc_id = .doc_id,
      user_id = .user_id,
      timestamp = timestamp,
      note_text = note_value,
      session_id = session_id,
      stringsAsFactors = FALSE
    )

    DBI::dbAppendTable(con, "note_log", note_record)

    invisible(NULL)
  })
}

#' Get Document IDs with Notes
#'
#' Returns list of document IDs that have notes (latest note is not NULL).
#'
#' @param .dir Path to project directory
#' @return Character vector of document IDs
#' @export
get_docids_with_notes <- function(.dir) {
  with_db_connection(.dir, function(con) {
    # Get documents where latest note is not NULL using dplyr
    result <- con %>%
      dplyr::tbl("note_log") %>%
      dplyr::group_by(doc_id) %>%
      dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(note_text)) %>%
      dplyr::distinct(doc_id) %>%
      dplyr::collect() %>%
      dplyr::pull(doc_id)

    return(result)
  })
}

#' Read All Notes
#'
#' Loads the latest notes for all documents from SQLite.
#'
#' @param .dir Path to project directory
#' @return Data frame with all latest note records
#' @export
read_all_notes <- function(.dir) {
  with_db_connection(.dir, function(con) {
    # Get latest notes for all documents using dplyr
    result <- con %>%
      dplyr::tbl("note_log") %>%
      dplyr::group_by(doc_id) %>%
      dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(note_text)) %>%
      dplyr::select(doc_id, user_id, timestamp, note_text) %>%
      dplyr::arrange(doc_id) %>%
      dplyr::collect()

    if (nrow(result) == 0) {
      return(data.frame(
        DocID = character(),
        UserID = character(),
        Timestamp = as.POSIXct(character()),
        NoteText = character(),
        stringsAsFactors = FALSE
      ))
    }

    # Standardize column names
    names(result) <- c("DocID", "UserID", "Timestamp", "NoteText")

    # Convert timestamp to POSIXct
    result$Timestamp <- as.POSIXct(result$Timestamp, origin = "1970-01-01")

    return(result)
  })
}

# ===== PROGRESS STATS =====

#' Get Overall Progress Statistics
#'
#' Calculates project-wide classification progress using SQLite.
#'
#' @param .dir Path to project directory
#' @return List with progress statistics
#' @export
get_progress_stats <- function(.dir) {
  total_docs <- get_document_count(.dir)

  if (total_docs == 0) {
    return(list(
      total_documents = 0,
      classified_documents = 0,
      unclassified_documents = 0,
      percentage_complete = 0
    ))
  }

  classified_docs <- with_db_connection(.dir, function(con) {
    # Count distinct classified documents using dplyr
    con %>%
      dplyr::tbl("classification_log") %>%
      dplyr::distinct(doc_id) %>%
      dplyr::count() %>%
      dplyr::collect() %>%
      dplyr::pull(n)
  })

  list(
    total_documents = total_docs,
    classified_documents = classified_docs,
    unclassified_documents = total_docs - classified_docs,
    percentage_complete = round(classified_docs / total_docs * 100, 1)
  )
}

# ===== TIMELINE FUNCTIONS =====

#' Get All Timestamps
#'
#' Retrieves all unique timestamps from classification and note logs.
#'
#' @param .dir Path to project directory
#' @return POSIXct vector of unique timestamps, sorted
#' @export
get_all_timestamps <- function(.dir) {
  with_db_connection(.dir, function(con) {
    # Get timestamps from classification_log
    class_ts <- con %>%
      dplyr::tbl("classification_log") %>%
      dplyr::distinct(timestamp) %>%
      dplyr::collect() %>%
      dplyr::mutate(timestamp = as.character(timestamp))  # Force character type

    # Get timestamps from note_log
    note_ts <- con %>%
      dplyr::tbl("note_log") %>%
      dplyr::distinct(timestamp) %>%
      dplyr::collect() %>%
      dplyr::mutate(timestamp = as.character(timestamp))  # Force character type

    # Combine and sort
    all_ts <- dplyr::bind_rows(class_ts, note_ts) %>%
      dplyr::distinct(timestamp) %>%
      dplyr::arrange(timestamp) %>%
      dplyr::pull(timestamp)

    if (length(all_ts) == 0) {
      return(as.POSIXct(character()))
    }

    as.POSIXct(all_ts, origin = "1970-01-01")
  })
}

# ===== HELPER FUNCTIONS FOR EXPORTS =====

#' Get Current State Classifications
#'
#' Internal helper to get latest classifications, optionally up to a timestamp.
#'
#' @param con Database connection
#' @param .max_timestamp Optional maximum timestamp (POSIXct)
#' @return Data frame of classifications
#' @keywords internal
get_current_classifications <- function(con, .max_timestamp = NULL) {
  query <- con %>%
    dplyr::tbl("classification_log")

  # Apply timestamp filter if provided
  if (!is.null(.max_timestamp)) {
    max_ts_str <- format(.max_timestamp, "%Y-%m-%d %H:%M:%S")
    query <- query %>%
      dplyr::filter(timestamp <= max_ts_str)
  }

  # Get latest for each document
  result <- query %>%
    dplyr::group_by(doc_id) %>%
    dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(doc_id, user_id, timestamp, schema, class, value) %>%
    dplyr::arrange(doc_id, schema, class) %>%
    dplyr::collect()

  return(result)
}

#' Get History Classifications
#'
#' Internal helper to get all classification history, optionally up to a timestamp.
#'
#' @param con Database connection
#' @param .max_timestamp Optional maximum timestamp (POSIXct)
#' @return Data frame of all classifications
#' @keywords internal
get_history_classifications <- function(con, .max_timestamp = NULL) {
  query <- con %>%
    dplyr::tbl("classification_log")

  # Apply timestamp filter if provided
  if (!is.null(.max_timestamp)) {
    max_ts_str <- format(.max_timestamp, "%Y-%m-%d %H:%M:%S")
    query <- query %>%
      dplyr::filter(timestamp <= max_ts_str)
  }

  result <- query %>%
    dplyr::select(doc_id, user_id, timestamp, schema, class, value, session_id) %>%
    dplyr::arrange(timestamp, doc_id, schema, class) %>%
    dplyr::collect()

  return(result)
}

#' Get Current State Notes
#'
#' Internal helper to get latest notes, optionally up to a timestamp.
#'
#' @param con Database connection
#' @param .max_timestamp Optional maximum timestamp (POSIXct)
#' @return Data frame of notes
#' @keywords internal
get_current_notes <- function(con, .max_timestamp = NULL) {
  query <- con %>%
    dplyr::tbl("note_log")

  # Apply timestamp filter if provided
  if (!is.null(.max_timestamp)) {
    max_ts_str <- format(.max_timestamp, "%Y-%m-%d %H:%M:%S")
    query <- query %>%
      dplyr::filter(timestamp <= max_ts_str)
  }

  # Get latest for each document
  result <- query %>%
    dplyr::group_by(doc_id) %>%
    dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(note_text)) %>%
    dplyr::select(doc_id, user_id, timestamp, note_text) %>%
    dplyr::arrange(doc_id) %>%
    dplyr::collect()

  return(result)
}

#' Get History Notes
#'
#' Internal helper to get all note history, optionally up to a timestamp.
#'
#' @param con Database connection
#' @param .max_timestamp Optional maximum timestamp (POSIXct)
#' @return Data frame of all notes
#' @keywords internal
get_history_notes <- function(con, .max_timestamp = NULL) {
  query <- con %>%
    dplyr::tbl("note_log")

  # Apply timestamp filter if provided
  if (!is.null(.max_timestamp)) {
    max_ts_str <- format(.max_timestamp, "%Y-%m-%d %H:%M:%S")
    query <- query %>%
      dplyr::filter(timestamp <= max_ts_str)
  }

  result <- query %>%
    dplyr::select(doc_id, user_id, timestamp, note_text, session_id) %>%
    dplyr::arrange(timestamp, doc_id) %>%
    dplyr::collect()

  return(result)
}

# ===== EXPORT FUNCTIONS =====

#' Export Data Helper (Internal)
#'
#' Internal helper function to consolidate export logic.
#'
#' @param .dir Path to project directory
#' @param .format Export format: "csv" or "parquet"
#' @param .scope Export scope: "current" or "history"
#' @param .content_type Content type: "both", "classifications", "notes"
#' @param .max_timestamp Optional maximum timestamp to filter to (POSIXct)
#' @return Path to exported file
#' @keywords internal
export_data_helper <- function(.dir, .format, .scope, .content_type, .max_timestamp = NULL) {
  # Create exports directory if it doesn't exist
  exports_dir <- file.path(.dir, "exports")
  if (!dir.exists(exports_dir)) {
    dir.create(exports_dir, recursive = TRUE)
  }

  # Generate filename with timestamp
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")

  filename_base <- if (!is.null(.max_timestamp)) {
    time_suffix <- format(.max_timestamp, "%Y%m%d_%H%M%S")
    if (.scope == "current") {
      paste0(.content_type, "_current_as_of_", time_suffix, "_", timestamp_str)
    } else {
      paste0(.content_type, "_history_up_to_", time_suffix, "_", timestamp_str)
    }
  } else {
    paste0(.content_type, "_", .scope, "_", timestamp_str)
  }

  # Use with_db_connection for all database operations
  export_data <- with_db_connection(.dir, function(con) {
    # Fetch data based on scope and content type
    if (.content_type == "classifications" || .content_type == "both") {
      if (.scope == "current") {
        classifications <- get_current_classifications(con, .max_timestamp)
      } else {
        classifications <- get_history_classifications(con, .max_timestamp)
      }

      # Standardize column names
      if (nrow(classifications) > 0) {
        if (.scope == "current") {
          names(classifications) <- c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value")
        } else {
          names(classifications) <- c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value", "SessionID")
        }
        classifications$Timestamp <- as.POSIXct(classifications$Timestamp, origin = "1970-01-01")
      }
    }

    if (.content_type == "notes" || .content_type == "both") {
      if (.scope == "current") {
        notes <- get_current_notes(con, .max_timestamp)
      } else {
        notes <- get_history_notes(con, .max_timestamp)
      }

      # Standardize column names
      if (nrow(notes) > 0) {
        if (.scope == "current") {
          names(notes) <- c("DocID", "UserID", "Timestamp", "NoteText")
        } else {
          names(notes) <- c("DocID", "UserID", "Timestamp", "NoteText", "SessionID")
        }
        notes$Timestamp <- as.POSIXct(notes$Timestamp, origin = "1970-01-01")
      }
    }

    # Combine data based on content type
    if (.content_type == "both") {
      # Combine classifications and notes
      if (nrow(notes) > 0) {
        notes_as_rows <- data.frame(
          DocID = notes$DocID,
          UserID = notes$UserID,
          Timestamp = notes$Timestamp,
          Schema = 999L,
          Class = "DocumentNote",
          Value = notes$NoteText,
          stringsAsFactors = FALSE
        )

        if (.scope == "history") {
          notes_as_rows$SessionID <- notes$SessionID
        }

        dplyr::bind_rows(classifications, notes_as_rows) %>%
          dplyr::arrange(Timestamp, DocID)
      } else {
        classifications
      }
    } else if (.content_type == "classifications") {
      classifications
    } else {
      notes
    }
  })

  # Export based on format
  if (.format == "csv") {
    filepath <- file.path(exports_dir, paste0(filename_base, ".csv"))
    readr::write_csv(export_data, filepath)
  } else {
    filepath <- file.path(exports_dir, paste0(filename_base, ".parquet"))
    arrow::write_parquet(export_data, filepath)
  }

  return(filepath)
}

#' Export Current State
#'
#' Exports the latest classifications and notes to file.
#' Can optionally filter to a specific point in time.
#'
#' @param .dir Path to project directory
#' @param .format Export format: "csv" or "parquet"
#' @param .include_notes Include notes in export (default: TRUE)
#' @param .max_timestamp Optional maximum timestamp to filter to (POSIXct)
#' @return Path to exported file
#' @export
export_current_state <- function(.dir, .format = c("csv", "parquet"),
                                 .include_notes = TRUE, .max_timestamp = NULL) {
  .format <- match.arg(.format)
  content_type <- if (.include_notes) "both" else "classifications"
  export_data_helper(.dir, .format, "current", content_type, .max_timestamp)
}

#' Export Full History
#'
#' Exports complete history of all classifications and notes with all versions.
#' Can optionally filter to a specific point in time.
#'
#' @param .dir Path to project directory
#' @param .format Export format: "csv" or "parquet"
#' @param .include_notes Include notes in export (default: TRUE)
#' @param .max_timestamp Optional maximum timestamp to filter to (POSIXct)
#' @return Path to exported file
#' @export
export_full_history <- function(.dir, .format = c("csv", "parquet"),
                                .include_notes = TRUE, .max_timestamp = NULL) {
  .format <- match.arg(.format)
  content_type <- if (.include_notes) "both" else "classifications"
  export_data_helper(.dir, .format, "history", content_type, .max_timestamp)
}

#' Export Classifications Only
#'
#' Exports only classifications (no notes) in specified scope.
#'
#' @param .dir Path to project directory
#' @param .format Export format: "csv" or "parquet"
#' @param .scope Export scope: "current" or "history"
#' @param .max_timestamp Optional maximum timestamp to filter to (POSIXct)
#' @return Path to exported file
#' @export
export_classifications <- function(.dir, .format = c("csv", "parquet"),
                                   .scope = c("current", "history"), .max_timestamp = NULL) {
  .format <- match.arg(.format)
  .scope <- match.arg(.scope)
  export_data_helper(.dir, .format, .scope, "classifications", .max_timestamp)
}

#' Export Notes Only
#'
#' Exports only notes in specified scope.
#'
#' @param .dir Path to project directory
#' @param .format Export format: "csv" or "parquet"
#' @param .scope Export scope: "current" or "history"
#' @param .max_timestamp Optional maximum timestamp to filter to (POSIXct)
#' @return Path to exported file
#' @export
export_notes <- function(.dir, .format = c("csv", "parquet"), .scope = c("current", "history"), .max_timestamp = NULL) {
  .format <- match.arg(.format)
  .scope <- match.arg(.scope)
  export_data_helper(.dir, .format, .scope, "notes", .max_timestamp)
}

#' List Exports
#'
#' Lists all export files in the exports directory.
#'
#' @param .dir Path to project directory
#' @return Data frame with export file information
#' @export
list_exports <- function(.dir) {
  exports_dir <- file.path(.dir, "exports")

  if (!dir.exists(exports_dir)) {
    return(data.frame(
      filename = character(),
      size = numeric(),
      modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }

  files <- list.files(exports_dir, full.names = TRUE, pattern = "\\.(csv|parquet)$")

  if (length(files) == 0) {
    return(data.frame(
      filename = character(),
      size = numeric(),
      modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }

  file_info <- file.info(files)

  data.frame(
    filename = basename(files),
    path = files,
    size = file_info$size,
    size_mb = round(file_info$size / 1024 / 1024, 2),
    modified = file_info$mtime,
    stringsAsFactors = FALSE
  )
}

#' Delete Export
#'
#' Deletes an export file.
#'
#' @param .dir Path to project directory
#' @param .filename Export filename to delete
#' @return Logical TRUE if successful
#' @export
delete_export <- function(.dir, .filename) {
  filepath <- file.path(.dir, "exports", .filename)

  if (!file.exists(filepath)) {
    warning("Export file not found: ", .filename)
    return(FALSE)
  }

  file.remove(filepath)
  return(TRUE)
}


#' Convert Documents Parquet to SQLite
#'
#' Converts a Documents.parquet file to a SQLite database with proper
#' indexing for fast lookups. Run this once before using the classification app.
#'
#' @param parquet_path Path to Documents.parquet file
#' @return Invisible path to created SQLite database
#'
#' @details
#' The function:
#' - Reads all documents from the Parquet file
#' - Creates a SQLite database in the same folder
#' - Creates a 'documents' table with doc_id (indexed) and html columns
#' - Optimizes the database for fast lookups
#'
#' Expected Parquet structure:
#' - Column 'DocID': Unique document identifier (text)
#' - Column 'HTML': Document content (text)
#'
#' @examples
#' \dontrun{
#' # Convert documents
#' convert_documents_to_sqlite("path/to/Documents.parquet")
#'
#' # The SQLite database will be created at:
#' # path/to/classification_data.db
#' }
#'
#' @export
convert_documents_to_sqlite <- function(parquet_path) {


  if (!file.exists(parquet_path)) {
    stop("File not found: ", parquet_path, call. = FALSE)
  }

  if (!grepl("\\.parquet$", parquet_path, ignore.case = TRUE)) {
    stop("Expected a .parquet file, got: ", basename(parquet_path), call. = FALSE)
  }


  parquet_dir <- dirname(parquet_path)
  db_path <- file.path(parquet_dir, "Documents.db")

  message("Converting Documents to SQLite")
  message("=" |> rep(60) |> paste(collapse = ""))
  message("Input:  ", parquet_path)
  message("Output: ", db_path)
  message("")


  message("Reading Parquet file...")
  docs <- arrow::read_parquet(parquet_path)

  # Validate columns
  if (!"DocID" %in% names(docs)) {
    stop("Parquet file must have a 'DocID' column", call. = FALSE)
  }
  if (!"HTML" %in% names(docs)) {
    stop("Parquet file must have an 'HTML' column", call. = FALSE)
  }

  n_docs <- nrow(docs)
  html_size_mb <- sum(nchar(docs$HTML), na.rm = TRUE) / 1024 / 1024

  message("  Documents: ", format(n_docs, big.mark = ","))
  message("  HTML size: ", round(html_size_mb, 1), " MB")
  message("")


  message("Creating SQLite database...")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Drop existing documents table if exists
  if (DBI::dbExistsTable(con, "documents")) {
    message("  Dropping existing documents table...")
    DBI::dbRemoveTable(con, "documents")
  }


  DBI::dbExecute(con, "
    CREATE TABLE documents (
      doc_id TEXT PRIMARY KEY,
      html TEXT NOT NULL
    )
  ")


  message("Inserting documents...")

  docs_df <- data.frame(
    doc_id = docs$DocID,
    html = docs$HTML,
    stringsAsFactors = FALSE
  )

  # Insert in chunks for large datasets (progress feedback)
  chunk_size <- 1000
  n_chunks <- ceiling(n_docs / chunk_size)

  for (i in seq_len(n_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n_docs)

    chunk <- docs_df[start_idx:end_idx, ]
    DBI::dbAppendTable(con, "documents", chunk)

    if (n_chunks > 1 && i %% 5 == 0) {
      message("  Progress: ", end_idx, "/", n_docs, " documents")
    }
  }


  message("Creating index and optimizing...")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_documents_doc_id ON documents(doc_id)")
  DBI::dbExecute(con, "ANALYZE")


  db_size_mb <- file.size(db_path) / 1024 / 1024
  parquet_size_mb <- file.size(parquet_path) / 1024 / 1024

  message("")
  message("=" |> rep(60) |> paste(collapse = ""))
  message("Conversion complete!")
  message("=" |> rep(60) |> paste(collapse = ""))
  message("")
  message("  Documents converted: ", format(n_docs, big.mark = ","))
  message("  Parquet file size:   ", round(parquet_size_mb, 1), " MB")
  message("  SQLite file size:    ", round(db_size_mb, 1), " MB")
  message("")
  message("  Database location: ", db_path)
  message("")
  message("You can now delete '", basename(parquet_path), "' if desired.")
  message("")

  invisible(db_path)
}

