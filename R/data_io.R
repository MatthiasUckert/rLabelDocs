# DATA I/O FUNCTIONS - SQLite Implementation
# This file handles all data reading and writing operations using SQLite:
# - Schema files (Excel/CSV) - unchanged
# - Documents (Parquet) - unchanged
# - Classifications (SQLite append-only log)
# - Notes (SQLite append-only log)

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

# ===== SCHEMA FUNCTIONS =====

#' Read Schema File
#'
#' Reads and parses the classification schema from Excel or CSV format.
#'
#' @param .dir Path to project directory
#' @return Nested list structure organized by schema ID
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
#' Retrieves a single document by DocID using Arrow lazy loading.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to retrieve
#' @return Data frame with DocID and HTML columns
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
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Get latest timestamp for this document
  query <- "
    SELECT schema, class, value, user_id, timestamp
    FROM classification_log
    WHERE doc_id = ?
      AND timestamp = (
        SELECT MAX(timestamp)
        FROM classification_log
        WHERE doc_id = ?
      )
    ORDER BY schema, class, value
  "

  result <- DBI::dbGetQuery(con, query, params = list(.doc_id, .doc_id))

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
}

#' Read All Classifications
#'
#' Loads the latest classifications for all documents from SQLite.
#'
#' @param .dir Path to project directory
#' @return Data frame with all latest classification records
#' @export
read_all_classifications <- function(.dir) {
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Get latest classifications for all documents
  query <- "
    SELECT c.doc_id, c.user_id, c.timestamp, c.schema, c.class, c.value
    FROM classification_log c
    INNER JOIN (
      SELECT doc_id, MAX(timestamp) as max_timestamp
      FROM classification_log
      GROUP BY doc_id
    ) latest ON c.doc_id = latest.doc_id AND c.timestamp = latest.max_timestamp
    ORDER BY c.doc_id, c.schema, c.class
  "

  result <- DBI::dbGetQuery(con, query)

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
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

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

  # Get classified document IDs from SQLite
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  query <- "SELECT DISTINCT doc_id FROM classification_log"
  classified_ids <- DBI::dbGetQuery(con, query)$doc_id

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
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Get latest note for this document
  query <- "
    SELECT user_id, timestamp, note_text
    FROM note_log
    WHERE doc_id = ?
    ORDER BY timestamp DESC
    LIMIT 1
  "

  result <- DBI::dbGetQuery(con, query, params = list(.doc_id))

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
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Generate session ID
  session_id <- generate_session_id()
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Prepare note text (NULL if empty)
  if (is.null(.note_text) || nchar(trimws(.note_text)) == 0) {
    note_value <- NA_character_
  } else {
    note_value <- .note_text
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
}

#' Get Document IDs with Notes
#'
#' Returns list of document IDs that have notes (latest note is not NULL).
#'
#' @param .dir Path to project directory
#' @return Character vector of document IDs
#' @export
get_docids_with_notes <- function(.dir) {
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Get documents where latest note is not NULL
  query <- "
    SELECT DISTINCT n.doc_id
    FROM note_log n
    INNER JOIN (
      SELECT doc_id, MAX(timestamp) as max_timestamp
      FROM note_log
      GROUP BY doc_id
    ) latest ON n.doc_id = latest.doc_id AND n.timestamp = latest.max_timestamp
    WHERE n.note_text IS NOT NULL
  "

  result <- DBI::dbGetQuery(con, query)
  return(result$doc_id)
}

#' Read All Notes
#'
#' Loads the latest notes for all documents from SQLite.
#'
#' @param .dir Path to project directory
#' @return Data frame with all latest note records
#' @export
read_all_notes <- function(.dir) {
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Get latest notes for all documents
  query <- "
    SELECT n.doc_id, n.user_id, n.timestamp, n.note_text
    FROM note_log n
    INNER JOIN (
      SELECT doc_id, MAX(timestamp) as max_timestamp
      FROM note_log
      GROUP BY doc_id
    ) latest ON n.doc_id = latest.doc_id AND n.timestamp = latest.max_timestamp
    WHERE n.note_text IS NOT NULL
    ORDER BY n.doc_id
  "

  result <- DBI::dbGetQuery(con, query)

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
}

# ===== PROGRESS STATS (helper for overview) =====

#' Get Overall Progress Statistics
#'
#' Calculates project-wide classification progress using SQLite.
#'
#' @param .dir Path to project directory
#' @return List with progress statistics
#' @keywords internal
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

  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  query <- "SELECT COUNT(DISTINCT doc_id) as count FROM classification_log"
  classified_docs <- DBI::dbGetQuery(con, query)$count

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
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Get timestamps from both tables
  query <- "
    SELECT DISTINCT timestamp FROM classification_log
    UNION
    SELECT DISTINCT timestamp FROM note_log
    ORDER BY timestamp
  "

  result <- DBI::dbGetQuery(con, query)

  if (nrow(result) == 0) {
    return(as.POSIXct(character()))
  }

  as.POSIXct(result$timestamp, origin = "1970-01-01")
}

# ===== EXPORT FUNCTIONS (with timeline support) =====

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

  # Create exports directory if it doesn't exist
  exports_dir <- file.path(.dir, "exports")
  if (!dir.exists(exports_dir)) {
    dir.create(exports_dir, recursive = TRUE)
  }

  # Generate filename with timestamp
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename_base <- if (!is.null(.max_timestamp)) {
    paste0("current_state_as_of_", format(.max_timestamp, "%Y%m%d_%H%M%S"), "_", timestamp_str)
  } else {
    paste0("current_state_", timestamp_str)
  }

  # Get current classifications (with optional time filter)
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  if (!is.null(.max_timestamp)) {
    # Get state as of specified timestamp
    query <- "
      SELECT c.doc_id, c.user_id, c.timestamp, c.schema, c.class, c.value
      FROM classification_log c
      INNER JOIN (
        SELECT doc_id, MAX(timestamp) as max_timestamp
        FROM classification_log
        WHERE timestamp <= ?
        GROUP BY doc_id
      ) latest ON c.doc_id = latest.doc_id AND c.timestamp = latest.max_timestamp
      ORDER BY c.doc_id, c.schema, c.class
    "

    result <- DBI::dbGetQuery(con, query, params = list(format(.max_timestamp, "%Y-%m-%d %H:%M:%S")))

    if (nrow(result) > 0) {
      names(result) <- c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value")
      result$Timestamp <- as.POSIXct(result$Timestamp, origin = "1970-01-01")
      classifications <- result
    } else {
      classifications <- data.frame(
        DocID = character(),
        UserID = character(),
        Timestamp = as.POSIXct(character()),
        Schema = integer(),
        Class = character(),
        Value = character(),
        stringsAsFactors = FALSE
      )
    }
  } else {
    classifications <- read_all_classifications(.dir)
  }

  if (.include_notes && nrow(classifications) > 0) {
    # Get notes (with optional time filter)
    if (!is.null(.max_timestamp)) {
      query_notes <- "
        SELECT n.doc_id, n.user_id, n.timestamp, n.note_text
        FROM note_log n
        INNER JOIN (
          SELECT doc_id, MAX(timestamp) as max_timestamp
          FROM note_log
          WHERE timestamp <= ?
          GROUP BY doc_id
        ) latest ON n.doc_id = latest.doc_id AND n.timestamp = latest.max_timestamp
        WHERE n.note_text IS NOT NULL
        ORDER BY n.doc_id
      "

      result_notes <- DBI::dbGetQuery(con, query_notes, params = list(format(.max_timestamp, "%Y-%m-%d %H:%M:%S")))

      if (nrow(result_notes) > 0) {
        names(result_notes) <- c("DocID", "UserID", "Timestamp", "NoteText")
        result_notes$Timestamp <- as.POSIXct(result_notes$Timestamp, origin = "1970-01-01")
        notes <- result_notes
      } else {
        notes <- data.frame(
          DocID = character(),
          UserID = character(),
          Timestamp = as.POSIXct(character()),
          NoteText = character(),
          stringsAsFactors = FALSE
        )
      }
    } else {
      notes <- read_all_notes(.dir)
    }

    if (nrow(notes) > 0) {
      # Merge classifications with notes
      notes_as_rows <- data.frame(
        DocID = notes$DocID,
        UserID = notes$UserID,
        Timestamp = notes$Timestamp,
        Schema = 999L,
        Class = "DocumentNote",
        Value = notes$NoteText,
        stringsAsFactors = FALSE
      )

      export_data <- dplyr::bind_rows(classifications, notes_as_rows)
    } else {
      export_data <- classifications
    }
  } else {
    export_data <- classifications
  }

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

  # Create exports directory if it doesn't exist
  exports_dir <- file.path(.dir, "exports")
  if (!dir.exists(exports_dir)) {
    dir.create(exports_dir, recursive = TRUE)
  }

  # Generate filename with timestamp
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename_base <- if (!is.null(.max_timestamp)) {
    paste0("full_history_up_to_", format(.max_timestamp, "%Y%m%d_%H%M%S"), "_", timestamp_str)
  } else {
    paste0("full_history_", timestamp_str)
  }

  # Get all classification history from SQLite
  con <- get_db_connection(.dir)
  on.exit(DBI::dbDisconnect(con))

  # Get all classification records (with optional time filter)
  if (!is.null(.max_timestamp)) {
    query_class <- "
      SELECT doc_id, user_id, timestamp, schema, class, value, session_id
      FROM classification_log
      WHERE timestamp <= ?
      ORDER BY timestamp, doc_id, schema, class
    "
    classifications <- DBI::dbGetQuery(con, query_class, params = list(format(.max_timestamp, "%Y-%m-%d %H:%M:%S")))
  } else {
    query_class <- "
      SELECT doc_id, user_id, timestamp, schema, class, value, session_id
      FROM classification_log
      ORDER BY timestamp, doc_id, schema, class
    "
    classifications <- DBI::dbGetQuery(con, query_class)
  }

  if (nrow(classifications) > 0) {
    names(classifications) <- c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value", "SessionID")
    classifications$Timestamp <- as.POSIXct(classifications$Timestamp, origin = "1970-01-01")
  }

  if (.include_notes) {
    # Get all note history (with optional time filter)
    if (!is.null(.max_timestamp)) {
      query_notes <- "
        SELECT doc_id, user_id, timestamp, note_text, session_id
        FROM note_log
        WHERE timestamp <= ?
        ORDER BY timestamp, doc_id
      "
      notes <- DBI::dbGetQuery(con, query_notes, params = list(format(.max_timestamp, "%Y-%m-%d %H:%M:%S")))
    } else {
      query_notes <- "
        SELECT doc_id, user_id, timestamp, note_text, session_id
        FROM note_log
        ORDER BY timestamp, doc_id
      "
      notes <- DBI::dbGetQuery(con, query_notes)
    }

    if (nrow(notes) > 0) {
      names(notes) <- c("DocID", "UserID", "Timestamp", "NoteText", "SessionID")
      notes$Timestamp <- as.POSIXct(notes$Timestamp, origin = "1970-01-01")

      # Convert notes to same format as classifications
      notes_as_rows <- data.frame(
        DocID = notes$DocID,
        UserID = notes$UserID,
        Timestamp = notes$Timestamp,
        Schema = 999L,
        Class = "DocumentNote",
        Value = notes$NoteText,
        SessionID = notes$SessionID,
        stringsAsFactors = FALSE
      )

      export_data <- dplyr::bind_rows(classifications, notes_as_rows) %>%
        dplyr::arrange(Timestamp, DocID)
    } else {
      export_data <- classifications
    }
  } else {
    export_data <- classifications
  }

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

  if (.scope == "current") {
    export_current_state(.dir, .format, .include_notes = FALSE, .max_timestamp = .max_timestamp)
  } else {
    export_full_history(.dir, .format, .include_notes = FALSE, .max_timestamp = .max_timestamp)
  }
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
export_notes <- function(.dir, .format = c("csv", "parquet"),
                         .scope = c("current", "history"), .max_timestamp = NULL) {
  .format <- match.arg(.format)
  .scope <- match.arg(.scope)

  # Create exports directory
  exports_dir <- file.path(.dir, "exports")
  if (!dir.exists(exports_dir)) {
    dir.create(exports_dir, recursive = TRUE)
  }

  # Generate filename
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename_base <- if (!is.null(.max_timestamp)) {
    paste0("notes_", .scope, "_up_to_", format(.max_timestamp, "%Y%m%d_%H%M%S"), "_", timestamp_str)
  } else {
    paste0("notes_", .scope, "_", timestamp_str)
  }

  if (.scope == "current") {
    if (!is.null(.max_timestamp)) {
      # Get state as of timestamp
      con <- get_db_connection(.dir)
      on.exit(DBI::dbDisconnect(con))

      query <- "
        SELECT n.doc_id, n.user_id, n.timestamp, n.note_text
        FROM note_log n
        INNER JOIN (
          SELECT doc_id, MAX(timestamp) as max_timestamp
          FROM note_log
          WHERE timestamp <= ?
          GROUP BY doc_id
        ) latest ON n.doc_id = latest.doc_id AND n.timestamp = latest.max_timestamp
        WHERE n.note_text IS NOT NULL
        ORDER BY n.doc_id
      "

      result <- DBI::dbGetQuery(con, query, params = list(format(.max_timestamp, "%Y-%m-%d %H:%M:%S")))

      if (nrow(result) > 0) {
        names(result) <- c("DocID", "UserID", "Timestamp", "NoteText")
        result$Timestamp <- as.POSIXct(result$Timestamp, origin = "1970-01-01")
        notes <- result
      } else {
        notes <- data.frame(
          DocID = character(),
          UserID = character(),
          Timestamp = as.POSIXct(character()),
          NoteText = character(),
          stringsAsFactors = FALSE
        )
      }
    } else {
      notes <- read_all_notes(.dir)
    }
  } else {
    # Get full history
    con <- get_db_connection(.dir)
    on.exit(DBI::dbDisconnect(con))

    if (!is.null(.max_timestamp)) {
      query <- "
        SELECT doc_id, user_id, timestamp, note_text, session_id
        FROM note_log
        WHERE timestamp <= ?
        ORDER BY timestamp, doc_id
      "
      notes <- DBI::dbGetQuery(con, query, params = list(format(.max_timestamp, "%Y-%m-%d %H:%M:%S")))
    } else {
      query <- "
        SELECT doc_id, user_id, timestamp, note_text, session_id
        FROM note_log
        ORDER BY timestamp, doc_id
      "
      notes <- DBI::dbGetQuery(con, query)
    }

    if (nrow(notes) > 0) {
      names(notes) <- c("DocID", "UserID", "Timestamp", "NoteText", "SessionID")
      notes$Timestamp <- as.POSIXct(notes$Timestamp, origin = "1970-01-01")
    }
  }

  # Export
  if (.format == "csv") {
    filepath <- file.path(exports_dir, paste0(filename_base, ".csv"))
    readr::write_csv(notes, filepath)
  } else {
    filepath <- file.path(exports_dir, paste0(filename_base, ".parquet"))
    arrow::write_parquet(notes, filepath)
  }

  return(filepath)
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
