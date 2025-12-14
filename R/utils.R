#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Null coalescing operator
#'
#' @name null-coalesce
#' @rdname null-coalesce
#' @keywords internal
#' @param x First value
#' @param y Fallback value if x is NULL
#' @return x if not NULL, otherwise y
NULL

#' @rdname null-coalesce
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Generate schema-class key
#' @param schema_id Schema ID
#' @param class_name Class name
#' @return Character key in format "schema_class"
#' @keywords internal
make_schema_class_key <- function(schema_id, class_name) {
  paste0(schema_id, "_", class_name)
}

#' Parse schema-class key
#' @param key Character key in format "schema_class"
#' @return List with schema_id and class_name
#' @keywords internal
parse_schema_class_key <- function(key) {
  parts <- strsplit(key, "_", fixed = TRUE)[[1]]
  list(
    schema_id = as.integer(parts[1]),
    class_name = paste(parts[-1], collapse = "_")
  )
}

#' Generate Session ID
#'
#' Creates a unique session ID for grouping related database operations.
#' Format: YYYYMMDD_HHMMSS_randomstring
#'
#' @return Character session ID
#' @export
generate_session_id <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  random_part <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  paste0(timestamp, "_", random_part)
}

#' Execute Function with Database Connection
#'
#' Wrapper that handles database connection lifecycle.
#' Automatically opens connection, executes function, and closes connection.
#'
#' @param .dir Path to project directory
#' @param .func Function to execute with connection (receives con as first arg)
#' @return Result of .func
#' @keywords internal
with_db_connection <- function(.dir, .func) {
  db_file <- file.path(.dir, "classification_data.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .func(con)
}

#' Format Timestamp
#'
#' Consistent timestamp formatting across the application.
#'
#' @param timestamp POSIXct timestamp
#' @param include_seconds Include seconds in output (default TRUE)
#' @return Formatted string
#' @export
format_timestamp <- function(timestamp, include_seconds = TRUE) {
  if (include_seconds) {
    format(timestamp, "%Y-%m-%d %H:%M:%S")
  } else {
    format(timestamp, "%Y-%m-%d %H:%M")
  }
}

#' Mark Documents
#'
#' Adds document IDs to the marked documents list.
#'
#' @param marked_docs ReactiveValues object with 'ids' component
#' @param doc_ids Character vector of document IDs to mark
#' @return Invisible NULL
#' @export
mark_documents <- function(marked_docs, doc_ids) {
  if (!is.null(marked_docs)) {
    marked_docs$ids <- unique(c(marked_docs$ids, doc_ids))
  }
  invisible(NULL)
}

#' Unmark Documents
#'
#' Removes document IDs from the marked documents list.
#'
#' @param marked_docs ReactiveValues object with 'ids' component
#' @param doc_ids Character vector of document IDs to unmark
#' @return Invisible NULL
#' @export
unmark_documents <- function(marked_docs, doc_ids) {
  if (!is.null(marked_docs)) {
    marked_docs$ids <- setdiff(marked_docs$ids, doc_ids)
  }
  invisible(NULL)
}

#' Clear All Marks
#'
#' Clears all marked documents.
#'
#' @param marked_docs ReactiveValues object with 'ids' component
#' @return Integer count of cleared marks
#' @export
clear_all_marks <- function(marked_docs) {
  if (!is.null(marked_docs)) {
    count <- length(marked_docs$ids)
    marked_docs$ids <- character(0)
    return(count)
  }
  return(0L)
}

#' Suppress NSE warnings
#' @keywords internal
utils::globalVariables(c(
  # Data I/O variables - database columns
  "DocID", "HTML", "UserID", "Timestamp", "Schema", "Class", "Value",
  "SchemaName", ".", "everything", "where",
  "NoteText",
  # Database column names (lowercase from SQLite)
  "doc_id", "user_id", "timestamp", "schema", "class", "value",
  "note_text", "session_id", "n",
  # Overview module variables
  "category", "count", "classifications", "ymax", "ymin", "Date", "documents",
  "fraction"
))
