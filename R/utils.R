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
#' @keywords internal
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

#' Suppress NSE warnings
#' @keywords internal
utils::globalVariables(c(
  # Data I/O variables
  "DocID", "HTML", "UserID", "Timestamp", "Schema", "Class", "Value",
  "SchemaName", ".", "everything", "where",
  "NoteText",
  # Overview module variables
  "category", "count", "classifications", "ymax", "ymin", "Date", "documents",
  "fraction"
))
