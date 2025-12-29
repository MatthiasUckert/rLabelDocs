# VALIDATION FUNCTIONS
# This file contains all validation logic for:
# - Directory structure
# - Schema files
# - Document files
# - SQLite database
# - Input parameters

#' Validate Project Directory
#'
#' Comprehensive validation of project directory structure and files.
#' Checks for required files, validates schema, and ensures data integrity.
#'
#' @param .dir Path to project directory
#' @return TRUE if valid, stops with error message if invalid
#' @export
validate_project_directory <- function(.dir) {
  # Check directory exists
  if (!dir.exists(.dir)) {
    stop("Directory does not exist: ", .dir, call. = FALSE)
  }

  # Check for schema file (CSV only)
  file_csv <- file.path(.dir, "Schema.csv")

  if (!file.exists(file_csv)) {
    stop(
      "Schema file not found. Expected Schema.csv in: ", .dir,
      call. = FALSE
    )
  }

  # Check for Documents.parquet
  # file_docs <- file.path(.dir, "Documents.parquet")
  # if (!file.exists(file_docs)) {
  #   stop("Documents.parquet not found in: ", .dir, call. = FALSE)
  # }

  # Validate Documents.parquet structure
  validate_documents_file(.dir)

  # Validate schema file
  validate_schema_file(.dir)

  # Initialize SQLite database if needed
  initialize_database(.dir)

  # Success message
  doc_count <- get_document_count(.dir)
  schema <- read_schema(.dir)

  message("\u2713 Directory structure valid")
  message("\u2713 Found ", doc_count, " documents")
  message("\u2713 Found ", length(schema), " classification schema(s)")
  message("\u2713 SQLite database ready")

  return(TRUE)
}

#' Validate Documents Database
#'
#' Checks Documents.db for required structure and data integrity.
#'
#' @param .dir Path to project directory
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_documents_file <- function(.dir) {
  db_path <- file.path(.dir, "Documents.db")

  if (!file.exists(db_path)) {
    stop("Documents.db not found in: ", .dir,
         "\nRun convert_documents_to_sqlite() to create it from Documents.parquet",
         call. = FALSE)
  }

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con))

    # Check table exists
    if (!DBI::dbExistsTable(con, "documents")) {
      stop("Documents.db does not contain 'documents' table", call. = FALSE)
    }

    # Check columns
    cols <- DBI::dbListFields(con, "documents")
    required_cols <- c("doc_id", "html")

    if (!all(required_cols %in% cols)) {
      missing <- setdiff(required_cols, cols)
      stop("documents table missing required columns: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }

    # Check for duplicate doc_ids
    dup_count <- con %>%
      dplyr::tbl("documents") %>%
      dplyr::group_by(doc_id) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup() %>%
      dplyr::count() %>%
      dplyr::collect() %>%
      dplyr::pull(n)

    if (dup_count > 0) {
      stop("Duplicate doc_ids found in documents table", call. = FALSE)
    }

    return(TRUE)

  }, error = function(e) {
    stop("Error validating Documents.db: ", e$message, call. = FALSE)
  })
}

#' Validate Schema File
#'
#' Checks schema file for required structure and valid values.
#'
#' @param .dir Path to project directory
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_schema_file <- function(.dir) {
  # Try to read schema
  tryCatch({
    schema_list <- read_schema(.dir)

    # Check that at least one schema exists
    if (length(schema_list) == 0) {
      stop("Schema file contains no schemas", call. = FALSE)
    }

    # Validate each schema
    for (schema_id in names(schema_list)) {
      schema <- schema_list[[schema_id]]

      # Check schema has classes
      if (length(schema$classes) == 0) {
        stop(
          "Schema ", schema_id, " has no classes defined",
          call. = FALSE
        )
      }

      # Check each class has values
      for (class_name in names(schema$classes)) {
        values <- schema$classes[[class_name]]

        if (length(values) == 0) {
          stop(
            "Schema ", schema_id, ", Class '", class_name, "' has no values",
            call. = FALSE
          )
        }

        # Check for empty values
        if (any(is.na(values) | values == "")) {
          stop(
            "Schema ", schema_id, ", Class '", class_name,
            "' contains empty or NA values",
            call. = FALSE
          )
        }
      }
    }

    return(TRUE)

  }, error = function(e) {
    stop("Error validating schema: ", e$message, call. = FALSE)
  })
}

#' Validate User ID
#'
#' Checks that user ID is valid (non-empty string).
#'
#' @param .user_id User ID to validate
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_user_id <- function(.user_id) {
  if (is.null(.user_id) || length(.user_id) == 0) {
    stop("User ID cannot be NULL or empty", call. = FALSE)
  }

  if (!is.character(.user_id) || nchar(.user_id) == 0) {
    stop("User ID must be a non-empty string", call. = FALSE)
  }

  return(TRUE)
}

#' Validate Document ID
#'
#' Checks that document ID exists in the project.
#'
#' @param .dir Path to project directory
#' @param .doc_id Document ID to validate
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_doc_id <- function(.dir, .doc_id) {
  if (is.null(.doc_id) || length(.doc_id) == 0) {
    stop("Document ID cannot be NULL or empty", call. = FALSE)
  }

  if (!is.character(.doc_id) || nchar(.doc_id) == 0) {
    stop("Document ID must be a non-empty string", call. = FALSE)
  }

  # Check if document exists
  all_ids <- get_docids(.dir, "All")
  if (!.doc_id %in% all_ids) {
    stop("Document ID '", .doc_id, "' not found in project", call. = FALSE)
  }

  return(TRUE)
}

#' Check Schema Changes
#'
#' Compares current schema hash with stored hash to detect changes.
#' Creates hash file on first run.
#'
#' @param .dir Path to project directory
#' @return List with components:
#'   - changed: TRUE if schema changed
#'   - message: Description of status
#' @export
check_schema_changes <- function(.dir) {
  hash_file <- file.path(.dir, ".schema_hash")

  # Check for CSV schema file
  file_csv <- file.path(.dir, "Schema.csv")

  if (!file.exists(file_csv)) {
    return(list(
      changed = FALSE,
      message = "No schema file found"
    ))
  }

  # Calculate current hash
  if (!requireNamespace("digest", quietly = TRUE)) {
    # If digest not available, skip hash checking
    return(list(
      changed = FALSE,
      message = "digest package not available for hash checking"
    ))
  }

  current_hash <- digest::digest(file = file_csv, algo = "md5")

  # First run - create hash file
  if (!file.exists(hash_file)) {
    writeLines(
      c(current_hash, as.character(Sys.time())),
      hash_file
    )
    return(list(
      changed = FALSE,
      message = "Schema hash initialized"
    ))
  }

  # Read stored hash
  stored_lines <- readLines(hash_file, warn = FALSE)
  if (length(stored_lines) < 1) {
    # Invalid hash file, recreate
    writeLines(
      c(current_hash, as.character(Sys.time())),
      hash_file
    )
    return(list(
      changed = FALSE,
      message = "Schema hash reset"
    ))
  }

  stored_hash <- stored_lines[1]

  # Compare hashes
  if (current_hash != stored_hash) {
    return(list(
      changed = TRUE,
      message = paste(
        "Schema has been modified since last use.",
        "This may cause inconsistencies in existing classifications.",
        "Please review your schema carefully before continuing."
      )
    ))
  }

  return(list(
    changed = FALSE,
    message = "Schema unchanged"
  ))
}

#' Update Schema Hash
#'
#' Updates stored schema hash after user confirms schema change.
#'
#' @param .dir Path to project directory
#' @return Invisible NULL
#' @export
update_schema_hash <- function(.dir) {
  hash_file <- file.path(.dir, ".schema_hash")

  # Check for CSV schema file
  file_csv <- file.path(.dir, "Schema.csv")

  if (!file.exists(file_csv)) {
    return(invisible(NULL))
  }

  if (!requireNamespace("digest", quietly = TRUE)) {
    return(invisible(NULL))
  }

  current_hash <- digest::digest(file = file_csv, algo = "md5")
  writeLines(
    c(current_hash, as.character(Sys.time())),
    hash_file
  )

  invisible(NULL)
}

#' Check Project Setup
#'
#' Checks if project has required files (Documents.db and Schema.csv).
#'
#' @param .dir Path to project directory
#' @return List with:
#'   - has_documents: logical
#'   - has_schema: logical
#'   - is_ready: logical (both files exist)
#' @export
check_project_setup <- function(.dir) {
  has_schema <- file.exists(file.path(.dir, "Schema.csv"))

  # Check for Documents.db with documents table
  db_path <- file.path(.dir, "Documents.db")
  has_documents <- FALSE

  if (file.exists(db_path)) {
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      has_documents <- DBI::dbExistsTable(con, "documents")
      DBI::dbDisconnect(con)
    }, error = function(e) {
      has_documents <- FALSE
    })
  }

  list(
    has_documents = has_documents,
    has_schema = has_schema,
    is_ready = has_documents && has_schema
  )
}
