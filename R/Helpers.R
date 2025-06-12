# Add this at the top of your main R file to suppress NSE warnings
utils::globalVariables(c(
  "DocID", "HTML", "UserID", "Timestamp", "Count", "Status",
  "Percentage", "Category", "Date", "Cumulative", "Classifications",
  ".", "everything"
))

if (FALSE) {
  .dir <- "inst/extdata/TestClassification/"
  .doc_id <- "0000001750-8f452cd44a90f9e02d4ee91b0df6588c"
  .classifications <- list(
    DocClasses = "PurchaseSales-Assets",
    Amendment = "Amended"
  )
  .user_id <- "Matthias"
}


#' Check Input Directory Structure
#'
#' @param .dir Path to classification project directory
#' @return TRUE if valid, stops with error if invalid
check_input_directory <- function(.dir) {
  if (!dir.exists(.dir)) {
    msg_ <- paste0("Directory does not exist: ", .dir)
    stop(msg_, call. = FALSE)
  }

  # Check required files
  file_docs <- file.path(.dir, "Documents.parquet")
  file_schema <- file.path(.dir, "Schema.csv")
  file_classes <- file.path(.dir, "Classifications.parquet")

  if (!file.exists(file_docs)) {
    msg_ <- paste0("Documents.parquet not found in: ", .dir)
    stop(msg_, call. = FALSE)
  }

  if (!file.exists(file_schema)) {
    msg_ <- paste0("Schema.csv not found in: ", .dir)
    stop(msg_, call. = FALSE)
  }

  # Check Documents.parquet
  arr_docs <- arrow::open_dataset(file_docs)
  if (!all(c("DocID", "HTML") %in% names(arr_docs))) {
    msg_ <- "Documents.parquet must have DocID and HTML columns"
    stop(msg_, call. = FALSE)
  }

  doc_ids <- dplyr::pull(dplyr::collect(dplyr::select(arr_docs, DocID)))
  if (any(duplicated(doc_ids))) {
    msg_ <- "DocIDs must be unique"
    stop(msg_, call. = FALSE)
  }

  if (any(is.na(doc_ids) | doc_ids == "")) {
    msg_ <- "DocIDs cannot be empty or NA"
    stop(msg_, call. = FALSE)
  }

  # Check ClassificationSchema.csv
  schema <- readr::read_csv(file_schema, show_col_types = FALSE)
  if (!all(c("Class", "Value") %in% names(schema))) {
    msg_ <- "ClassificationSchema.csv must have Class and Value columns"
    stop(msg_, call. = FALSE)
  }

  if (any(is.na(schema$Class) | schema$Class == "")) {
    msg_ <- "Class names cannot be empty or NA"
    stop(msg_, call. = FALSE)
  }

  if (any(is.na(schema$Value) | schema$Value == "")) {
    msg_ <- "Class values cannot be empty or NA"
    stop(msg_, call. = FALSE)
  }

  if (!file.exists(file_classes)) {
    classes <- unique(schema$Class)
    empty_classifications <- tibble::tibble(
      DocID = character(),
      UserID = character(),
      Timestamp = as.POSIXct(character())
    )
    for (class_name in classes) {
      empty_classifications[[class_name]] <- character()
    }

    arrow::write_parquet(empty_classifications, file_classes)
  } else {
    classifications <- arrow::read_parquet(file_classes, mmap = FALSE)
    classes <- unique(schema$Class)
    required_cols <- c("DocID", "UserID", "Timestamp", classes)

    if (!all(required_cols %in% names(classifications))) {
      missing_cols <- setdiff(required_cols, names(classifications))
      msg_ <- paste0("Classifications.parquet missing columns: ", paste(missing_cols, collapse = ", "))
      stop(msg_, call. = FALSE)
    }
  }

  # Replace non-ASCII characters with ASCII equivalents
  cat("* Directory structure valid\n")
  cat("* Found", length(doc_ids), "documents\n")
  cat("* Found", length(unique(schema$Class)), "classification classes\n")

  return(TRUE)
}

#' Read Single Document by ID
#'
#' @param .dir Path to classification project directory
#' @param .doc_id Document ID to retrieve
#'
#' @return Data frame with single document (DocID, HTML columns) or
#'   empty data frame if document not found
read_single_document <- function(.dir, .doc_id) {
  file.path(.dir, "Documents.parquet") %>%
    arrow::open_dataset() %>%
    dplyr::filter(DocID == .doc_id) %>%
    dplyr::collect()
}

#' Read Classification Schema
#'
#' @param .dir Path to classification project directory
#' @return List with schema (Class -> Values)
read_schema <- function(.dir) {
  file_schema <- file.path(.dir, "Schema.csv")
  schema_df <- readr::read_csv(file_schema, show_col_types = FALSE)
  split(schema_df$Value, schema_df$Class)
}

#' Get Document Count (without loading all data)
#' @param .dir Path to classification project directory
#' @return Number of documents
get_document_count <- function(.dir) {
  file.path(.dir, "Documents.parquet") %>%
    arrow::open_dataset() %>%
    nrow()
}

#' Get All Document IDs (for navigation)
#' @param .dir Path to classification project directory
#' @param .type Type of documents to return
#' @return Vector of document IDs
get_docids <- function(.dir, .type = c("All", "Classified", "Unclassified")) {
  type <- match.arg(.type, c("All", "Classified", "Unclassified"))
  classified <- read_classification(.dir)[["DocID"]]
  all_ids <- file.path(.dir, "Documents.parquet") %>%
    arrow::open_dataset() %>%
    dplyr::select(DocID) %>%
    dplyr::collect() %>%
    dplyr::pull()

  if (type == "All") {
    all_ids
  } else if (type == "Classified") {
    all_ids[all_ids %in% classified]
  } else {
    all_ids[!all_ids %in% classified]
  }
}

#' Read Classifications
#' @param .dir Path to classification project directory
#' @param .doc_id Optional document ID to filter to specific document
#' @return Data frame with classifications
read_classification <- function(.dir, .doc_id = NULL) {
  if (is.null(.doc_id)) {
    file.path(.dir, "Classifications.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::collect()
  } else {
    file.path(.dir, "Classifications.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::filter(DocID == .doc_id) %>%
      dplyr::collect()
  }
}

#' Save Classification
#' @param .dir Path to classification project directory
#' @param .doc_id Document ID
#' @param .user_id User ID
#' @param .classifications Named list of classifications
save_classification <- function(.dir, .doc_id, .user_id, .classifications) {
  new_record <- tibble::tibble(
    DocID = .doc_id,
    UserID = .user_id,
    Timestamp = Sys.time(),
  ) %>% dplyr::bind_cols(tibble::as_tibble(.classifications))

  dplyr::bind_rows(
    new_record,
    read_classification(.dir)
  ) %>%
    dplyr::distinct(DocID, .keep_all = TRUE) %>%
    arrow::write_parquet(file.path(.dir, "Classifications.parquet"))
}

#' Get Classification Statistics
#' @param .dir Path to classification project directory
#' @return Data frame with classification statistics
get_classification_stats <- function(.dir) {
  count_ <- tibble::tibble(
    DocID = get_docids(.dir, "All")
  ) %>%
    dplyr::left_join(
      y = read_classification(.dir),
      by = dplyr::join_by(DocID)
    ) %>%
    dplyr::mutate(dplyr::across(
      .cols = -c(DocID, UserID, Timestamp),
      .fns = ~ dplyr::if_else(is.na(.), "Unclassified", .)
    )) %>%
    dplyr::select(-c(DocID, UserID, Timestamp)) %>%
    dplyr::count(dplyr::across(dplyr::everything()))

  return(count_)
}

#' Check if Document is Fully Classified
#' @param .dir Path to classification directory
#' @param .doc_id Document ID to check
#' @return TRUE if document has all required classifications

is_document_classified <- function(.dir, .doc_id) {
  classified_ids <- get_docids(.dir, "Classified")
  .doc_id %in% classified_ids
}

#' Get Classification Count by Filter Type
#' @param .dir Path to classification directory
#' @return Named list with counts for each filter type

get_filter_counts <- function(.dir) {
  all_ids <- get_docids(.dir, "All")
  unclassified_ids <- get_docids(.dir, "Unclassified")
  classified_ids <- get_docids(.dir, "Classified")

  list(
    all = length(all_ids),
    unclassified = length(unclassified_ids),
    classified = length(classified_ids)
  )
}

#' Get Classification Progress Statistics
#' @param .dir Path to classification project directory
#' @return List with progress statistics
get_progress_stats <- function(.dir) {
  total_docs <- get_document_count(.dir)

  file_classes <- file.path(.dir, "Classifications.parquet")
  if (!file.exists(file_classes)) {
    return(list(
      total_documents = total_docs,
      classified_documents = 0,
      percentage_complete = 0
    ))
  }

  classified_docs <- arrow::read_parquet(file_classes, mmap = FALSE) %>%
    dplyr::pull(DocID) %>%
    unique() %>%
    length()

  list(
    total_documents = total_docs,
    classified_documents = classified_docs,
    percentage_complete = round(classified_docs / total_docs * 100, 1)
  )
}

#' Format Document Info Display
#' @param .dir Path to classification project directory
#' @param .doc_id Current document ID (can be NULL)
#' @param .current_index Current position in filtered list
#' @param .total_filtered Total number of filtered documents
#' @param .filter_type Current filter type
#' @param .schema Classification schema
#' @return Formatted string for document info display
format_document_info <- function(.dir, .doc_id, .current_index, .total_filtered, .filter_type, .schema) {
  if (is.null(.doc_id)) {
    return(paste("No documents available\nFilter:", .filter_type))
  }

  # Get progress stats
  progress <- get_progress_stats(.dir)

  # Get current classification
  classification_df <- read_classification(.dir, .doc_id)
  if (nrow(classification_df) > 0) {
    schema_cols <- names(.schema)
    existing_cols <- intersect(schema_cols, names(classification_df))
    if (length(existing_cols) > 0) {
      classification <- as.list(classification_df[1, existing_cols, drop = FALSE])
      classification_text <- paste(
        purrr::imap_chr(classification, ~ paste0(" - ", .y, ": ", ifelse(is.na(.x) || .x == "", "Not set", .x))),
        collapse = "\n"
      )
    } else {
      classification_text <- "No classification yet"
    }
  } else {
    classification_text <- "No classification yet"
  }

  # Format the complete info string
  paste0(
    "Document: ", .doc_id, "\n",
    "Position: ", .current_index, " of ", .total_filtered, "\n",
    "Progress: ", paste0(
      progress$classified_documents, "/", progress$total_documents,
      " (", progress$percentage_complete, "%)"
    ), "\n\n",
    "Current Classification:\n", classification_text
  )
}

#' Get Unique Classification Values for Each Class
#' @param .dir Path to classification project directory
#' @return Named list where each element is a vector of unique values for that class
get_classification_values <- function(.dir) {
  classifications_file <- file.path(.dir, "Classifications.parquet")
  if (!file.exists(classifications_file)) {
    return(list())
  }

  classifications_df <- arrow::read_parquet(classifications_file, mmap = FALSE)
  if (nrow(classifications_df) == 0) {
    return(list())
  }

  # Get schema to know which columns are classification classes
  schema <- read_schema(.dir)
  schema_cols <- names(schema)

  # Get unique values for each classification class
  result <- list()
  for (class_name in schema_cols) {
    if (class_name %in% names(classifications_df)) {
      unique_vals <- unique(classifications_df[[class_name]])
      unique_vals <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
      if (length(unique_vals) > 0) {
        result[[class_name]] <- sort(unique_vals)
      }
    }
  }

  return(result)
}

#' Filter Documents by Classification Criteria
#' @param .dir Path to classification project directory
#' @param .class_filters Named list of classification filters (class_name -> selected_value)
#' @return Vector of document IDs that match the criteria
filter_documents_by_classification <- function(.dir, .class_filters) {
  # Start with all classified documents
  all_classified <- get_docids(.dir, "Classified")

  if (length(.class_filters) == 0 || length(all_classified) == 0) {
    return(all_classified)
  }

  classifications_df <- read_classification(.dir)
  if (nrow(classifications_df) == 0) {
    return(character(0))
  }

  # Start with all classified documents
  filtered_df <- classifications_df

  # Apply each filter
  for (class_name in names(.class_filters)) {
    filter_value <- .class_filters[[class_name]]
    if (!is.null(filter_value) && filter_value != "" && filter_value != "All") {
      if (class_name %in% names(filtered_df)) {
        # Filter to documents that have this specific value
        filtered_df <- filtered_df[!is.na(filtered_df[[class_name]]) &
          filtered_df[[class_name]] == filter_value, ]
      } else {
        # If the classification column doesn't exist, return empty
        return(character(0))
      }
    }
  }

  # Return document IDs that match all criteria
  result <- unique(filtered_df$DocID)
  return(result[result %in% all_classified]) # Ensure they're actually classified
}

# ========================
# PLOTTING FUNCTIONS
# ========================

#' Create Progress Overview Plot
#' @param .dir Path to classification project directory
#' @return ggplot object showing classification progress
plot_progress_overview <- function(.dir) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }

  progress <- get_progress_stats(.dir)

  # Create data for donut chart
  plot_data <- data.frame(
    Status = c("Classified", "Remaining"),
    Count = c(progress$classified_documents, progress$total_documents - progress$classified_documents),
    Percentage = c(progress$percentage_complete, 100 - progress$percentage_complete),
    stringsAsFactors = FALSE
  )

  # Create donut chart
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = 2, y = Count, fill = Status)) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::xlim(0.5, 2.5) +
    ggplot2::scale_fill_manual(values = c("Classified" = "#2ecc71", "Remaining" = "#ecf0f1")) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    ggplot2::labs(
      title = paste0("Classification Progress: ", progress$percentage_complete, "%"),
      fill = ""
    ) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white", fontface = "bold"
    )

  return(p)
}

#' Create Classification Distribution Plot
#' @param .dir Path to classification project directory
#' @param .class_name Classification class to plot
#' @return ggplot object showing distribution of values for a classification class
plot_classification_distribution <- function(.dir, .class_name) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }

  classifications_df <- read_classification(.dir)

  if (nrow(classifications_df) == 0 || !.class_name %in% names(classifications_df)) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = "No data available"), size = 5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = paste(.class_name, "Distribution")))
  }

  # Count occurrences of each value
  class_data <- classifications_df[[.class_name]]
  class_data <- class_data[!is.na(class_data) & class_data != ""]

  if (length(class_data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = "No data available"), size = 5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = paste(.class_name, "Distribution")))
  }

  # Create count data frame
  class_counts <- as.data.frame(table(class_data), stringsAsFactors = FALSE)
  names(class_counts) <- c("Category", "Count")
  class_counts <- class_counts[order(class_counts$Count, decreasing = TRUE), ]

  # Create bar chart
  p <- ggplot2::ggplot(class_counts, ggplot2::aes(x = stats::reorder(Category, Count), y = Count)) +
    ggplot2::geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::labs(
      title = paste(.class_name, "Distribution"),
      x = .class_name,
      y = "Number of Documents"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = Count), hjust = -0.1, size = 3)

  return(p)
}

#' Create Classification Timeline Plot
#' @param .dir Path to classification project directory
#' @return ggplot object showing classification activity over time
plot_classification_timeline <- function(.dir) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }

  classifications_df <- read_classification(.dir)

  if (nrow(classifications_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = "No classifications yet"), size = 5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Classification Timeline"))
  }

  # Group by date and count classifications
  timeline_data <- classifications_df %>%
    dplyr::mutate(Date = as.Date(Timestamp)) %>%
    dplyr::count(Date, name = "Classifications") %>%
    dplyr::arrange(Date)

  if (nrow(timeline_data) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = "No timeline data"), size = 5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Classification Timeline"))
  }

  # Add cumulative count
  timeline_data$Cumulative <- cumsum(timeline_data$Classifications)

  # Create timeline plot
  p <- ggplot2::ggplot(timeline_data, ggplot2::aes(x = Date, y = Cumulative)) +
    ggplot2::geom_line(color = "#e74c3c", linewidth = 1.2) +
    ggplot2::geom_point(color = "#e74c3c", size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "Cumulative Classifications Over Time",
      x = "Date",
      y = "Total Classifications"
    ) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d")

  return(p)
}

#' Create User Activity Plot
#' @param .dir Path to classification project directory
#' @return ggplot object showing activity by user
plot_user_activity <- function(.dir) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }

  classifications_df <- read_classification(.dir)

  if (nrow(classifications_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = "No user data"), size = 5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "User Activity"))
  }

  # Count classifications by user
  user_counts <- classifications_df %>%
    dplyr::count(UserID, name = "Classifications") %>%
    dplyr::arrange(dplyr::desc(Classifications))

  if (nrow(user_counts) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = "No user data"), size = 5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "User Activity"))
  }

  # Create bar chart
  p <- ggplot2::ggplot(user_counts, ggplot2::aes(x = stats::reorder(UserID, Classifications), y = Classifications)) +
    ggplot2::geom_bar(stat = "identity", fill = "#9b59b6", alpha = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold")
    ) +
    ggplot2::labs(
      title = "Classifications by User",
      x = "User",
      y = "Number of Classifications"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = Classifications), hjust = -0.1, size = 3)

  return(p)
}
