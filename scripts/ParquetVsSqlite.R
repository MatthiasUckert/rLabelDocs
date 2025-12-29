# ============================================================================
# BENCHMARK: Parquet vs SQLite Document Lookup Performance
# ============================================================================
#
# This script compares the performance of looking up documents by DocID
# using two approaches:
#   1. Parquet with Arrow (current implementation)
#   2. SQLite with indexed doc_id column
#
# Usage:
#   source("benchmark_document_lookup.R")
#   results <- run_benchmark("path/to/your/Documents.parquet")
#
# Or run interactively by setting the path below and sourcing the file.
# ============================================================================

library(arrow)
library(DBI)
library(RSQLite)
library(dplyr)

# ============================================================================
# SETUP FUNCTIONS
# ============================================================================

#' Create SQLite Database from Parquet
#'
#' Migrates documents from Parquet to SQLite with proper indexing.
#'
#' @param parquet_path Path to Documents.parquet
#' @param sqlite_path Path for output SQLite file (default: same dir, .db extension
#' @return Path to created SQLite file
create_sqlite_from_parquet <- function(parquet_path, sqlite_path = NULL) {

  if (is.null(sqlite_path)) {
    sqlite_path <- sub("\\.parquet$", "_benchmark.db", parquet_path)
  }

  message("Reading Parquet file...")
  docs <- arrow::read_parquet(parquet_path)
  message("  Found ", nrow(docs), " documents")
  message("  Total size: ", round(sum(nchar(docs$HTML)) / 1024 / 1024, 1), " MB of HTML")

  # Remove existing benchmark DB if exists
  if (file.exists(sqlite_path)) {
    file.remove(sqlite_path)
  }

  message("\nCreating SQLite database...")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)

  # Create table
  DBI::dbExecute(con, "
    CREATE TABLE documents (
      doc_id TEXT PRIMARY KEY,
      html TEXT NOT NULL
    )
  ")

  # Insert documents
  message("  Inserting documents...")
  docs_df <- data.frame(
    doc_id = docs$DocID,
    html = docs$HTML,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "documents", docs_df, append = TRUE)

  # Create explicit index (PRIMARY KEY already creates one, but let's be sure)
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_doc_id ON documents(doc_id)")

  # Analyze for query optimization
  DBI::dbExecute(con, "ANALYZE")

  DBI::dbDisconnect(con)

  message("  SQLite file created: ", sqlite_path)
  message("  SQLite file size: ", round(file.size(sqlite_path) / 1024 / 1024, 1), " MB")

  return(sqlite_path)
}

# ============================================================================
# LOOKUP FUNCTIONS
# ============================================================================

#' Lookup document using Parquet (current approach)
#'
#' @param parquet_path Path to Parquet file
#' @param doc_id Document ID to lookup
#' @return Data frame with DocID and HTML
lookup_parquet <- function(parquet_path, doc_id) {
  arrow::open_dataset(parquet_path) %>%
    dplyr::filter(DocID == doc_id) %>%
    dplyr::collect()
}

#' Lookup document using SQLite with index
#'
#' @param sqlite_path Path to SQLite file
#' @param doc_id Document ID to lookup
#' @return Data frame with DocID and HTML
lookup_sqlite <- function(sqlite_path, doc_id) {
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con))

  result <- con %>%
    dplyr::tbl("documents") %>%
    dplyr::filter(doc_id == !!doc_id) %>%
    dplyr::collect()

  if (nrow(result) == 0) {
    return(data.frame(DocID = character(), HTML = character()))
  }

  data.frame(
    DocID = result$doc_id,
    HTML = result$html,
    stringsAsFactors = FALSE
  )
}

#' Lookup document using SQLite with persistent connection
#'
#' @param con Active DBI connection
#' @param doc_id Document ID to lookup
#' @return Data frame with DocID and HTML
lookup_sqlite_persistent <- function(con, doc_id) {
  result <- con %>%
    dplyr::tbl("documents") %>%
    dplyr::filter(doc_id == !!doc_id) %>%
    dplyr::collect()

  if (nrow(result) == 0) {
    return(data.frame(DocID = character(), HTML = character()))
  }

  data.frame(
    DocID = result$doc_id,
    HTML = result$html,
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# BENCHMARK FUNCTION
# ============================================================================

#' Run Benchmark Comparison
#'
#' Compares Parquet vs SQLite lookup performance.
#'
#' @param parquet_path Path to Documents.parquet
#' @param n_lookups Number of random document lookups to perform (default: 50)
#' @param n_warmup Number of warmup lookups before timing (default: 5
#' @return Data frame with timing results
run_benchmark <- function(parquet_path, n_lookups = 50, n_warmup = 5) {

  if (!file.exists(parquet_path)) {
    stop("Parquet file not found: ", parquet_path)
  }

  message("=" |> rep(70) |> paste(collapse = ""))
  message("BENCHMARK: Parquet vs SQLite Document Lookup")
  message("=" |> rep(70) |> paste(collapse = ""))

  # Setup
  sqlite_path <- create_sqlite_from_parquet(parquet_path)

  # Get all document IDs for random sampling
  message("\nGetting document IDs...")
  all_docs <- arrow::read_parquet(parquet_path, col_select = "DocID")
  all_doc_ids <- all_docs$DocID
  message("  Total documents: ", length(all_doc_ids))

  # Sample random doc IDs for testing
  set.seed(42)  # Reproducibility
  test_doc_ids <- sample(all_doc_ids, min(n_lookups, length(all_doc_ids)))
  warmup_doc_ids <- sample(all_doc_ids, min(n_warmup, length(all_doc_ids)))

  # -------------------------------------------------------------------------
  # Warmup runs (not timed)
  # -------------------------------------------------------------------------
  message("\nWarmup runs (", n_warmup, " lookups each)...")

  for (doc_id in warmup_doc_ids) {
    lookup_parquet(parquet_path, doc_id)
  }

  con_warmup <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  for (doc_id in warmup_doc_ids) {
    lookup_sqlite_persistent(con_warmup, doc_id)
  }
  DBI::dbDisconnect(con_warmup)

  # -------------------------------------------------------------------------
  # Benchmark: Parquet
  # -------------------------------------------------------------------------
  message("\nBenchmarking Parquet (", n_lookups, " lookups)...")

  parquet_times <- numeric(length(test_doc_ids))

  for (i in seq_along(test_doc_ids)) {
    start_time <- Sys.time()
    result <- lookup_parquet(parquet_path, test_doc_ids[i])
    end_time <- Sys.time()
    parquet_times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))

    if (i %% 10 == 0) {
      message("  Completed ", i, "/", length(test_doc_ids))
    }
  }

  # -------------------------------------------------------------------------
  # Benchmark: SQLite (new connection each time - realistic scenario)
  # -------------------------------------------------------------------------
  message("\nBenchmarking SQLite - new connection each lookup (", n_lookups, " lookups)...")

  sqlite_times_new_conn <- numeric(length(test_doc_ids))

  for (i in seq_along(test_doc_ids)) {
    start_time <- Sys.time()
    result <- lookup_sqlite(sqlite_path, test_doc_ids[i])
    end_time <- Sys.time()
    sqlite_times_new_conn[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))

    if (i %% 10 == 0) {
      message("  Completed ", i, "/", length(test_doc_ids))
    }
  }

  # -------------------------------------------------------------------------
  # Benchmark: SQLite (persistent connection - best case scenario)
  # -------------------------------------------------------------------------
  message("\nBenchmarking SQLite - persistent connection (", n_lookups, " lookups)...")

  sqlite_times_persistent <- numeric(length(test_doc_ids))
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)

  for (i in seq_along(test_doc_ids)) {
    start_time <- Sys.time()
    result <- lookup_sqlite_persistent(con, test_doc_ids[i])
    end_time <- Sys.time()
    sqlite_times_persistent[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))

    if (i %% 10 == 0) {
      message("  Completed ", i, "/", length(test_doc_ids))
    }
  }

  DBI::dbDisconnect(con)

  # -------------------------------------------------------------------------
  # Results
  # -------------------------------------------------------------------------
  message("\n")
  message("=" |> rep(70) |> paste(collapse = ""))
  message("RESULTS")
  message("=" |> rep(70) |> paste(collapse = ""))

  results <- data.frame(
    Method = c("Parquet", "SQLite (new conn)", "SQLite (persistent)"),
    Mean_ms = c(
      mean(parquet_times) * 1000,
      mean(sqlite_times_new_conn) * 1000,
      mean(sqlite_times_persistent) * 1000
    ),
    Median_ms = c(
      median(parquet_times) * 1000,
      median(sqlite_times_new_conn) * 1000,
      median(sqlite_times_persistent) * 1000
    ),
    Min_ms = c(
      min(parquet_times) * 1000,
      min(sqlite_times_new_conn) * 1000,
      min(sqlite_times_persistent) * 1000
    ),
    Max_ms = c(
      max(parquet_times) * 1000,
      max(sqlite_times_new_conn) * 1000,
      max(sqlite_times_persistent) * 1000
    ),
    SD_ms = c(
      sd(parquet_times) * 1000,
      sd(sqlite_times_new_conn) * 1000,
      sd(sqlite_times_persistent) * 1000
    )
  )

  # Round for display
  results_display <- results
  results_display[, -1] <- round(results_display[, -1], 2)

  message("\nTiming Statistics (milliseconds):")
  message("-" |> rep(70) |> paste(collapse = ""))
  print(results_display, row.names = FALSE)

  # Speedup calculations
  speedup_new_conn <- mean(parquet_times) / mean(sqlite_times_new_conn)
  speedup_persistent <- mean(parquet_times) / mean(sqlite_times_persistent)

  message("\n")
  message("-" |> rep(70) |> paste(collapse = ""))
  message("SPEEDUP SUMMARY")
  message("-" |> rep(70) |> paste(collapse = ""))
  message("SQLite (new conn) is     ", round(speedup_new_conn, 1), "x faster than Parquet")
  message("SQLite (persistent) is   ", round(speedup_persistent, 1), "x faster than Parquet")

  message("\n")
  message("-" |> rep(70) |> paste(collapse = ""))
  message("PRACTICAL IMPACT")
  message("-" |> rep(70) |> paste(collapse = ""))
  message("Average document load time:")
  message("  Parquet:              ", round(mean(parquet_times) * 1000, 0), " ms")
  message("  SQLite (new conn):    ", round(mean(sqlite_times_new_conn) * 1000, 0), " ms")
  message("  SQLite (persistent):  ", round(mean(sqlite_times_persistent) * 1000, 0), " ms")

  message("\n")
  message("-" |> rep(70) |> paste(collapse = ""))
  message("FILE SIZES")
  message("-" |> rep(70) |> paste(collapse = ""))
  message("  Parquet: ", round(file.size(parquet_path) / 1024 / 1024, 1), " MB")
  message("  SQLite:  ", round(file.size(sqlite_path) / 1024 / 1024, 1), " MB")

  # Return detailed results
  invisible(list(
    summary = results,
    parquet_times = parquet_times,
    sqlite_times_new_conn = sqlite_times_new_conn,
    sqlite_times_persistent = sqlite_times_persistent,
    speedup_new_conn = speedup_new_conn,
    speedup_persistent = speedup_persistent,
    sqlite_path = sqlite_path,
    n_documents = length(all_doc_ids),
    n_lookups = n_lookups
  ))
}

# ============================================================================
# QUICK TEST FUNCTION
# ============================================================================

#' Quick Test with Fewer Lookups
#'
#' @param parquet_path Path to Documents.parquet
#' @return Benchmark results
quick_benchmark <- function(parquet_path) {
  run_benchmark(parquet_path, n_lookups = 20, n_warmup = 3)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# Uncomment and modify the path below to run the benchmark:
#
results <- run_benchmark("../_package_debug/rLabelDocs/01-Classification/Documents.parquet")
#
# Or for a quick test:
#
# results <- quick_benchmark("path/to/your/project/Documents.parquet")

message("\n")
message("Benchmark script loaded!")
message("Usage:")
message("  results <- run_benchmark('path/to/Documents.parquet')")
message("  results <- quick_benchmark('path/to/Documents.parquet')  # faster, fewer lookups")
message("\n")
