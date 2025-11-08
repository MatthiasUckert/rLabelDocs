library(tidyverse)
devtools::load_all(".")

start_classification("inst/extdata/TestClassification/")

#
# "inst/extdata/TestClassification/ClassificationDetails.parquet" %>%
#   arrow::read_parquet()

db_file <- file.path("inst/extdata/TestClassification/classification_data.db")

# Connect to database (creates file if doesn't exist)
con <- DBI::dbConnect(RSQLite::SQLite(), db_file)

dplyr::tbl(con, "classification_log") %>%
  dplyr::arrange(dplyr::desc(timestamp)) %>%
  dplyr::collect() %>%
  dplyr::distinct(doc_id, class, .keep_all = TRUE)
