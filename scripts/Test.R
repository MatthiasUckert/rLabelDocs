library(tidyverse)
devtools::load_all(".")

dir_ <- "../_package_debug/rLabelDocs/01-Classification/"
start_classification(project_dir = dir_)

#
#
# arrow::open_dataset("inst/extdata/TestClassification/Documents.parquet")
#
# db_file <- file.path("inst/extdata/TestClassification/classification_data.db")
#
# # Connect to database (creates file if doesn't exist)
# con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
#
#
#
#
# get_db_connection(dir_) %>%
#   dplyr::tbl("classification_log") %>%
#   dplyr::arrange(dplyr::desc(timestamp)) %>%
#   dplyr::collect() %>%
#   dplyr::distinct(doc_id, class, .keep_all = TRUE)
