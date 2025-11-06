library(tidyverse)
devtools::load_all(".")

start_classification("inst/extdata/TestClassification/")

#
# "inst/extdata/TestClassification/ClassificationDetails.parquet" %>%
#   arrow::read_parquet()
