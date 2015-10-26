library(data.table)

stockCodes <- data.table(read.csv("data-raw/stockCodes.csv", stringsAsFactors = FALSE, colClasses = "character"), key="module")

devtools::use_data(stockCodes, overwrite = TRUE)
