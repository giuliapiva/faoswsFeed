animalCPCGroup <- data.table(read.csv("data-raw/animalCPCGroup.csv"))
setkey(animalCPCGroup, measuredItemCPC)

devtools::use_data(animalCPCGroup, overwrite = TRUE)
