animalCPCGroup <- data.table(read.csv("data-raw/animalCPCGroup.csv", colClasses="character"))
setkey(animalCPCGroup, measuredItemCPC)

devtools::use_data(animalCPCGroup, overwrite = TRUE)
