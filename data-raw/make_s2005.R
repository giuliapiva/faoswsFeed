library(data.table)

#2005 intensification data
s2005 <- data.table(read.csv("data-raw/IR_factor/gleam2005.csv",
                             colClasses = c("character", "character", "character", "character", "numeric")),
                    key = c("geographicAreaM49", "timePointYears", "AnimalGroup"))
#Remove NA M49 codes -  Channel Islands
s2005[geographicArea == "Channel Islands", geographicAreaM49 := "830"]
s2005[geographicArea == "China, Taiwan Province of", geographicAreaM49 := "158"]

s2005[, geographicArea := NULL]

setnames(s2005, "AnimalGroup", "animalGroup")
setkey(s2005, geographicAreaM49, timePointYears, animalGroup)

devtools::use_data(s2005, overwrite = TRUE)
