animalCoefficients <- data.table(read.csv("data-raw/IR_factor/animalCoefficients.csv", 
                                          colClasses = c("character", "numeric")),
                                 key="animalGroup")

devtools::use_data(animalCoefficients, overwrite = TRUE)