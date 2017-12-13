library(data.table)

# read original feedlist
feedNutrients = fread("data-raw/feedlist.csv")

# pick only relevant variables
feedNutrients = feedNutrients[, .(CPC, ENERGY, PROTEIN, feedClassification)] 

# name properly
setnames(feedNutrients, c("measuredItemCPC", "energyContent", "proteinContent", "feedClassification"))

# convert energy from MJ per kg to MJ per tonne, convert protein % into share
feedNutrients[, energyContent :=  energyContent * 1000]
feedNutrients[, proteinContent :=  proteinContent / 100]
setkey(feedNutrients, measuredItemCPC)

devtools::use_data(feedNutrients, overwrite = TRUE)
