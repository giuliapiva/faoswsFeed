library(data.table)

# read original feedlist
feedNutrients = data.table(read.csv("data-raw/feedlist.csv", stringsAsFactors = FALSE))

# pick only relevant variables
feedNutrients = feedNutrients[, .(CPC, ENERGY, PROTEIN, feedClassification)] 

# name properly
setnames(feedNutrients, c("measuredItemCPC", "energyContent", "proteinContent", "feedClassification"))

# convert energy from MJ per kg to MJ per tonne, convert protein % into share
feedNutrients[, energyContent :=  energyContent * 1000]
feedNutrients[, proteinContent :=  proteinContent / 100]

devtools::use_data(feedNutrients, overwrite = TRUE)
