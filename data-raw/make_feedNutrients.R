library(data.table)

# read original feedlist
feedNutrients = data.table(read.csv("data-raw/feedlist.csv"))

# pick only relevant variables
feedNutrients = feedNutrients[, .(CPC, ENERGY, PROTEIN, feedClassification)] 

# name properly
setnames(feedNutrients, c("measuredItemCPC", "energyContent", "proteinContent", "feedClassification"))


devtools::use_data(feedNutrients, overwrite = TRUE)
