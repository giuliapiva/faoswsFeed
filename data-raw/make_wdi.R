library(data.table)
library(countrycode)

#Agricultural productivity data
rawWdi <- data.table(read.csv("data-raw/IR_factor/wdi-productivity_6-12.csv", stringsAsFactors = FALSE))
#Remove useless metadata at bottom of csv by keeping rows with country names
rawWdi <- rawWdi[Country.Name != "",]

#Make years all one column
wdi <- melt(rawWdi, c("Country.Name", "Country.Code", "Series.Name", "Series.Code"), 
            variable.name = "timePointYears", value.name = "productivity")
#Remove 'y' prefix
wdi[, timePointYears := sub("y", "", timePointYears)]
#Change names to M49
wdi[, geographicAreaM49 := as.character(countrycode(Country.Code, "wb", "iso3n"))]
#Remove any countries that aren't converted (probably Kosovo and Channel Islands)
wdi <- wdi[!is.na(geographicAreaM49),]
#keep only select columns
wdi <- wdi[,.(geographicAreaM49, timePointYears, productivity)]
setkey(wdi, geographicAreaM49, timePointYears)

devtools::use_data(wdi, overwrite = TRUE)

