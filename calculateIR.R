library(faosws)
library(countrycode)
library(reshape2)
library(data.table)

#2005 intensification data
s2005 <- data.table(read.csv("data-raw/IR_factor/gleam2005.csv",
                             colClasses = c("character", "NULL", "integer", "character", "numeric")),
                    key = c("geographicAreaM49", "timePointYears", "AnimalGroup"))
#Remove NA M49 codes -  Channel Islands
s2005 <- s2005[!is.na(geographicAreaM49),]
setnames(s2005, "AnimalGroup", "animalGroup")
raw_wdi <- data.table(read.csv("data-raw/IR_factor/wdi-productivity_6-12.csv", stringsAsFactors = FALSE))

#Remove useless metadata at bottom of csv by keeping rows with country names
raw_wdi <- raw_wdi[Country.Name != "",]
#Make years all one column
wdi <- melt(raw_wdi, c("Country.Name", "Country.Code", "Series.Name", "Series.Code"), 
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

#TODO: Add this to animalCPCGroup?
animalConstants = data.table(animalGroup=1:8, coefficient=c(0.675328547, 0.160315045
, 0.357946158, 0.347057854, 0.157, 0.164, 0.186, 0.265), key="animalGroup")

mergeAnimalkeys <- lapply(animalConstants[,animalGroup], 
                          function(x){data.frame(animalGroup=x, 
                                                 wdi[,.(geographicAreaM49, timePointYears)])})

mergeAnimalkeys <- rbindlist(mergeAnimalkeys)
setkeyv(mergeAnimalkeys, names(mergeAnimalkeys))

mergeAnimalkeys <- animalConstants[mergeAnimalkeys]
setkey(mergeAnimalkeys, geographicAreaM49, timePointYears, animalGroup)

rawLabor <- mergeAnimalkeys[wdi]

betaTable <- rawLabor[s2005]

calculateBeta <- function(intensity2005, param, densprod2005){
  log(intensity2005/(1 - intensity2005)) - param * log(densprod2005)
}

betas <- betaTable[, .(beta = calculateBeta(intensity, coefficient, productivity)),
                   by = .(animalGroup, geographicAreaM49)]
