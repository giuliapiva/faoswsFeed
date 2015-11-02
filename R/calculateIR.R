#' Calculate intensity factor for a given country in a given year
#' 
#' This function pulls in data from GLEAM to do so

#' @param density_replacements Groups which use livestock density instead of agricultural productivity.

calculateIR <- function(density_replacements = "1"){


#2005 intensification data
s2005 <- data.table(read.csv("data-raw/IR_factor/gleam2005.csv",
                             colClasses = c("character", "NULL", "integer", "character", "numeric")),
                    key = c("geographicAreaM49", "timePointYears", "AnimalGroup"))
#Remove NA M49 codes -  Channel Islands
s2005 <- s2005[!is.na(geographicAreaM49),]
s2005[,AnimalGroup := as.character(AnimalGroup)]
setnames(s2005, "AnimalGroup", "animalGroup")
setkey(s2005, geographicAreaM49, timePointYears, animalGroup)

livestockDensity <- calculateLivestockDensity(animalCPCGroup[animalGroup %in% density_replacements, measuredItemCPC],
                                              addyear = "2005")

mergeAnimalkeys <- lapply(animalCoefficients[,animalGroup], 
                          function(x){data.frame(animalGroup=x, 
                                                 wdi[,.(geographicAreaM49, timePointYears)])})

mergeAnimalkeys <- rbindlist(mergeAnimalkeys)
setkeyv(mergeAnimalkeys, names(mergeAnimalkeys))

mergeAnimalkeys <- animalCoefficients[mergeAnimalkeys]
setkey(mergeAnimalkeys, geographicAreaM49, timePointYears, animalGroup)

#Merge data for all years together
rawLabor <- livestockDensity[mergeAnimalkeys[wdi]]
#For cattle (code 1) use livestock density instead of agricultural productivity
rawLabor[,densprod := productivity]
rawLabor[animalGroup %in% density_replacements, densprod := livestockDensity]

betaTable <- rawLabor[s2005]


calculateBeta <- function(intensity2005, param, densprod2005){
  log(intensity2005/(1 - intensity2005)) - param * log(densprod2005)
}

betas <- betaTable[, .(beta = calculateBeta(intensity, coefficient, densprod)),
                   by = .(animalGroup, geographicAreaM49)]
setkey(betas, geographicAreaM49, animalGroup)


IRTable <- merge(betas, rawLabor, by = c("geographicAreaM49", "animalGroup"))

calculateIntensity <- function(beta, coefficient, densprod){
  nominator <- exp(beta) * densprod ^ coefficient
  nominator / (1 + nominator)
}

IRCalculated <- IRTable[, .(intensity = calculateIntensity(beta, coefficient, densprod)),
                          by = .(animalGroup, geographicAreaM49, timePointYears)]
#Give relevant cols and remove 2005 if not in query

areaKeys <- getQueryKey("geographicAreaM49")
yearKeys <- getQueryKey("timePointYears")

IRCalculated[geographicAreaM49 %in% areaKeys & timePointYears %in% yearKeys,
             .(geographicAreaM49, animalGroup, timePointYears, intensity)]

}
