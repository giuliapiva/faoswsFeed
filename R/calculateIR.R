#' Calculate intensity factor for a given country in a given year
#' 
#' This function pulls in data from GLEAM to do so

#' @param density_replacements Groups which use livestock density instead of agricultural productivity.
#' 
#' @export

calculateIR <- function(density_replacements = "1"){

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
## TODO come back and add `on` argument to these merges
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
