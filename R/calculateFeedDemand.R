#' Calculates protein and energy units 
#' 
#' Read in animal stocks and such to get that
#' 
#' @export

calculateFeedDemand <- function(){

## FS and FCL code are on FAOStat 1 > SUA Working
animalHeads = getAnimalStocks()
#saveRDS(animalHeads, "rdata/animalHeads.rda")

animalUnit = calculateAnimalUnits()
#saveRDS(animalUnit, "rdata/animalUnit.rda")
                  
## intensity factors

intensityFactor <- calculateIR()
#saveRDS(intensityFactor, "rdata/intensityFactor.rda")

# compile Total Feed Demand in Energy and Protein

headUnit = merge(animalHeads, animalUnit, 
                  by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"),
                  all.x = T, allow.cartesian = TRUE) 

headUnitGroup = merge(headUnit, animalCPCGroup, by = "measuredItemCPC", all.x = TRUE)


livestockDemandData = merge(headUnitGroup,intensityFactor, 
                          by = c("geographicAreaM49", "animalGroup", "timePointYears"),
                          all = T) 
livestockDemandData[is.na(livestockDemandData)] = 0

# 35000 and 0.319 are energy and protein requirements in MJ and 
# MT of protein per year of the base unit
livestockDemandData[, energyDemand := animalHeads * energy * intensity * 35600]
livestockDemandData[, proteinDemand := animalHeads * protein * intensity * 0.319]

livestockEnergyDemand = as.data.table(aggregate(data = livestockDemandData, 
                                          energyDemand ~ geographicAreaM49 + timePointYears, 
                                          sum))
livestockProteinDemand = as.data.table(aggregate(data = livestockDemandData, 
                                                 proteinDemand ~ geographicAreaM49 + timePointYears, 
                                                 sum))
livestockDemand = merge(livestockEnergyDemand, livestockProteinDemand,
                         by = c("geographicAreaM49", "timePointYears")) 
  
setnames(livestockDemand, c("geographicAreaM49", "timePointYears", 
                            "livestockEnergyDemand", "livestockProteinDemand"))
## add aquaculture

aquaDemand <- calculateAquaDemand()
#saveRDS(aquaDemand, "rdata/aquaDemand.rda")

feedDemandData = merge(livestockDemand, aquaDemand, 
                       by = c("geographicAreaM49", "timePointYears"),
                       all = T)

feedDemandData[is.na(feedDemandData)] = 0


## calculate Total feed Demand

feedDemandData[, energyDemand := livestockEnergyDemand + aquaEnergyDemand]
feedDemandData[, proteinDemand := livestockProteinDemand + aquaProteinDemand]

feedDemand = feedDemandData[, .(geographicAreaM49, timePointYears, energyDemand, proteinDemand)]

feedDemand
#saveRDS(feedDemand, "rdata/feedDemand.rda")

}