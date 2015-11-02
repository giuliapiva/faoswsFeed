# Calculates protein and energy units 

library(faosws)
library(faoswsUtil)
suppressPackageStartupMessages(library(data.table))
library(reshape2)
library(faoswsFeed)


if (CheckDebug()) {
  SetClientFiles("~/certificates/production")
  #GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")
  GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "f45d0a2a-a798-435d-84e9-897a572c0d10")
}

## FS and FCL code are on FAOStat 1 > SUA Working

## PROBLEMATIC CODES
# 22 (Aruba) is mapped to 532, should be 533

animalHeads = getAnimalStocks()

animalUnit = calculateAnimalUnits()
                  
## intensity factors

intensityFactor <- calculateIR()

# compile Total Feed Demand in Energy and Protein

headUnit = merge(animalHeads, animalUnit, 
                  by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"),
                  all.x = T, allow.cartesian = TRUE) 

headUnitGroup = merge(headUnit, animalCPCGroup, by = "measuredItemCPC", all.x = TRUE)


livestockDemandData = merge(headUnitGroup,intensityFactor, 
                          by = c("geographicAreaM49", "animalGroup", "timePointYears"),
                          all.x = T) 
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

feedDemandData = merge(livestockDemand, aquaDemand, 
                       by = c("geographicAreaM49", "timePointYears"),
                       all.x = T)

feedDemandData[is.na(feedDemandData)] = 0


## calculate Total feed Demand

feedDemandData[, energyDemand := livestockEnergyDemand + aquaEnergyDemand]
feedDemandData[, proteinDemand := livestockProteinDemand + aquaProteinDemand]

feedDemand = feedDemandData[, .(geographicAreaM49, timePointYears, energyDemand, proteinDemand)]
