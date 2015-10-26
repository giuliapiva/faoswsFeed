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

animalKeys <- stockCodes[, measuredItemCPC]
stockKeys <- c("5111", "5112")
thousandHeads <- "5112"

key = DatasetKey(domain = "agriculture", dataset = "agriculture",
                  dimensions = list(
                    Dimension(name = "geographicAreaM49", keys = slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")), #user input
                    Dimension(name = "measuredItemCPC", keys = animalKeys), # user input
                    Dimension(name = "measuredElement", keys = stockKeys),
                    Dimension(name = "timePointYears", keys = slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")) #user input
                    
                    )
                 )
#define this as a subset of the larger data
animalHeads = GetData(key)

setnames(animalHeads, "Value", "animalHeads")

# Poultry and Rabbits are expressed in '000 heads, hence convert into heads
animalHeads[measuredElement %in% thousandHeads , animalHeads := animalHeads * 1000]

#Remove flags and measuredElement column
animalHeads[, `:=`(measuredElement = NULL, flagObservationStatus = NULL, flagMethod = NULL)]


# # animal units
# animalUnit = as.data.table(read.csv('../Data/trans/animal_unit_6-12.csv'))
# animalUnit = animalUnit[,.(Area.Code, Item.Code, Year, Energy.Factor, Protein.Factor)]
# setnames(animalUnit, c("geographicAreaM49", "measuredItemCPC", "timePointYears", 
#                       "energyFactor", "proteinFactor"))
# 
# #convert types and codes for merging
# animalUnit[,`:=`(geographicAreaM49 = fs2m49(as.character(geographicAreaM49)), 
#               measuredItemCPC = fcl2cpc(sprintf("%04d", measuredItemCPC)),
#               timePointYears = as.character(timePointYears))]

animalUnit <- calculateAnimalUnits()


## Link animal types with intensity groups
animalCPCGroup = data.table(measuredItemCPC = fcl2cpc(sprintf("%04d",
                                                              c(866, 946, 976, 1016, 1034, 1057, 1068, 1072, 1079, 1096, 1107,
                                                                1110, 1126, 1140))),
                            animalGroup = c(1, 5, 2, 2, 3, 4, 4, 4, 4, 7, 7, 7, 6, 8))                         
## intensity factors

intensityFactor  = as.data.table(read.csv("../Data/trans/IR-estimated_6-15.csv"))
intensityFactor  =  intensityFactor[, .(AREA, AnimGroup, Year, IR)]

setnames(intensityFactor, c("geographicAreaM49", "animalGroup",'timePointYears', 'intensity' ))
#convert types for merging
intensityFactor[,`:=`(geographicAreaM49 = fs2m49(as.character(geographicAreaM49)), 
                      timePointYears = as.character(timePointYears))]


# compile Total Feed Demand in Energy and Protein

headUnit = merge(animalHeads, animalUnit, 
                  by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"),
                  all.x = T, allow.cartesian = TRUE) 

headUnitGroup = merge(headUnit, animalCPCGroup, by = "measuredItemCPC", all.x = TRUE)


livestockDemandData = merge(headUnitGroup,intensityFactor, 
                          by = c("geographicAreaM49", "animalGroup", "timePointYears"),
                          all.x = T) 
livestockDemandData[is.na(livestockDemandData)] <- 0

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

aquaDemand = as.data.table(read.csv("../Data/trans/aquademand.csv"))

aquaDemand = aquaDemand[, .(area.code, year, energy, protein)]

setnames(aquaDemand, c("geographicAreaM49", "timePointYears", 
                    "aquaEnergyDemand", "aquaProteinDemand" ))

aquaDemand[,`:=`(geographicAreaM49 = fs2m49(as.character(geographicAreaM49)), 
                      timePointYears = as.character(timePointYears))]


feedDemandData = merge(livestockDemand, aquaDemand, 
                       by = c("geographicAreaM49", "timePointYears"),
                       all = T)

feedDemandData[is.na(feedDemandData)] <- 0


## calculate Total feed Demand

feedDemandData[, energyDemand := livestockEnergyDemand + aquaEnergyDemand]
feedDemandData[, proteinDemand := livestockProteinDemand + aquaProteinDemand]

feedDemand = feedDemandData[, .(geographicAreaM49, timePointYears, energyDemand, proteinDemand)]
