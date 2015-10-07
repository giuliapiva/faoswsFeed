library(faosws)
library(faoswsUtil)
library(data.table)
library(reshape2)


setwd("T:/Team_working_folder/A/Total-Feed-Model/Programming/Programs/functions")
source("functions/sws_query.r")


## retrieve data on animal numbers from old sws using FCL codes (to be converted into new sws and CPC coding)
animalHeads = as.data.table(sws_query(area=1:299, item=c(866, 946, 976, 1016, 1034, 1057, 1068, 1072, 1079, 1096,
                                            1107, 1110, 1126, 1140 ), ele=11, year=1990:2012, value.names=F))

animalHeads$ele = NULL
animalHeads$flag = NULL
setnames(animalHeads, c('geographicAreaFCL', "measuredItemFCL", "timePointYears", "animalHeads"))

# Poultry and Rabbits are expressed in '000 heads, hence convert into heads
animalHeads[, animalHeads := ifelse( measuredItemFCL %in% c(1057, 1068, 1072, 1079, 1140), 
                                     animalHeads * 1000, 
                                     animalHeads)]

## animal units
setwd("T:/Team_working_folder/A/Total-Feed-Model/Programming/Data/trans")
animalUnit = as.data.table(read.csv('animal_unit_6-12.csv')) 
animalUnit = animalUnit[,.(Area.Code, Item.Code, Year, Energy.Factor, Protein.Factor)]
setnames(animalUnit, c("geographicAreaFCL", "measuredItemFCL", "timePointYears", 
                        "energyFactor", "proteinFactor"))


## Link animal types with intensity groups
animalFCLGroup = data.table( measuredItemFCL = sort(unique(animalHeads$measuredItemFCL)),
                                 animalGroup = c(1, 5, 2, 2, 3, 4, 4, 4, 4, 7, 7, 7, 6, 8))                         
## intensity factors

intensityFactor  = as.data.table(read.csv("IR-estimated_6-15.csv"))
intensityFactor  =  intensityFactor[, .(AREA, AnimGroup, Year, IR)]

setnames(intensityFactor, c("geographicAreaFCL", "animalGroup",'timePointYears', 'intensity' ))


# compile Total Feed Demand in Energy and Protein

headUnit = merge(animalHeads, animalUnit, 
                  by = c("geographicAreaFCL", "measuredItemFCL", "timePointYears"),
                  all.x=T, allow.cartesian=T) 

headUnitGroup = merge(headUnit, animalFCLGroup, by = "measuredItemFCL", all.x=T)


livestockDemandData = merge(headUnitGroup,intensityFactor, 
                          by = c("geographicAreaFCL", "animalGroup", "timePointYears"),
                          all.x=T) 
livestockDemandData[is.na(livestockDemandData)] <- 0

# 35000 and 0.319 are energy and protein requirements in MJ and 
# MT of protein per year of the base unit
livestockDemandData[, energyDemand := animalHeads * energyFactor * intensity * 35600]
livestockDemandData[, proteinDemand := animalHeads * proteinFactor * intensity * 0.319]

livestockEnergyDemand = as.data.table(aggregate(data = livestockDemandData, 
                                          energyDemand ~ geographicAreaFCL + timePointYears, 
                                          sum))
livestockProteinDemand = as.data.table(aggregate(data = livestockDemandData, 
                                                 proteinDemand ~ geographicAreaFCL + timePointYears, 
                                                 sum))
livestockDemand = merge(livestockEnergyDemand, livestockProteinDemand,
                         by = c("geographicAreaFCL", "timePointYears")) 
  
setnames(livestockDemand, c("geographicAreaFCL", "timePointYears", 
                            "livestockEnergyDemand", "livestockProteinDemand"))
## add aquaculture

aquaDemand = as.data.table(read.csv("aquademand.csv"))

aquaDemand = aquaDemand[, .(area.code, year, energy, protein)]

setnames(aquaDemand, c("geographicAreaFCL", "timePointYears", 
                    "aquaEnergyDemand", "aquaProteinDemand" ))

feedDemandData = merge(livestockDemand, aquaDemand, 
                       by = c("geographicAreaFCL", "timePointYears"),
                       all=T)

feedDemandData[is.na(feedDemandData)] <- 0


## calculate Total feed Demand

feedDemandData[, energyDemand := livestockEnergyDemand + aquaEnergyDemand]
feedDemandData[, proteinDemand := livestockProteinDemand + aquaProteinDemand]

feedDemand = feedDemandData[, .(geographicAreaFCL, timePointYears, energyDemand, proteinDemand)]
