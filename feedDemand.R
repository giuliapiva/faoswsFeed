# Calculates protein and energy units 

library(faosws)
library(faoswsUtil)
library(data.table)
library(reshape2)


#setwd("T:/Team_working_folder/A/Total-Feed-Model/Programming/Programs/functions")
source("functions/sws_query.r")


if(CheckDebug()){
  SetClientFiles("~/certificates/production")
}

GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")


## retrieve data on animal numbers from old sws using FCL codes (to be converted into new sws and CPC coding)
# animalHeads = as.data.table(sws_query(area=1:299, item=c(866, 946, 976, 1016, 1034, 1057, 1068, 1072, 1079, 1096,
#                                             1107, 1110, 1126, 1140 ), ele=11, year=1990:2012, value.names=F,
#                                       class.path = 'functions/ojdbc14.jar'))

## FS and FCL code are on FAOStat 1 > SUA Working

## PROBLEMATIC CODES
# 22 (Aruba) is mapped to 532, should be 533

key = DatasetKey(domain="agriculture", dataset="agriculture",
                  dimensions=list(
                    Dimension(name="geographicAreaM49", keys=na.omit(fs2m49(as.character((1:299)[-22])))),
                    Dimension(name="measuredItemCPC", keys=na.omit(fcl2cpc(sprintf("%04d", c(866, 946, 976, 1016, 1034, 1057, 1068, 1072, 1079, 1096,
                                                                                             1107, 1110, 1126, 1140 ))))),
                    Dimension(name="measuredElement", keys=c("5111", "5112")),
                    Dimension(name="timePointYears", keys=as.character(1990:2012))
                    
                    )
                 )
animalHeads = GetData(key)

#animalHeads$ele = NULL
#animalHeads$flag = NULL
animalHeads[, `:=`(measuredElement=NULL, flagObservationStatus=NULL, flagMethod=NULL)]
setnames(animalHeads, c('geographicAreaM49', "measuredItemCPC", "timePointYears", "animalHeads"))

# Poultry and Rabbits are expressed in '000 heads, hence convert into heads
animalHeads[, animalHeads := ifelse( measuredItemCPC %in% fcl2cpc(as.character(c(1057, 1068, 1072, 1079, 1140))), 
                                     animalHeads * 1000, 
                                     animalHeads)]

## animal units
#setwd("T:/Team_working_folder/A/Total-Feed-Model/Programming/Data/trans")
animalUnit = as.data.table(read.csv('../Data/trans/animal_unit_6-12.csv'))
animalUnit = animalUnit[,.(Area.Code, Item.Code, Year, Energy.Factor, Protein.Factor)]
setnames(animalUnit, c("geographicAreaM49", "measuredItemCPC", "timePointYears", 
                        "energyFactor", "proteinFactor"))

#convert types for merging
animalUnit[,`:=`(geographicAreaM49=as.character(geographicAreaM49), 
              measuredItemCPC=as.character(measuredItemCPC), 
              timePointYears=as.character(timePointYears))]


## Link animal types with intensity groups
animalCPCGroup = data.table(measuredItemCPC = sort(unique(animalHeads$measuredItemCPC)),
                                 animalGroup = c(1, 5, 2, 2, 3, 4, 4, 4, 4, 7, 7, 7, 6, 8))                         
## intensity factors

intensityFactor  = as.data.table(read.csv("../Data/trans/IR-estimated_6-15.csv"))
intensityFactor  =  intensityFactor[, .(AREA, AnimGroup, Year, IR)]

setnames(intensityFactor, c("geographicAreaM49", "animalGroup",'timePointYears', 'intensity' ))
#convert types for merging
intensityFactor[,`:=`(geographicAreaM49=as.character(geographicAreaM49), 
                      timePointYears=as.character(timePointYears))]


# compile Total Feed Demand in Energy and Protein

headUnit = merge(animalHeads, animalUnit, 
                  by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"),
                  all.x=T, allow.cartesian=T) 

headUnitGroup = merge(headUnit, animalCPCGroup, by = "measuredItemCPC", all.x=T)


livestockDemandData = merge(headUnitGroup,intensityFactor, 
                          by = c("geographicAreaM49", "animalGroup", "timePointYears"),
                          all.x=T) 
livestockDemandData[is.na(livestockDemandData)] <- 0

# 35000 and 0.319 are energy and protein requirements in MJ and 
# MT of protein per year of the base unit
livestockDemandData[, energyDemand := animalHeads * energyFactor * intensity * 35600]
livestockDemandData[, proteinDemand := animalHeads * proteinFactor * intensity * 0.319]

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

aquaDemand[,`:=`(geographicAreaM49=as.character(geographicAreaM49), 
                      timePointYears=as.character(timePointYears))]


feedDemandData = merge(livestockDemand, aquaDemand, 
                       by = c("geographicAreaM49", "timePointYears"),
                       all=T)

feedDemandData[is.na(feedDemandData)] <- 0


## calculate Total feed Demand

feedDemandData[, energyDemand := livestockEnergyDemand + aquaEnergyDemand]
feedDemandData[, proteinDemand := livestockProteinDemand + aquaProteinDemand]

feedDemand = feedDemandData[, .(geographicAreaM49, timePointYears, energyDemand, proteinDemand)]

