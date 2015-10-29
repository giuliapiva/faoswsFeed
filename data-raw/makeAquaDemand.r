library(stringr)
library(plyr)
library(data.table)
library(countrycode)

# Read the production file
aquaProductionData = data.table(read.csv('data-raw/aquaData/globalAquacultureProduction.csv"'))

# Remove the flag columns
aquaProductionData[, c(which(like(colnames(aquaProductionData), "S_"))) := NULL]

# Produce M49 codes using countrycode
aquaProductionData[, geographicAreaM49 := countrycode(Land.Area, 
                                               origin = "country.name", 
                                               destination = "iso3n", 
                                               warn =F)] 


# Remove the areas without code (they're non existent)
aquaProductionData = aquaProductionData[!is.na(geographicAreaM49), ]

# Rename column names
setnames(aquaProductionData, c(1,2,3,4,5), c("geographicArea", "oceanArea", 
                                             "waterEnvironment", "figisSpecies", "scientificName"))

# Ensure that all data columns are numeric
aquaProductionData[, (6:(ncol(aquaProductionData)-1)) := lapply(.SD, as.numeric), 
                   .SDcols = 6:(ncol(aquaProductionData)-1)]

# From wide to long
aquaProductionLongData = melt(aquaProductionData, 
                      id=c("geographicAreaM49", "geographicArea", "oceanArea",
                           "waterEnvironment", "figisSpecies", "scientificName"), 
                      na.rm=TRUE)

setnames(aquaProductionLongData, c("variable", "value"), c("timePointYears", "aquaProduction")) 

# adjust timePointYears
aquaProductionLongData[, timePointYears := gsub("X", "", timePointYears)]

# fish data and species map
speciesMap = data.table(read.csv("data-raw/speciesMap.csv"))

# merge production data with species groups
aquaProductionMergedData = merge(aquaProductionLongData, speciesMap, 
                                 by = "figisSpecies", all.x=T, allow.cartesian = T)

# sum up figisSpecies within each aquaSpecies
aquaProduction= as.data.table(aggregate(data = aquaProductionMergedData, 
                          aquaProduction ~ geographicAreaM49 + geographicArea + timePointYears + aquaSpecies, 
                          sum))

# reorder
setkey(aquaProduction, geographicAreaM49, timePointYears)


## import parameter survey data
parameters <- data.table(read.csv('/home/bernhard/faoswsFeed/data-raw/aquaSurveyData.csv'))


## predict missing parameter values

target = ddply(parameters, c("aquaSpecies"), 
               summarise, 
               timePointYears = min(aquaProduction$timePointYears):max(aquaProduction$timePointYears))

for(i in c("feedConversionRate", "proportionOnFeed")){

    estimates = ddply(parameters, .(aquaSpecies), 
              function(x) {predict(lm(get(i) ~ poly(timePointYears, 2), data=x ), 
              newdata=target)})[,c(0:length(unique(aquaProduction$timePointYears))+1)]
  
    estimatesLong = melt(estimates, id="aquaSpecies", na.rm =T, value.name = i) 
    estimatesLongOrdered = estimatesLong[order(estimatesLong$aquaSpecies),]
     
    target = cbind(target, estimatesLongOrdered[[i]])

}


# change names
setnames(target, c("aquaSpecies", "timePointYears", 
                             "fittedFeedConversionRate", "fittedProportionOnFeed"))

# merge with survey data
fittedParameters = merge(parameters, target, by = c("aquaSpecies","timePointYears"), all.y=T)

# construct combined parameters
fittedParameters[, proportionOnFeed := ifelse(is.na(proportionOnFeed),                                           
                                                  fittedProportionOnFeed, 
                                                  proportionOnFeed)]
                       
fittedParameters[, feedConversionRate := ifelse(is.na(feedConversionRate), 
                                                  fittedFeedConversionRate, 
                                                  feedConversionRate)]
                     


parameterValues = fittedParameters[, .(aquaSpecies, timePointYears, proportionOnFeed, feedConversionRate)]

parameterValues[, timePointYears := as.character(timePointYears)]

# merge with Production Data
aquaProductionParameters = merge(aquaProduction, parameterValues, by = c("aquaSpecies", "timePointYears"), all=T)
  

# Country Specific Survey Data on fcr
pointData = data.table(read.csv('data-raw/fcr2006.csv')) 

pointData[, geographicAreaM49 := countrycode(area.code, 
                                             origin = "fao", 
                                             destination = "iso3n", 
                                             warn =T)] 

## Manually adjust M49, fao codes for those countries are not (yet) translated 

pointData$geographicAreaM49[pointData$area.code == 153] = 540
pointData$geographicAreaM49[pointData$area.code == 214] = 158

# Ensure that all data columns are numeric before melt
pointData[, (3:(ncol(pointData)-1)) := lapply(.SD, as.numeric), 
                   .SDcols = 3:(ncol(pointData)-1)]

# melt into long format
pointDataLong = melt(pointData, id=c("area.code","timePointYears", "geographicAreaM49"), 
                     na.rm = T, 
                     variable.name = "aquaSpecies", value.name = "pointFeedConversionRate") 

pointDataLong[, aquaSpecies := sub("[.]", " ", aquaSpecies) ]
 
surveyData = pointDataLong[, .(geographicAreaM49, timePointYears, aquaSpecies, pointFeedConversionRate)]

surveyData[, timePointYears := as.character(timePointYears)]

## apply prediction method also to missing country specific EFCR
aquaFitted = merge(aquaProductionParameters, surveyData, by = c("geographicAreaM49", "timePointYears", "aquaSpecies"), all.x = T)
                               
 extrapolate <- function(x) {
                           
          setkey(aquaFitted, geographicAreaM49, aquaSpecies, timePointYears)
          #Create Variables for year change calculation             
          aquaFitted[, Y2Ychange := (proportionOnFeed - c(0, proportionOnFeed[1:length(proportionOnFeed)-1])) / 
                 c(0, proportionOnFeed[1:length(proportionOnFeed)-1])]
                           
          aquaFitted$Y2Ychange[aquaFitted$timePointYears == min(aquaFitted$timePointYears)] = 0
                           
          aquaFitted[, pointFeedConversionRatenext := c(pointFeedConversionRate[2:length(pointFeedConversionRate)],
                                                0) ]
          aquaFitted[, pointFeedConversionRateprevious := c(0,
               pointFeedConversionRate[1:length(pointFeedConversionRate)-1])] 
    
          aquaFitted[, Y2Ychangenext := c(Y2Ychange[2:length(Y2Ychange)],0) ]
 # extrapolate feedConversionRate                          
ifelse(aquaFitted$timePointYears == x & x < unique(surveyData$timePointYears),
        aquaFitted$pointFeedConversionRatenext * (1 + aquaFitted$Y2Ychangenext),
            ifelse(x == aquaFitted$timePointYears & x > unique(surveyData$timePointYears), 
                aquaFitted$pointFeedConversionRateprevious * (1 - aquaFitted$Y2Ychange),
                   aquaFitted$pointFeedConversionRate))
 }

 # pick years with missing values and order according to function
 missing = c(aquaFitted[,unique(timePointYears[is.na(pointFeedConversionRate) 
                      & timePointYears > unique(surveyData$timePointYears)])],
                        sort(aquaFitted[, unique(timePointYears[timePointYears < unique(surveyData$timePointYears)])],
                             decreasing = TRUE))
                                                 
# Apply function
 for(i in c(missing))   
   aquaFitted[, pointFeedConversionRate:= extrapolate(i)]
 
 aquaFitted[, feedConversionRate := ifelse(is.na(pointFeedConversionRate), 
                                      feedConversionRate, 
                                      pointFeedConversionRate)]
                                             
# Pick variables needed (remove everything not required anymore)
 aquaProductionFull = aquaFitted[ , .(geographicAreaM49, timePointYears, aquaSpecies, proportionOnFeed,
                                  feedConversionRate, aquaProduction)]


# merge with nutrient factors
fishData = data.table(read.csv("data-raw/aquaTable.csv"))
aquaDemandData = merge(aquaProductionFull, fishData, by = "aquaSpecies") 
   
# Calculate Aquafeed demand
aquaDemandData[, aquaEnergyDemand := aquaProduction * feedConversionRate * proportionOnFeed * energy]
aquaDemandData[, aquaProteinDemand := aquaProduction * feedConversionRate * proportionOnFeed * protein]

# Sum Species Demands up to arrive at country aqua demand
aquaDemand = aquaDemandData[, lapply(.SD, sum), by=.(geographicAreaM49, timePointYears), .SDcols=c("aquaEnergyDemand", "aquaProteinDemand")]

aquaDemandTable = aquaDemand

devtools::use_data(aquaDemandTable, overwrite = TRUE)




