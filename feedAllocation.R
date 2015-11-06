
##packages
library(faosws)
library(faoswsUtil)
suppressPackageStartupMessages(library(data.table))
#library(RJDBC)
#library(stringr)
#library(reshape2)
#library(rJava)
library(plyr)
#library(DBI)
library(faoswsFeed)

## functions
source('archive/R/feedAvail.r')
source('archive/R/optimize.r')

#Set environment
if (CheckDebug()) {
  SetClientFiles("~/certificates/production")
  #GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")
  GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "f45d0a2a-a798-435d-84e9-897a572c0d10")
}


feedDemand = calculateFeedDemand()

## Potential Feeds (All feeditems excluding Oil meals, meals and brans)
potentialFeeds = feedNutrients$measuredItemCPC[feedNutrients$feedClassification == "Potential Feed"]


## Protein meals and Items that have only feed purpose
feedOnlyFeeds = feedNutrients$measuredItemCPC[feedNutrients$feedClassification == "FeedOnly"]


# 3. Subtract Nutrients provided by FeedOnly items (oilcakes, brans, etc.)
  
## Retrieve Availability (Supply) of FeedOnly items
## This should Return data.table[, .(geographicAreaM49, measuredItemCPC, timePointYears, feedAvailability)]
feedOnlyAvailability = feedAvail(geographicAreaM49, timePointYears, feedOnlyFeeds, vars = NULL)[,
                             .(geographicAreaM49, measuredItemCPC, timePointYears, feedAvailability)]    
                    
# only keep those without official feed                                                          
feedOnlyAvailability[!feedflag == "",]

feedOnlyNutrients = merge(feedOnlyAvailability, feedNutrients, all.x=T)

### calculate Nutrient Availability
feedOnlyNutrients[, feedOnlyEnergyAvailability := feedAvailability * energyContent]
feedOnlyNutrients[, feedOnlyProteinAvailability := feedAvailability * proteinContent]


### aggregate energy and protein availabilities from different items 
feedOnlyNutrientSupply = feedOnlyNutrients[, lapply(na.omit(.SD), sum), by = .(geographicAreaM49, timePointYears),
                                 .SDcols = c("feedOnlyEnergyAvailability", "feedOnlyProteinAvailability")]


# merge with demand data
minusfeedOnlyDemand = merge(feedDemand, feedOnlyNutrientSupply, all.x = T)

### Subtract Availability from Demand
minusfeedOnlyDemand[, minusfeedOnlyEnergyDemand := energyDemand - feedOnlyEnergyAvailability]
minusfeedOnlyDemand[, minusfeedOnlyProteinDemand := proteinDemand - feedOnlyProteinAvailability]

# For validation: Here, we need a scatterplot with log(feedonlyProteinAviabilty) on the x - axis 
# and log(energydemand)  on y axis

# maybe there's a more elegant way of doing this
minusfeedOnlyDemand$minusfeedOnlyEnergyDemand[minusfeedOnlyDemand$minusfeedOnlyEnergyDemand < 0] = 0
minusfeedOnlyDemand$minusfeedOnlyProteinDemand[minusfeedOnlyDemand$minusfeedOnlyProteinDemand < 0 ] = 0


# Subtracting official feed 

# Here we need to pull all official feed figures, so all feed elements with flag " "
# officialFeed = data.table[, .(measuredItemCPC, timePointYears, officialFeedValue)]
officialFeed = feedAvail(geographicArea49, timePointYears, c(potentialFeeds, feedOnlyfeeds), vars = NULL)[, 
                                                                        feedflag == " "]# keep only official figures

officialFeedNutrients = merge(officialFeed, feedNutrients, all.x=T)

# Calculate nutrient availabilities
officialFeedNutrients[, officialFeedEnergyAvailability := feed * energyContent]
officialFeedNutrients[, officialFeedProteinAvailability := feed * proteinContent]


### aggregate energy and protein availabilities from different items 
officialFeedNutrientSupply = officialFeedNutrients[, lapply(na.omit(.SD), sum), 
                                                   by = .(geographicAreaM49, timePointYears),
                                                  .SDcols = c("officialFeedEnergyAvailability", 
                                                              "officialFeedProteinAvailability")]

# merge with residual demand
residualFeedDemand = merge(minusfeedOnlyDemand, officialFeedNutrientSupply)

# subtract official feed nutrients
residualFeedDemand[, residualEnergyDemand := minusfeedOnlyEnergyDemand - officialFeedEnergyAvailability]
residualFeedDemand[, residualProteinDemand := minusfeedOnlyEnergyDemand - officialFeedProteinAvailability]

# Again, not very elegant
residualFeedDemand$residualEnergyDemand[residualFeedDemand$residualEnergyDemand < 0] = 0
residualFeedDemand$residualProteinDemand[residualFeedDemand$residualProteinDemand < 0] = 0

# 4. Establish distributions of feed based on Availability 

## Retrieve Potential feed items data
feedAvailability <- feedAvail(geographicAreaM49, timePointYears, potentialFeeds)[!feedflag == "", ]
                                                          
# Should look something like 
# feedAvailability = data.table(geographicAreaM49, timePointYears, measuredItemCPC, feedAvailability)

## Apply nutritive factors and calculate nutrient availability
feedAvailabilityData <- merge(feedAvailability, feedNutrients, all.x=T)

feedAvailabilityData[, energyAvailability := feedAvailability * eneryContent]
feedAvailabilityData[, proteinAvailability := feedAvailability * proteinContent]

## Sum nutrient Availabilities for each country and year (for construction of shares)
nutrientAvailability = officialFeedNutrients[, lapply(na.omit(.SD), sum), 
                                                by = .(geographicAreaM49, timePointYears),
                                                .SDcols = c("energyAvailability", 
                                                             "proteinAvailability")]
# change names for merging later
setnames(nutrientAvailability,  c("energyAvailability", "proteinAvailability"),
                                c("sumEnergyAvailability", "sumProteinAvailability"))

# merge back with availability data 
availabilityData = merge(feedAvailabilityData, nutrientAvailability, 
                         by = c("geographicAreaM49", "timePointYears"), all.x = T)


# Construct shares of feed availablility
availabilityData[, energyShare = energyAvailability / sumEnergyAvailability]
availabilityData[, proteinShare = proteinAvailability / sumProteinAvailability]

# 5. Allocate Feed

## merge demand and availability data
availabilityDemand = merge(residualFeedDemand, availabilityData, 
                            by=c("geographicAreaM49", "timePointYears"))

## apply availability shares to demand and convert back to quantites
availabilityDemand[, energyBaseFeed := (energyShare * residualEnergyDemand) / energyContent]
availabilityDemand[, proteinBaseFeed := (proteinShare * residualProteinDemand) / proteinContent]

# simple version (without additional optimization:

#

# create dataframe listing all countries and years
yearCountryList <- unique(availabilityDemand[, .(geographicAreaM49, timepointYears)])

# create auxiliary funcion which returns all years for a given country
years = function(x) {
  yearCountryList$timePointYears[yearlist$geographicArea == x]
  }

## Apply optimization function: note Avialability is not a constraint

feedAllocated = data.table()

for(i in unique(yearCountryList[, timePointYears]))
  for(j in 1:length(yearCountryList$timePointYears[yearCountryList$geographicAreaM49 == i]))
    feedAllocated = rbind(feedAllocated, optimize(i, years(i)[j] ))


## Check if all conditions are met and  demand is met

# finalenergy <- ddply(feed, .(area, year), function(x) {sum(x$finalfeed * x$ENERGY)})
# colnames(finalenergy) <- c("area", "year", "finalenergy")
# finalprotein <- ddply(feed, .(area, year), function(x) {sum(x$finalfeed * x$PROTEIN)})
# colnames(finalprotein) <- c("area", "year", "finalprotein")
# feed <- merge(feed, finalenergy, by=c("area", "year"), all.x=T)
# feed <- merge(feed, finalprotein, by=c("area", "year"), all.x=T)
# 
# feed$finalcheck <- ifelse((feed$REDemand - feed$finalenergy) > 1, "GAP",
#                                ifelse((feed$RPDemand - feed$finalprotein) > 1, "GAP", "MET")) 

## Prepare all 3 feed type data for rbind
# allocated Feed
allocatedFeed = feedallocated[ ,.(geographicAreaM49, measuredItemCPC, timePointYears, allocatedFeed)]
setnames(allocatedFeed, allocatedFeed, "feed")

# flags for allocated Feeds
allocatedFeed[, ObservationFlag := rep("E", length(geographicAreaM49))]
allocatedFeed[, MethodFlag := rep("e", length(geographicAreaM49))]

# Feed-only Feed
feedOnlyFeed = feedOnlyAvailability[, .(geogrphicAreaM49, measuredItemCPC, timePointYears, feedAvailability)]
setnames(feedOnlyFeed, "feedAvailability", "feed")    

# flags for feedOnly Feed
feedOnlyAvailability[, ObservationFlag := rep("E", length(geographicAreaM49))]
feedOnlyAvailability[, MethodFlag := rep("b", length(geographicAreaM49))]

# Official Feed
reportedFeed = officialFeed[, .(geogrphicAreaM49, geographicAreaM49, measuredItemCPC, timePointYears, feed, 
                            ObervationFlag, MethodFlag)]

# rbind all datasets 
feedData <- rbind(allocatedFeed, feedOnlyFeed, reportedFeed)

setkey(feedData, geographicAreaM49, measuredItemCPC, timePointYears)
