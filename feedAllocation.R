##packages
library(faosws)
suppressPackageStartupMessages(library(data.table))
library(faoswsFeed)

#Set environment
if (CheckDebug()) {
  #SetClientFiles("~/certificates/production")
  SetClientFiles("~/certificates/qa")
  #GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")
  #GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "f45d0a2a-a798-435d-84e9-897a572c0d10")
  #GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "326db9b9-411d-4a0f-b1a9-0ac6ec4b4bad")
  #GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", "e412705a-f02c-4dfa-85c4-1d1132b58dcf")
  #GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", "0bb0db3f-8681-4a4a-bf14-e36635574f30")
  #GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", "98a2d80b-e55b-424a-af62-05890a9bcb6b")
  #GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", "af9323ee-8878-4870-b559-851c2bf5558a")
  GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", "fbbfebbe-2eae-4e19-8d46-514345bcdbf5")
}


feedDemand = calculateFeedDemand()

## Potential Feeds (All feeditems excluding Oil meals, meals and brans)
potentialFeeds = feedNutrients[feedClassification == "Potential Feed", measuredItemCPC]


## Protein meals and Items that have only feed purpose
feedOnlyFeeds = feedNutrients[feedClassification == "FeedOnly", measuredItemCPC]

# Getofficial feed data
#define measuredElement for Feed
feedItem = "5520"
officialKey = DatasetKey(domain = "agriculture", dataset = "agriculture",
                          dimensions = list(
                            Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                            Dimension(name = "measuredItemCPC", keys = c(potentialFeeds, feedOnlyFeeds)),
                            Dimension(name = "measuredElement", keys = feedItem),
                            Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                          ),
                          sessionId =  slot(swsContext.datasets[[1]], "sessionId")
)

officialFeed = GetData(officialKey, pivoting = c(Pivoting(code = "timePointYears", ascending = TRUE),
                                Pivoting(code = "geographicAreaM49", ascending = FALSE),
                                Pivoting(code = "measuredItemCPC", ascending = FALSE),
                                Pivoting(code = "measuredElement", ascending = FALSE)),
        normalized = F)


setnames(officialFeed, 
         paste(c("Value_measuredElement", "flagObservationStatus_measuredElement", "flagMethod_measuredElement"), feedItem, sep="_"),
         c("feed", "flagObservationStatus", "flagMethod"))

# Official feed is only that with official flags
officialFeed <- officialFeed[flagObservationStatus == "",]

#HORRIBLE HACK - concatenate keys in order to do anti join. See http://stackoverflow.com/a/33667203/1465387 with data.table 1.9.6
officialFlagKeys <- apply(officialFeed[ , .(geographicAreaM49, measuredItemCPC, timePointYears)], 1, paste, collapse="&")

# 3. Subtract Nutrients provided by FeedOnly items (oilcakes, brans, etc.)
  
## Retrieve Availability (Supply) of FeedOnly items
#Only those with no official data
feedOnlyAvailability = feedAvail(c("production", "imports", "exports"), 
                                 measuredItem = feedOnlyFeeds, amperflagskeys = officialFlagKeys, negate = TRUE)[, 
                                      .(geographicAreaM49, measuredItemCPC, timePointYears, feedAvailability)]

feedOnlyNutrients = merge(feedOnlyAvailability, feedNutrients, all.x = T, by = "measuredItemCPC")

### calculate Nutrient Availability
feedOnlyNutrients[, feedOnlyEnergyAvailability := feedAvailability * energyContent]
feedOnlyNutrients[, feedOnlyProteinAvailability := feedAvailability * proteinContent]


### aggregate energy and protein availabilities from different items 
feedOnlyNutrientSupply = feedOnlyNutrients[, lapply(.SD, sum, na.rm = TRUE), by = .(geographicAreaM49, timePointYears),
                                 .SDcols = c("feedOnlyEnergyAvailability", "feedOnlyProteinAvailability")]


# merge with demand data
minusfeedOnlyDemand = merge(feedDemand, feedOnlyNutrientSupply, all.x = T)

### Subtract Availability from Demand
minusfeedOnlyDemand[, minusfeedOnlyEnergyDemand := energyDemand - feedOnlyEnergyAvailability]
minusfeedOnlyDemand[, minusfeedOnlyProteinDemand := proteinDemand - feedOnlyProteinAvailability]

# For validation: Here, we need a scatterplot with log(feedonlyProteinAviabilty) on the x - axis 
# and log(energydemand)  on y axis

# Set these to 0
minusfeedOnlyDemand[minusfeedOnlyEnergyDemand < 0, minusfeedOnlyEnergyDemand := 0]
minusfeedOnlyDemand[minusfeedOnlyProteinDemand < 0, minusfeedOnlyProteinDemand := 0]


# Subtracting official feed 

# Here we need to pull all official feed figures, so all feed elements with flag " "
# officialFeed = data.table[, .(measuredItemCPC, timePointYears, officialFeedValue)]

officialFeedNutrients = merge(officialFeed, feedNutrients, all.x=T, by="measuredItemCPC")

# Calculate nutrient availabilities
officialFeedNutrients[, officialFeedEnergyAvailability := feed * energyContent]
officialFeedNutrients[, officialFeedProteinAvailability := feed * proteinContent]


### aggregate energy and protein availabilities from different items 
officialFeedNutrientSupply = officialFeedNutrients[, lapply(.SD, sum, na.rm=TRUE), 
                                                   by = .(geographicAreaM49, timePointYears),
                                                  .SDcols = c("officialFeedEnergyAvailability", 
                                                              "officialFeedProteinAvailability")]

# merge with residual demand
residualFeedDemand = merge(minusfeedOnlyDemand, officialFeedNutrientSupply)

#Remove all NAs introduced by the merge
residualFeedDemand[is.na(residualFeedDemand)] <- 0

# subtract official feed nutrients
residualFeedDemand[, residualEnergyDemand := minusfeedOnlyEnergyDemand - officialFeedEnergyAvailability]
residualFeedDemand[, residualProteinDemand := minusfeedOnlyEnergyDemand - officialFeedProteinAvailability]

# All that are less than 0 become 0
residualFeedDemand[residualEnergyDemand < 0, residualEnergyDemand := 0]
residualFeedDemand[residualProteinDemand < 0, residualProteinDemand := 0]

# 4. Establish distributions of feed based on Availability 

## Retrieve Potential feed items data
feedAvailability = feedAvail(vars = c("production", "imports", "exports", "food"), 
                             measuredItem = potentialFeeds, amperflagskeys = officialFlagKeys, negate = TRUE)
                                                          
# Should look something like 
# feedAvailability = data.table(geographicAreaM49, timePointYears, measuredItemCPC, feedAvailability)

## Apply nutritive factors and calculate nutrient availability
feedAvailabilityData <- merge(feedAvailability, feedNutrients, all.x = T)

feedAvailabilityData[, energyAvailability := feedAvailability * energyContent]
feedAvailabilityData[, proteinAvailability := feedAvailability * proteinContent]

## Sum nutrient Availabilities for each country and year (for construction of shares)
nutrientAvailability = feedAvailabilityData[, lapply(.SD, sum, na.rm = TRUE), 
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
availabilityData[, energyShare := energyAvailability / sumEnergyAvailability]
availabilityData[, proteinShare := proteinAvailability / sumProteinAvailability]
#Remove NaNs introduced by 0 / 0
availabilityData[is.na(energyShare), energyShare := 0]
availabilityData[is.na(proteinShare), energyShare := 0]

# 5. Allocate Feed

## merge demand and availability data
availabilityDemand = merge(residualFeedDemand, availabilityData, 
                            by = c("geographicAreaM49", "timePointYears"))

## apply availability shares to demand and convert back to quantites
availabilityDemand[, energyBaseFeed := (energyShare * residualEnergyDemand) / energyContent]
availabilityDemand[, proteinBaseFeed := (proteinShare * residualProteinDemand) / proteinContent]
#Remove NaNs introduced by 0 / 0
availabilityDemand[is.na(energyBaseFeed), energyBaseFeed := 0]
availabilityDemand[is.na(proteinBaseFeed), proteinBaseFeed := 0]

# simple version (without additional optimization:

#

# create dataframe listing all countries and years
yearCountryList <- unique(availabilityDemand[, .(geographicAreaM49, timePointYears)])

# create auxiliary funcion which returns all years for a given country
years = function(x) {
  yearCountryList$timePointYears[yearCountryList$geographicAreaM49 == x]
  }

## Apply optimization function: note Avialability is not a constraint

feedAllocated = data.table()

for(i in unique(yearCountryList[, geographicAreaM49]))
  for(j in seq_along(yearCountryList[geographicAreaM49 == i, timePointYears]))
    feedAllocated = rbind(feedAllocated, optimizeFeed(i, years(i)[j] ))


## Prepare all 3 feed type data for rbind
# allocated Feed
allocatedFeed = feedAllocated[ ,.(geographicAreaM49, measuredItemCPC, timePointYears, allocatedFeed)]
setnames(allocatedFeed, "allocatedFeed", "feed")

# flags for allocated Feeds
allocatedFeed[, `:=`(flagObservationStatus = "I",
                     flagMethod = "e")]

# Feed-only Feed
feedOnlyFeed = feedOnlyAvailability[, .(geographicAreaM49, measuredItemCPC, timePointYears, feedAvailability)]
setnames(feedOnlyFeed, "feedAvailability", "feed")    

# flags for feedOnly Feed
feedOnlyFeed[, `:=`(flagObservationStatus = "I",
                    flagMethod = "b")]

# rbind all datasets 
feedData <- rbind(allocatedFeed, feedOnlyFeed)
feedData[,measuredElement := "5520"]
setnames(feedData, "feed", "Value")
setcolorder(feedData, c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears", "Value", "flagObservationStatus", "flagMethod"))
setkey(feedData, geographicAreaM49, measuredElement, measuredItemCPC, timePointYears)

SaveData("agriculture", "agriculture", feedData)
