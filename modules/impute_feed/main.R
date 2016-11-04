##packages
library(faosws)
suppressPackageStartupMessages(library(data.table))
library(faoswsFeed)

#Set environment
if (CheckDebug()) {

  library(faoswsModules)
  SETTINGS <- ReadSettings("modules/impute_feed/sws.yml")
  
  SetClientFiles(SETTINGS[["certdir"]])
  GetTestEnvironment(SETTINGS[["server"]], SETTINGS[["token"]])
}

#useSession <- as.logical(swsContext.computationParams$useSession)

feedDemand = calculateFeedDemand()#session = useSession)
#saveRDS(feedDemand, "rdata/feedDemand.rda")

## Potential Feeds (All feeditems excluding Oil meals, meals and brans)
potentialFeeds = feedNutrients[feedClassification == "Potential Feed", measuredItemCPC]


## Protein meals and Items that have only feed purpose
feedOnlyFeeds = feedNutrients[feedClassification == "FeedOnly", measuredItemCPC]

# Getofficial feed data
#define measuredElement for Feed
feedItem = "5520"
officialKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
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
         paste(c("Value_measuredElement", "flagObservationStatus_measuredElement", "flagMethod_measuredElement"), feedItem, sep = "_"),
         c("feed", "flagObservationStatus", "flagMethod"))

# Official feed is only that with official flags
officialFeed <- officialFeed[flagObservationStatus == "",]
#saveRDS(officialFeed, "rdata/officialFeed.rda")

# 3. Subtract Nutrients provided by FeedOnly items (oilcakes, brans, etc.)
  
## Retrieve Availability (Supply) of FeedOnly items
#Only those with no official data
feedOnlyAvailability = feedAvail(c("production", "imports", "exports"), 
                                 measuredItem = feedOnlyFeeds, officialData = officialFeed, negate = TRUE)

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

# For validation: Here, we need a scatterplot with log(feedonlyProteinAvailability) on the x - axis 
# and log(energydemand)  on y axis

# Set these to 0
minusfeedOnlyDemand[minusfeedOnlyEnergyDemand < 0, minusfeedOnlyEnergyDemand := 0]
minusfeedOnlyDemand[minusfeedOnlyProteinDemand < 0, minusfeedOnlyProteinDemand := 0]


# Subtracting official feed 

# Here we need to pull all official feed figures, so all feed elements with flag " "
# officialFeed = data.table[, .(measuredItemCPC, timePointYears, officialFeedValue)]

officialFeedNutrients = merge(officialFeed, feedNutrients, all.x = T, by = "measuredItemCPC")

# Calculate nutrient availabilities
officialFeedNutrients[, officialFeedEnergyAvailability := feed * energyContent]
officialFeedNutrients[, officialFeedProteinAvailability := feed * proteinContent]


### aggregate energy and protein availabilities from different items 
officialFeedNutrientSupply = officialFeedNutrients[, lapply(.SD, sum, na.rm = TRUE), 
                                                   by = .(geographicAreaM49, timePointYears),
                                                  .SDcols = c("officialFeedEnergyAvailability", 
                                                              "officialFeedProteinAvailability")]

# merge with residual demand
residualFeedDemand = merge(minusfeedOnlyDemand, officialFeedNutrientSupply, all.x = TRUE)

#Remove all NAs introduced by the merge
residualFeedDemand[is.na(residualFeedDemand)] <- 0

# subtract official feed nutrients
residualFeedDemand[, residualEnergyDemand := minusfeedOnlyEnergyDemand - officialFeedEnergyAvailability]
residualFeedDemand[, residualProteinDemand := minusfeedOnlyProteinDemand - officialFeedProteinAvailability]

# All that are less than 0 become 0
residualFeedDemand[residualEnergyDemand < 0, residualEnergyDemand := 0]
residualFeedDemand[residualProteinDemand < 0, residualProteinDemand := 0]

# 4. Establish distributions of feed based on Availability 

## Retrieve Potential feed items data
feedAvailability = feedAvail(vars = c("production", "imports", "exports", "food", "processed"), 
                             measuredItem = potentialFeeds, officialData = officialFeed, negate = TRUE)
                                                          
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
availabilityData[is.na(proteinShare), proteinShare := 0]

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

# create dataframe listing all countries and years
yearCountryList <- unique(availabilityDemand[, .(geographicAreaM49, timePointYears)])

# create auxiliary funcion which returns all years for a given country
years = function(x) {
  yearCountryList$timePointYears[yearCountryList$geographicAreaM49 == x]
  }

## Apply optimization function: note Availability is not a constraint

feedAllocated = data.table()

for (i in unique(yearCountryList[, geographicAreaM49]))
  for (j in seq_along(yearCountryList[geographicAreaM49 == i, timePointYears]))
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
feedData[,measuredElement := feedItem]
setnames(feedData, "feed", "Value")

#Remove absurdly high values
feedData <- feedData[Value < 10e22,]

setcolorder(feedData, c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears", "Value", "flagObservationStatus", "flagMethod"))
setkey(feedData, geographicAreaM49, measuredElement, measuredItemCPC, timePointYears)

#saveRDS(feedData, "rdata/feedData.rda")

results <- SaveData("agriculture", "aproduction", feedData)

paste0(paste(names(results), unlist(results), collapse = "\n", sep = ": "), "\n\nModule completed successfully")
