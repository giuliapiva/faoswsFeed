#' Calculate Feed Availability
#' 
#' This function provides feed availabilities. Using the equation:
#' Production - Exports + Imports + Stockvariation = Food + Seed + Other Uses + Waste + Processed
#' usage of single summands depends on user input. Data is downloaded from FAOSTAT sws
#' 
#' @param vars character. Variables that should be included along with production, feed, imports and exports
#' @param measuredItem character. Vector of CPC values by which the results should be subsetted
#' @param flags character. If not NULL, subset keep only data with these flags
#' @param negate character. When flags is not NULL, keep only data without these flags
#' 
#' @export

feedAvail = function(vars = c("food", "processed"), measuredItem = feedNutrients$measuredItemCPC, flags = NULL, negate = FALSE) {

  usedvars = c("production", "feed", "imports", "exports", vars)
  
  feedCodeTable = data.table(measuredElement = c("5510", "5525", "5015", "5520", "5023", "5153", "5141", "5600", "5900"),
                             dataset = c("production", "production", "production", "production", "production", "production", "production", "trade", "trade"),
                              variable = c("production", "seed", "waste", "feed", "processed", "other", "food", "imports", "exports"))
  
  productionKey = DatasetKey(domain = "agriculture", dataset = "agriculture",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                         Dimension(name = "measuredItemCPC", keys = measuredItem),
                         Dimension(name = "measuredElement", keys = 
                                     feedCodeTable[variable %in% usedvars & dataset == "production", measuredElement]),
                         Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                       ),
                       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
                   )
  
  productionData = GetData(productionKey, flags = !is.null(flags))
  setkey(productionData, geographicAreaM49, measuredItemCPC, measuredElement, timePointYears)
  
  #Remove this column so the rbind goes ahead
  if (!is.null(flags)) {productionData[, flagMethod := NULL]}
  
  tradeKey = DatasetKey(domain = "trade", dataset = "total_trade_CPC",
                        dimensions = list(
                          Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                          Dimension(name = "measuredItemCPC", keys = measuredItem),
                          Dimension(name = "measuredElementTrade", keys = 
                                      feedCodeTable[variable %in% usedvars & dataset == "trade", measuredElement]),
                          Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                        ),
                        sessionId =  slot(swsContext.datasets[[1]], "sessionId")
                        )
  

  tradeData = GetData(tradeKey, flags = !is.null(flags))
  
  #Trade has different names
  setnames(tradeData, "measuredElementTrade", "measuredElement")
  if (!is.null(flags)) {setnames(tradeData, "flagTrade", "flagObservationStatus")}
  setkey(tradeData, geographicAreaM49, measuredItemCPC, measuredElement, timePointYears)
  
  #Stick all the data together
  allData = rbind(productionData, tradeData)
  
  #If a flag is specified, subset by it
  if (!is.null(flags)) {
    if(negate){
      allData <- allData[!(flagObservationStatus %in% flags),]
    } else {
    allData <- allData[flagObservationStatus %in% flags,]
    }
    allData[, flagObservationStatus := NULL]
    }

allData =  merge(allData, feedCodeTable, all.y = TRUE, by = "measuredElement")

avail = dcast.data.table(allData, geographicAreaM49 + measuredItemCPC + timePointYears ~ variable, value.var = "Value")

#### Get rid of NAs
avail = avail[!apply(avail, 1, function(x) all(is.na(x))),]
avail[is.na(avail)] = 0

### 1.3 Calculate availability
avail[, feedAvailability := production + imports - exports - 
                        processed - food - seed - other - waste]

avail[feedAvailability < 0 , feedAvailability := feed]

#If official figure exists, use that instead
#Avail = within(Avail, { #Avail = ifelse(Avail <= 0, feed, Avail)
#                         Avail = ifelse(flag == " ", feed, Avail)})

avail <- avail[, .(geographicAreaM49, measuredItemCPC, timePointYears, feed, feedAvailability)]
setkey(avail, geographicAreaM49, measuredItemCPC, timePointYears)

avail

}



