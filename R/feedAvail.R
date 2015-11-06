
# This function provides feed availabilities. Using the equation:
# Production - Exports + Imports + Stockvariation = Food + Seed + Other Uses + Waste + Processed
# usage of single summands depends on user input. Data is downloaded from FAOSTAT sws



feedAvail = function(vars = c("food", "processed"), flags = FALSE) {

  usedvars = c("production", "feed", "imports", "exports", vars)
  
  feedCodeTable = data.table(measuredElement = c("5510", "5525", "5015", "5520", "5023", "5153", "5141", "5600", "5900"),
                             dataset = c("production", "production", "production", "production", "production", "production", "production", "trade", "trade"),
                              variable = c("production", "seed", "waste", "feed", "processed", "other", "food", "imports", "exports"))
  
  productionKey = DatasetKey(domain = "agriculture", dataset = "agriculture",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                         Dimension(name = "measuredItemCPC", keys = feedNutrients$measuredItemCPC),
                         Dimension(name = "measuredElement", keys = 
                                     feedCodeTable[variable %in% usedvars & dataset == "production", measuredElement]),
                         Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                       ),
                       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
                   )
  
  productionData = GetData(productionKey, flags = flags)
  setkey(productionData, geographicAreaM49, measuredItemCPC, measuredElement, timePointYears)
  
  #Remove this column so the rbind goes ahead
  if (flags) {productionData[, flagMethod := NULL]}
  
  tradeKey = DatasetKey(domain = "trade", dataset = "total_trade_CPC",
                        dimensions = list(
                          Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                          Dimension(name = "measuredItemCPC", keys = feedNutrients$measuredItemCPC),
                          Dimension(name = "measuredElementTrade", keys = 
                                      feedCodeTable[variable %in% usedvars & dataset == "trade", measuredElement]),
                          Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                        ),
                        sessionId =  slot(swsContext.datasets[[1]], "sessionId")
                        )
  

  tradeData = GetData(tradeKey, flags = flags)
  
  #Trade has different names
  setnames(tradeData, "measuredElementTrade", "measuredElement")
  if (flags) {setnames(tradeData, "flagTrade", "flagObservationStatus")}
  setkey(tradeData, geographicAreaM49, measuredItemCPC, measuredElement, timePointYears)
  
  #Stick all the data together
  allData = rbind(productionData, tradeData)

allData =  merge(allData, feedCodeTable, all.y = TRUE, by = "measuredElement")

terms = c("geographicAreaM49", "measuredItemCPC", "timePointYears")
if(flags){terms = c(terms, "flagObservationStatus")}
form = as.formula(paste(paste(terms, collapse = " + "), "~", "variable"))

avail = dcast.data.table(allData, form, value.var = "Value")

#### Get rid of NAs
avail = avail[!apply(avail, 1, function(x) all(is.na(x))),]
avail[is.na(avail)] = 0

### 1.3 Calculate availability
avail[, feedAvailability := production + imports - exports - 
                        processed - food - seed - other - waste]

avail[feedAvailability <= 0 , feedAvailability := feed]

#If official figure exists, use that instead
#Avail = within(Avail, { #Avail = ifelse(Avail <= 0, feed, Avail)
#                         Avail = ifelse(flag == " ", feed, Avail)})

avail[, c("geographicAreaM49", "measuredItemCPC", "timePointYears", if(flags) "flagObservationStatus" else NULL, "feed", "feedAvailability"), with = FALSE]

}



