
# This function provides feed availabilities. Using the equation:
# Production - Exports + Imports + Stockvariation = Food + Seed + Other Uses + Waste + Processed
# usage of single summands depends on user input. Data is downloaded from FAOSTAT sws


feedAvail <- function(area, year, feeditem, vars = c("food", "processed")) {
    
  usedvars <- c("production", "feed", "imports", "exports", vars)
  
  feedCodeTable <- data.frame(measuredElement = c("5510", "5525", "5015", "5520", "5023", "5153", "5141", "5600", "5900"),
                              variable = c("production", "seed", "waste", "feed", "processed", "other", "food", "imports", "exports"))
  
  feedCodeTable <- feedCodeTable[variable %in% usedvars]
  
  productionKey = DatasetKey(domain = "agriculture", dataset = "agriculture",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                         Dimension(name = "measuredItemCPC", keys = feedNutrients$measuredItemCPC),
                         Dimension(name = "measuredElement", keys = c("5520", "5510", "5023", "5141", "5525", "5153", "5015")),
                         Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                       ),
                       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
                   )
  
  productionData = GetData(productionKey)#, flags=FALSE)
  
  tradeKey = DatasetKey(domain = "trade", dataset = "total_trade_CPC",
                        dimensions = list(
                          Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                          Dimension(name = "measuredItemCPC", keys = feedNutrients$measuredItemCPC),
                          Dimension(name = "measuredElementTrade", keys = c("5600", "5900")),
                          Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                        ),
                        sessionId =  slot(swsContext.datasets[[1]], "sessionId")
                        )
  
  tradeData <- GetData(tradeKey)#, flags=FALSE)
  setnames(tradeData, "measuredElementTrade", "measuredElement")
  
  allData <- rbind(productionData, tradeData)

allData <-  merge(allData, feedCodeTable, all.y = TRUE, by="measuredElement")

avail <- dcast.data.table(allData, geographicAreaM49 + measuredItemCPC + timePointYears ~ variable, value.var = "Value")
#### Get rid of NAs
    avail[is.na(avail)] <- 0

### 1.3 Calculate availability
avail[, feedAvailability := production + imports - exports - 
                        processed - food - seed - other - waste]
avail <- avail[,]

#checkfeed <- avail[, .(checkfeed = sum(feed)), by = .(geographicAreaM49, measuredItemCPC)]

#avail[avail <= 0 , avail := feed]

#If official figure exists, use that instead
#Avail <- within(Avail, { #Avail <- ifelse(Avail <= 0, feed, Avail)
#                         Avail <- ifelse(flag == " ", feed, Avail)})

avail[, .(geographicAreaM49, measuredItemCPC, timePointYears, feed, feedAvailability)]

}



