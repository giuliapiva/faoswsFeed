#' Calculate Feed Availability
#' 
#' This function provides feed availabilities. Using the equation: Production -
#' Exports + Imports + Stockvariation = Food + Seed + Other Uses + Waste +
#' Processed usage of single summands depends on user input. Data is downloaded
#' from FAOSTAT sws
#' 
#' @param vars character. Variables that should be included. one of
#'   "production", "seed", "waste", "feed", "processed", "other", "food",
#'   "imports", "exports"
#' @param measuredItem character. Vector of CPC values by which the results
#'   should be subsetted
#' @param flags character. If not NULL, subset keep only data with these flags
#' @param negate character. When flags is not NULL, keep only data without these
#'   flags
#'   
#' @export

feedAvail = function(vars, measuredItem = feedNutrients$measuredItemCPC, amperflagskeys = NULL, negate = FALSE) {

  feedCodeTable = data.table(measuredElement = c("5510", "5023", "5141", "5600", "5900"),
                             dataset = c("production", "production", "production", "trade", "trade"),
                              variable = c("production", "processed", "food", "imports", "exports"))
  
  productionKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
                         Dimension(name = "measuredItemCPC", keys = measuredItem),
                         Dimension(name = "measuredElement", keys = 
                                     feedCodeTable[variable %in% vars & dataset == "production", measuredElement]),
                         Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
                       ),
                       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
                   )
  
  productionData = GetData(productionKey, flags = FALSE)
  
  if(nrow(feedCodeTable[variable %in% vars & dataset == "trade"]) != 0) {
    #Remove this column so the rbind goes ahead
    
    tradeKey = DatasetKey(
      domain = "trade", dataset = "total_trade_CPC",
      dimensions = list(
        Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")),
        Dimension(name = "measuredItemCPC", keys = measuredItem),
        Dimension(name = "measuredElementTrade", keys =
                    feedCodeTable[variable %in% vars &
                                    dataset == "trade", measuredElement]),
        Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
      ),
      sessionId =  slot(swsContext.datasets[[1]], "sessionId")
    )
    
    
    tradeData = GetData(tradeKey, flags = FALSE)
    
    #Trade has different names
    setnames(tradeData, "measuredElementTrade", "measuredElement")
  
    #Stick all the data together
    allData = rbind(productionData, tradeData)
    
  } else {
    allData <- productionData
  }

  setkey(allData, geographicAreaM49, measuredItemCPC, measuredElement, timePointYears)
  
  #If a flag is specified, subset by it
  

allData =  merge(allData, feedCodeTable, all.y = TRUE, by = "measuredElement")

avail = dcast.data.table(allData, geographicAreaM49 + measuredItemCPC + timePointYears ~ variable, value.var = "Value")

# If we want to include or exclude based on flags
# Replace this with a data.table anti join
if(!is.null(amperflagskeys)){
  amperkeys <- apply(avail[,.(geographicAreaM49, measuredItemCPC, timePointYears)], 1, paste, collapse="&")
  if(negate){
    # Include only flags which don't correspond with feed official data
    avail <- avail[!(amperkeys %in% amperflagskeys),]
  } else {
    # Include only flags which correspond with feed official data
    avail <- avail[amperkeys %in% amperflagskeys,]
  }
}

#### Get rid of NAs
# Get rid of rows of NAs (where a variable does not exist)
avail = avail[!apply(avail, 1, function(x) all(is.na(x))),]
avail[is.na(avail)] = 0

### 1.3 Calculate availability
avail[, feedAvailability := production + imports - exports - 
                        processed - food]

#If official figure exists, use that instead
#Avail = within(Avail, { #Avail = ifelse(Avail <= 0, feed, Avail)
#                         Avail = ifelse(flag == " ", feed, Avail)})

avail <- avail[, .(geographicAreaM49, measuredItemCPC, timePointYears, feedAvailability)]
setkey(avail, geographicAreaM49, measuredItemCPC, timePointYears)

avail

}



