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
#' @param officialData data.table. A table containing all official feed values.
#' @param negate character. When flags is not NULL, keep only data without these
#'   flags
#'   
#' @export

feedAvail = function(vars, measuredItem = feedNutrients$measuredItemCPC, officialData = NULL, negate = FALSE) {

  feedCodeTable = data.table(measuredElement = c("5510", "5023", "5141", "5610", "5910"),
                             measuredElementFS = c(NA, NA, NA, "61", "91"),
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
  
  productionData = removeMissingFlags(GetData(productionKey, flags = TRUE))
  productionData[,`:=`(flagObservationStatus = NULL,
                 flagMethod = NULL)]
  
  if(nrow(feedCodeTable[variable %in% vars & dataset == "trade"]) != 0) {
    #Remove this column so the rbind goes ahead
    
    tradeKey = DatasetKey(
      domain = "faostat_one", dataset = "FS1_SUA",
      dimensions = list(
        #user input except curacao,  saint martin and former germany
        Dimension(name = "geographicAreaFS", keys = setdiff(m492fs(getQueryKey("geographicAreaM49")), c("279", "534", "280"))),
        Dimension(name = "measuredItemFS", keys = sub("^0+", "", cpc2fcl(unique(measuredItem, version = "latest")))),
        Dimension(name = "measuredElementFS", keys =
                    feedCodeTable[variable %in% vars &
                                    dataset == "trade", measuredElementFS]),
        Dimension(name = "timePointYears", keys = getQueryKey("timePointYears"))
      ),
      sessionId =  slot(swsContext.datasets[[1]], "sessionId")
    )
    
    
    tradeData = GetData(tradeKey, flags = FALSE)
    
    tradeData[, `:=`(geographicAreaFS = fs2m49(geographicAreaFS),
                     measuredItemFS = fcl2cpc(sprintf("%04d", as.numeric(measuredItemFS))))]
    tradeData <- tradeData[unique(feedCodeTable[!is.na(measuredElementFS), .(measuredElementFS, measuredElement)]), on = "measuredElementFS"]
    tradeData[, measuredElementFS := NULL]
    
    setnames(tradeData, c("geographicAreaFS", "measuredItemFS"),
             c("geographicAreaM49", "measuredItemCPC"))
    
    setcolorder(tradeData, c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears", "Value"))
    
    #Trade has different names
    #setnames(tradeData, "measuredElementTrade", "measuredElement")
  
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
if (!is.null(officialData)) {
  if (negate) {
    # Include only flags which don't correspond with feed official data
    avail <- avail[!officialData, on=c("geographicAreaM49", "measuredItemCPC", "timePointYears")]
  } else {
    # Include only flags which correspond with feed official data
    avail <- avail[officialData, on=c("geographicAreaM49", "measuredItemCPC", "timePointYears")]
  }
}

#### Get rid of NAs
# Get rid of rows of NAs (where a variable does not exist)
avail = avail[!apply(avail, 1, function(x) all(is.na(x))),]
avail[is.na(avail)] = 0

### 1.3 Calculate availability
avail[, feedAvailability := production + imports - exports - 
                        processed - food]

# Replace all < 0  with 0

avail[feedAvailability < 0, feedAvailability := 0]


#If official figure exists, use that instead
#Avail = within(Avail, { #Avail = ifelse(Avail <= 0, feed, Avail)
#                         Avail = ifelse(flag == " ", feed, Avail)})

avail <- avail[, .(geographicAreaM49, measuredItemCPC, timePointYears, feedAvailability)]
setkey(avail, geographicAreaM49, measuredItemCPC, timePointYears)

avail

}



