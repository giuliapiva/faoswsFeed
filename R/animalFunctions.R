#' Animal Functions
#' 
#' Functions mostly used in animal functions. These are for retrieving data from
#' the SWS.
#' 
#' @rdname animal-functions
#'   
#' @param animal character of animal as listed in \code{\link{codeTable}}.
#' @param func character. Whether for protein or energy function - All of the
#'   form {animal}_{energy|protein}_factor
#' @param area character. M49 area codes
#' @param year character. Years

getProdData <- function(animal, func, area, year){
  
  prodCodes <- codeTable[module == animal & fun == func & table == "production", ]
  
  
  prodKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = area), #user input
                         Dimension(name = "measuredItemCPC", keys = unique(prodCodes$measuredItemCPC)),
                         Dimension(name = "measuredElement", keys = unique(prodCodes$measuredElement)),
                         Dimension(name = "timePointYears", keys = year) #user input
                       ),
                       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
  )
  
  prodData <- removeMissingFlags(GetData(prodKey, flags = TRUE))
  prodData[,`:=`(flagObservationStatus = NULL,
                    flagMethod = NULL)]
  prodData
}

getTradeData <- function(animal, func, area, year) {
  
  tradeCodes <-  codeTable[module == animal & fun == func & table == "trade",]
  
  tradeKey = DatasetKey(
    domain = "trade", dataset = "total_trade_CPC",
    dimensions = list(
      Dimension(name = "geographicAreaM49", keys = area), #user input
      Dimension(name = "measuredItemCPC", keys = unique(tradeCodes$measuredItemCPC)),
      Dimension(name = "measuredElementTrade", keys = unique(tradeCodes$measuredElement)),
      Dimension(name = "timePointYears", keys = year) #user input
    ),
    sessionId =  slot(swsContext.datasets[[1]], "sessionId")
  )
  
  tradeData <- GetData(tradeKey, flags = FALSE)
  setnames(tradeData, "measuredElementTrade", "measuredElement")
  
}

removeMissingFlags <- function(data, delete = FALSE, observationFlag = "flagObservationStatus", 
                               methodFlag = "flagMethod", missingCode = "M"){
  workingdata <- copy(data)
  workingdata <- workingdata[get(observationFlag) != missingCode,]
  
  if(delete){
    # There may not be a method flag
    to_delete <- setdiff(names(workingdata), c(observationFlag, methodFlag))
    workingdata <- workingdata[,to_delete, with = FALSE]
  }
  
  workingdata
  
  
}
