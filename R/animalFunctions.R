#' Animal Functions
#' 
#' Functions mostly used in animal functions
#' 
#' @rdname animal-functions
#' 
#' @param animal character of animal as listed in \code{\link{codeTable}}.
#' @param fun character. Whether for protein or energy function
#' @param area character. M49 area codes
#' @param year character. Years

getProdData <- function(animal, fun, area, year){
  
  prodCodes <- codeTable[module == animal & fun == fun & table == "production", ]
  
  
  prodKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = area), #user input
                         Dimension(name = "measuredItemCPC", keys = unique(prodCodes$measuredItemCPC)),
                         Dimension(name = "measuredElement", keys = unique(prodCodes$measuredElement)),
                         Dimension(name = "timePointYears", keys = year) #user input
                       ),
                       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
  )
  GetData(prodKey, flags = FALSE)
  
}

getTradeData <- function(animal, fun, area, year) {
  
  tradeCodes <-  codeTable[module == animal & fun == fun & table == "trade",]
  
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
