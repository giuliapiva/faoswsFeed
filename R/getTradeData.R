getTradeData <- function(animal, fun, area, year) {
  
  load("data/codeTable.rda")
  
  tradeCodes <-  codeTable[module == animal & fun == fun & table == "trade",]
  
  tradeKey = DatasetKey(
    domain = "trade", dataset = "total_trade_CPC",
    dimensions = list(
      Dimension(name = "geographicAreaM49", keys = area), #user input
      Dimension(name = "measuredItemCPC", keys = unique(tradeCodes$measuredItemCPC)),
      Dimension(name = "measuredElementTrade", keys = unique(tradeCodes$measuredElement)),
      Dimension(name = "timePointYears", keys = year) #user input
    )
  )
  
  tradeData <- GetData(tradeKey)[,.(geographicAreaM49, measuredItemCPC, measuredElementTrade, timePointYears, Value)]
  setnames(tradeData, "measuredElementTrade", "measuredElement")
  
}
