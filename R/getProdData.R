getProdData <- function(animal, fun, area, year){
  
  #load("data/codeTable.rda")
  
  prodCodes <- codeTable[module == animal & fun == fun & table == "production", ]
  
  
  prodKey = DatasetKey(domain = "agriculture", dataset = "agriculture",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = area), #user input
                         Dimension(name = "measuredItemCPC", keys = unique(prodCodes$measuredItemCPC)),
                         Dimension(name = "measuredElement", keys = unique(prodCodes$measuredElement)),
                         Dimension(name = "timePointYears", keys = year) #user input
                       )
  )
  GetData(prodKey)[,.(geographicAreaM49, measuredItemCPC, measuredElement, timePointYears, Value)]
  
}
