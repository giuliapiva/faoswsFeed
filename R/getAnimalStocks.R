getAnimalStocks <- function(stockKeys = c("5111", "5112"),  thousandHeads = "5112"){
  
  # Retrieve all animals
  animalKeys = stockCodes[, measuredItemCPC]
  
  key = DatasetKey(domain = "agriculture", dataset = "agriculture",
                   dimensions = list(
                     Dimension(name = "geographicAreaM49", keys = slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")), #user input
                     Dimension(name = "measuredItemCPC", keys = animalKeys),
                     Dimension(name = "measuredElement", keys = stockKeys),
                     Dimension(name = "timePointYears", keys = slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")) #user input
                     
                   )
  )
  #define this as a subset of the larger data
  animalHeads = GetData(key)
  
  setnames(animalHeads, "Value", "animalHeads")
  
  # Poultry and Rabbits are expressed in '000 heads, hence convert into heads
  animalHeads[measuredElement %in% thousandHeads , animalHeads := animalHeads * 1000]
  
  #Remove flags and measuredElement column
  animalHeads[, `:=`(measuredElement = NULL, flagObservationStatus = NULL, flagMethod = NULL)]
  setkey(animalHeads, geographicAreaM49, measuredItemCPC, timePointYears)
  
  animalHeads
}