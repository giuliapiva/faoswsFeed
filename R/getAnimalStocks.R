getAnimalStocks <- function(stockKeys = c("5111", "5112"),  thousandHeads = "5112", addyear){
  
  year <- getQueryKey("timePointYears")
  
  #Add year is to add year 2005 for the livestock density calculation
  if(!missing(addyear)){
    stopifnot(is.character(addyear))
    year <- unique(c(year, addyear))
  }
  
  # Retrieve all animals
  animalKeys = stockCodes[, measuredItemCPC]
  
  key = DatasetKey(domain = "agriculture", dataset = "aproduction",
                   dimensions = list(
                     Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")), #user input
                     Dimension(name = "measuredItemCPC", keys = animalKeys),
                     Dimension(name = "measuredElement", keys = stockKeys),
                     Dimension(name = "timePointYears", keys = year) #user input
                   ),
                   sessionId =  slot(swsContext.datasets[[1]], "sessionId")
  )
  #define this as a subset of the larger data
  animalHeads = GetData(key, flags = TRUE)
  
  # Remove missing flags
  animalHeads <- removeMissingFlags(animalHeads)
  animalHeads[,`:=`(flagObservationStatus = NULL,
                    flagMethod = NULL)]
  
  setnames(animalHeads, "Value", "animalHeads")
  
  # Poultry and Rabbits are expressed in '000 heads, hence convert into heads
  animalHeads[measuredElement %in% thousandHeads , animalHeads := animalHeads * 1000]
  
  #measuredElement column
  animalHeads[, measuredElement := NULL]
  setkey(animalHeads, geographicAreaM49, measuredItemCPC, timePointYears)
  
  animalHeads
}