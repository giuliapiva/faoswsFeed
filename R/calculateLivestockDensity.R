#'
#'
#'@export

calculateLivestockDensity = function(cattleCPC = "02111", addyear) {

  
  year <- getQueryKey("timePointYears")
    
  if(!missing(addyear)){
    stopifnot(is.character(addyear))
    year <- unique(c(year, addyear))
    }
  
  meadowsKey <- "6655"
  elementKey <- "5110"
  
  ## Get data for permanent meadows and pastures
  key = DatasetKey(domain = "Land", dataset = "land",
                   dimensions = list(
                     Dimension(name = "geographicAreaM49", keys = getQueryKey("geographicAreaM49")), 
                     Dimension(name = "itemLand", keys = meadowsKey), 
                     Dimension(name = "landElement", keys = elementKey),
                     Dimension(name = "timePointYears", keys = year) 
                     
                   ),
                   sessionId =  slot(swsContext.datasets[[1]], "sessionId")
  )
  
  
  
  permanentMeadows = GetData(key)
  
  setnames(permanentMeadows, "Value", "permanentMeadows")
  
  animalHeads = getAnimalStocks(addyear = addyear)
  
  # merge with cattle head data
  livestockDensityData = merge(animalHeads[measuredItemCPC %in% cattleCPC,], permanentMeadows, 
                              by = c("geographicAreaM49", "timePointYears"))
  
  # calculate Livestock density
  livestockDensityData[, livestockDensity := animalHeads / permanentMeadows]
  #Add animal groups
  livestockDensityData[, animalGroup := animalCPCGroup[cattleCPC, animalGroup]]
 
  setkey(livestockDensityData, geographicAreaM49, timePointYears, animalGroup)
  livestockDensityData[, .(geographicAreaM49, timePointYears, animalGroup, livestockDensity)]
  
}
