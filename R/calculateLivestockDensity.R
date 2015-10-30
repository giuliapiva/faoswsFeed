calculateLivestockDensity = function(cattleCPC = "02111", addyear) {

  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
    
  if(!missing(addyear)){
    stopifnot(is.character(addyear))
    year <- unique(c(year, addyear))
    }
  
  ## Get data for permanent meadows and pastures
  key = DatasetKey(domain = "Land", dataset = "land",
                   dimensions = list(
                     Dimension(name = "geographicAreaM49", keys = slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")), 
                     Dimension(name = "itemLand", keys = "6655"), 
                     Dimension(name = "landElement", keys = "5110"),
                     Dimension(name = "timePointYears", keys = year) 
                     
                   )
  )
  
  
  
  permanentMeadows = GetData(key)
  
  setnames(permanentMeadows, "Value", "permanentMeadows")
  
  animalHeads = getAnimalStocks()
  
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
