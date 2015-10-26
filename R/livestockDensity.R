livestockDensity = function() {
  

  ## Get data for permanent meadows and pastures
  key = DatasetKey(domain = "land", dataset = "land",
                   dimensions = list(
                     Dimension(name = "geographicAreaM49", keys = slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")), 
                     Dimension(name = "measuredItemCPC", keys = "6655"), 
                     Dimension(name = "measuredElement", keys = "5110"),
                     Dimension(name = "timePointYears", keys = slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")) 
                     
                   )
  )
  
  
  
  permanentMeadows = GetData(key)
  
  setnames(animalHeads, "Value", "permanentMeadows")
  
  # merge with cattle head data
  livestockDensityData = merge(animalHeads[measuredItemCPC == "2111",], permanentMeadows, 
                              by=c("geographicAreaM49", "timePointYears"))
  
  # calculate Livestock density
  livestockDensityData[, := livestockDensity = animalHeads / permanentMeadows]
 
  livestockDensityData[, "geographicAreaM49", "timePointYears", "livestockDensity"]
  
}
