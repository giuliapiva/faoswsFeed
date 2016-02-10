turkey_energy_factor <- function() {
  
  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  
  rawData <-  getProdData(animal = "turkey", func = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "turkey" & fun == "energy", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  
  #If data is empty, return it
  if (nrow(data) == 0) {
    data[,energy := numeric(0)]
    return(data)
  }
  
  # All missing values are to be treated as zero
  #data[is.na(data)] <- 0
  
  within(data, {
    
    #Conversion from g into kg
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 0.68
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    energy <- (metabolicweight * 78.3 * 3.4 * 365 * 0.0041868) / 35600
    
    })
  
  
}
