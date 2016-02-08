sheep_energy_factor <- function() {
    
#    if(length(year) > 1) {
#      library(plyr)
 #      return(ldply(year, cattle_energy_factor, area = area))
  # }
  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  
  rawData <-  getProdData(animal = "sheep", func = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "sheep" & fun == "energy",.(measuredItemCPC, measuredElement, variable)], 
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
  data[is.na(data)] <- 0
    

  data <- within(data, {
    #Convert production from tonnes to kilograms
    milkpersheep <- Production * 1000 / Stocks
    energy <- (365 * (1.8 + 0.1 * Carcass.Wt * 2) + 4.6 * milkpersheep) / 35600
  })
  
  data
}
