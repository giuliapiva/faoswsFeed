chicken_energy_factor <- function() {
  #   
  #   if(length(year) > 1) {
  #     library(plyr)
  #     return(ldply(year, cattle_energy_factor, area = area))
  #   }

  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  
  rawData <-  getProdData(animal = "chicken", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "chicken" & fun == "energy",.(measuredItemCPC, measuredElement, variable)], 
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
  
  within(data, {
    Stocks <- Stocks * 1000
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 0.68
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    
    if (all(Laying == 0))  
    {energy <- (metabolicweight * 78.3 *2.5 * 365 * 0.0041868) / 35600
    
     } else {
      Laying <- Laying * 1000
      Yield <- Yield/ 1000
    layingenergy <- ((metabolicweight*120 + 2.07 * Yield)*0.0041868*365)/35600
    chickenenergy <- (metabolicweight * 78.3 *2 * 365 * 0.0041868) / 35600
    energy <- (Laying * layingenergy + (Stocks - Laying) * chickenenergy)/Stocks
    }
    energy[is.na(Laying)] <- (metabolicweight[is.na(Laying)] * 78.3 *2.5 * 365 * 0.0041868) / 35600
  })
  
  
}
