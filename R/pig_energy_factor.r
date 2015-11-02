pig_energy_factor <- function() {
  #   
  #   if(length(year) > 1) {
  #     library(plyr)
  #     return(ldply(year, cattle_energy_factor, area = area))
  #   }
  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  
  rawData <-  getProdData(animal = "pig", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "pig" & fun == "energy", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0

  
  data <- within(data, {
    # no conversion: data comes in kg
    liveweight <- Carcass.Wt / 0.75
    metabolicweight <- liveweight ^ 0.75
    
    proteingain <- ((0.4767 + (0.02147 * liveweight) - (0.0002376 * liveweight ^ 2) + 
                       (0.000000713 * liveweight ^ 3)) * liveweight) / 2.55
    
    energy <- (((106 * metabolicweight + (proteingain * 10.6)) * 
                  (1 + (0.0614 * liveweight * 0.0165))) * 0.0041868 * 365) / 35600
  })
  
  data
  
}
