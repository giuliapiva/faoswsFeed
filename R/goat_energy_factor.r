goat_energy_factor <- function() {
  #   
  #   if(length(year) > 1) {
  #     library(plyr)
  #     return(ldply(year, cattle_energy_factor, area = area))
  #   }
  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  
  rawData <-  getProdData(animal = "goat", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "goat" & fun == "energy",.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  within(data, {
    milkpergoat <- Production * 1000 / Stocks
    energy <- (365 * (1.8 + 0.1 * Carcass.Wt * 2) + 4.6 * milkpergoat) / 35600
  })
  
  
}
