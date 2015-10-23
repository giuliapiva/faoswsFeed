camel_energy_factor <- function() {
  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  
  rawData <-  getProdData(animal = "camel", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "camel" & fun == "energy",.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  data <- within(data, {
    
    liveweight <- Carcass.Wt / 0.62
    metabolicweight <- liveweight ^ 0.75
    protein <- ((0.12 * metabolicweight * 4.184) * 365) / 35600
  })
  
  data
  
}
