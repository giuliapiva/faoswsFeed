rabbit_energy_factor <- function() {

  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "rabbit", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "rabbit" & fun == "energy", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  
  within(data, {
    #Conversion from g into kg
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 0.55
    energy <- (200 * 0.0041868 * liveweight * 365) / 35600
  })
  
}
