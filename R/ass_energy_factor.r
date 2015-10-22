ass_energy_factor <- function() {

  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "ass", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "ass" & fun == "energy", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  within(data, {
    
    #No COnversion: Carcass.Wt comes in kg
    
    liveweight <- Carcass.Wt / 0.63
    energy <- ( 0.1548 *  liveweight) * 356 / 35600
  })
  
}
