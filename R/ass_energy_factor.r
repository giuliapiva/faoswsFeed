ass_energy_factor <- function() {

  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "ass", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "ass" & fun == "energy", .(measuredItemCPC, measuredElement, variable)], 
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
    
    #No COnversion: Carcass.Wt comes in kg
    
    liveweight <- Carcass.Wt / 0.63
    energy <- (0.1548 *  liveweight) * 356 / 35600
  })
  
}
