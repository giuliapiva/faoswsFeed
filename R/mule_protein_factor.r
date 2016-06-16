mule_protein_factor <- function() {

  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  
  rawData <-  getProdData(animal = "mule", fun = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "mule" & fun == "protein", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  
  #If data is empty, return it
  if (nrow(data) == 0) {
    data[,protein := numeric(0)]
    return(data[ , .(geographicAreaM49, timePointYears, protein)])
  }

  data <- within(data, {
    #No Conversion: Carcass.Wt comes in kg
    liveweight <- Carcass.Wt / 0.63
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    protein <- (2.7 * metabolicweight) / 874.1886
  })
  
  data[, .(geographicAreaM49, timePointYears, protein)]
  
}
