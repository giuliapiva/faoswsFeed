rabbit_protein_factor <- function() {

  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  
  rawData <-  getProdData(animal = "rabbit", func = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "rabbit" & fun == "protein", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  
  #If data is empty, return it
  if (nrow(data) == 0) {
    data[,protein := numeric(0)]
    return(data[ , .(geographicAreaM49, timePointYears, protein)])
  }
  
  # All missing values are to be treated as zero
  #data[is.na(data)] <- 0
  
  data <- within(data, {
    #Conversion from g into kg
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 0.79
    protein <- (8.4 * liveweight) / 874.1886
  })
  
  data[, .(geographicAreaM49, timePointYears, protein)]
  
}
