camel_protein_factor <- function(area, year) {

  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  
  rawData <-  getProdData(animal = "camel", func = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "camel" & fun == "protein",.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  
  #If data is empty, return it
  if (nrow(data) == 0) {
    data[, protein := numeric(0)]
    return(data[, .(geographicAreaM49, timePointYears, protein)])
  }
  
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  data <- within(data, {
    
    liveweight <- Carcass.Wt / 0.60
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    protein <- (0.96 * liveweight + 0.06 * liveweight ^ 0.75) / 874.1886
  })
  
  data[, .(geographicAreaM49, timePointYears, protein)]
  
}


