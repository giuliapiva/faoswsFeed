goose_protein_factor <- function() {
  
  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "goose", fun = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "goose" & fun == "protein", .(measuredItemCPC, measuredElement, variable)], 
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
  data[is.na(data)] <- 0
  
  
  data <- within(data, {
    
    #Conversion from g into kg
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 0.68
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    protein <- (liveweight * 0.07 * 0.18 * 1000) / 874.1886
  })
  
  data[ ,.(geographicAreaM49, timePointYears, protein)] 
}
