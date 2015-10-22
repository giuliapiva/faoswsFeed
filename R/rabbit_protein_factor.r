rabbit_protein_factor <- function() {

  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "rabbit", fun = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "rabbit" & fun == "protein", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  
  within(data, {
    #Conversion from g into kg
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 0.79
    protein <- (8.4 * liveweight) / 874.1886
  })
  
}
