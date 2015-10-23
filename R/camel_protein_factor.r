camel_protein_factor <- function(area, year) {

  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  
  rawData <-  getProdData(animal = "camel", fun = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "camel" & fun == "protein",.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  data <- within(data, {
    
    liveweight <- Carcass.Wt / 0.60
    metabolicweight <- liveweight ^ 0.75
    protein <- (0.96 * liveweight + 0.06 * liveweight ^ 0.75) / 874.1886
  })
  
  data[, .(geographicAreaM49, timePointYears, protein)]
  
}


