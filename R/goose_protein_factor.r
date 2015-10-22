goose_protein_factor <- function() {
  
  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "goose", fun = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "goose" & fun == "protein", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  
  data <- within(data, {
    
    #Conversion from g into kg
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 0.68
    metabolicweight <- liveweight^0.75
    protein <- (liveweight*0.07*0.18*1000) / 874.1886
  })
  
  data[ ,c("area", "year", "protein")] 
}
