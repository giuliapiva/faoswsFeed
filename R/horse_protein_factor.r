horse_protein_factor <- function() {

  
  year <- getQueryKey("timePointYears")
  area <- getQueryKey("geographicAreaM49")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "horse", fun = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "horse" & fun == "protein", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  
  within(data, {
    
    # No Conversion: Carcass.Wt comes in kg
    liveweight <- Carcass.Wt / 0.62
    metabolicweight <- liveweight ^ 0.75
    protein <- (1.08 * liveweight + 0.08 * liveweight) / 874.1886
  })
  
}
