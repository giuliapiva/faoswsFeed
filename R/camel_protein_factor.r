source('R/sws_query_2.r')

camel_protein_factor <- function(area, year) {

  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  
  rawData <-  getProdData(animal = "camel", fun = "protein", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "camel" & fun == "protein",.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  within(data, {
    
    liveweight <- Carcass.Wt / 0.60
    metabolicweight <- liveweight ^ 0.75
    protein <- (0.96 * liveweight + 0.06 * liveweight ^ 0.75) / 874.1886
  })
  
}


