#source('R/sws_query_2.r')


pig_energy_factor <- function() {
  #   
  #   if(length(year) > 1) {
  #     library(plyr)
  #     return(ldply(year, cattle_energy_factor, area = area))
  #   }
  
  year <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  rawData <-  getProdData(animal = "pig", fun = "energy", area = area, year = year)
  
  namedData <- merge(rawData, codeTable[module == "pig" & fun == "energy", .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  
  
  
  #vars <- list(heads = c(11, 1034), carcass = c(41, 1035))
  
  
  
  
  
  #data <- sws_query(area = area, year = year, 
  #                 pairs = vars)
  
  data <- within(data, {
    liveweight <- Carcass.Wt / 10 / 0.75
    metabolicweight <- liveweight^0.75
    
    proteingain <- ((0.4767 + (0.02147 * liveweight) - (0.0002376 * liveweight^2) + 
                       (0.000000713 * liveweight^3)) * liveweight) / 2.55
    
    energy <- (((106 * metabolicweight + (proteingain * 10.6)) * 
                  (1+(0.0614 * liveweight * 0.0165))) * 0.0041868 * 365) / 35600
  })
  
  data
  
}
