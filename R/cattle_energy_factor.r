source('R/sws_query_2.r')


cattle_energy_factor <- function() {
  
#  if(length(year) > 1) {
 #   library(plyr)
  #  return(ldply(year, cattle_energy_factor, area = area))
  #}

  queryYear <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  year <- c(queryYear, max(as.numeric((queryYear))) + 1)
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  
  prodData <-  getProdData(animal = "buffalo", fun = "energy", area = area, year = year)
  tradeData <- getTradeData(animal = "buffalo", fun = "energy", area = area, year = year)
  
  rawData <- rbind(prodData, tradeData)
  
  namedData <- merge(rawData, codeTable[,.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  data <- within(data, {
    Carcass.Wt <- Carcass.Wt / 10
    Beef.Animals <- Stocks - Milk.Animals
    if(!exists("Production")) 
    {Production <- 0}
    Production[is.na(Production)] <- 0
    Stocksnext <- c(Stocks[2:length(Stocks)], NA)
    #Stocksnext[year==2011] <- 0
      
    if(!exists("Exports")) 
    {Exports <- 0}
    Exports[is.na(Exports)] <- 0
    
    if(!exists("Imports")) 
    {Imports <- 0}
    Imports[is.na(Imports)] <- 0
    
    liveweight <- Carcass.Wt / .55
    milkpercow <- Production * 1000 / Milk.Animals
    metabolicweight <- liveweight^0.75
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks 
                     - Stocks * 0.032) * liveweight) / Beef.Animals) / 365
    
    weightgain[weightgain < 0] <- 0 
    milkenergy <- (((365 * 0.077 * metabolicweight) + ( 0.74 * milkpercow))*4.184) / 0.6/ 35600
    beefenergy <- (365 * 4.184 * (0.077 * metabolicweight +(0.063 * (0.96*liveweight)^0.75 *                                                          
                                                          weightgain^1.097)))/0.6/35600
    #alternatively
    #beefenergy[weightgain == 0] <- (365*(8.3 + (0.091 * Carcass.Wt[weightgain == 0] * 2)))/35600
    
    energy <- (milkenergy * Milk.Animals + beefenergy * Beef.Animals)/Stocks
  })
  
  data[data$year != max(data$year), c("area", "year", "energy","Milk.Animals", "milkenergy", "Beef.Animals", 
                                      "beefenergy", "liveweight", "weightgain", "milkpercow") ]
  
}
