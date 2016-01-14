cattle_energy_factor <- function() {
  
#  if(length(year) > 1) {
 #   library(plyr)
  #  return(ldply(year, cattle_energy_factor, area = area))
  #}

  queryYear <- getQueryKey("timePointYears")
  year <- c(queryYear, max(as.numeric((queryYear))) + 1)
  area <- getQueryKey("geographicAreaM49")
  
  prodData <-  getProdData(animal = "cattle", fun = "energy", area = area, year = year)
  tradeData <- getTradeData(animal = "cattle", fun = "energy", area = area, year = year)
  
  rawData <- rbind(prodData, tradeData)
  
  namedData <- merge(rawData, codeTable[module == "cattle" & fun == "energy",
                                        .(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]

  #If data is empty, return it
  if (nrow(data) == 0) {
    data[,energy := numeric(0)]
    return(data[timePointYears != max(as.numeric(timePointYears)),])
    }
  
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  data <- within(data, {
    Beef.Animals <- Stocks - Milk.Animals
    
    Stocksnext <- c(Stocks[2:length(Stocks)], NA)
    #Stocksnext[year==2011] <- 0
      
    liveweight <- Carcass.Wt / .55
    milkpercow <- Production * 1000 / Milk.Animals
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks 
                     - Stocks * 0.032) * liveweight) / Beef.Animals) / 365
    
    weightgain[weightgain < 0] <- 0 
    milkenergy <- (((365 * 0.077 * metabolicweight) + (0.74 * milkpercow)) * 4.184) / 0.6/ 35600
    beefenergy <- (365 * 4.184 * (0.077 * metabolicweight + (0.063 * (0.96*liveweight) ^ 0.75 *                                                          
                                                          weightgain ^ 1.097))) / 0.6 / 35600
    #alternatively
    #beefenergy[weightgain == 0] <- (365*(8.3 + (0.091 * Carcass.Wt[weightgain == 0] * 2)))/35600
    
    energy <- (milkenergy * Milk.Animals + beefenergy * Beef.Animals)/Stocks
  })
  
  data[timePointYears != max(as.numeric(timePointYears)),  
  .(geographicAreaM49, timePointYears, energy, Milk.Animals, milkenergy,  Beef.Animals, beefenergy, liveweight, weightgain, milkpercow
    )]
  
}
