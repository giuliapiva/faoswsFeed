buffalo_energy_factor <- function() {
  
  queryYear <- getQueryKey("timePointYears")
  newYear <- as.character(max(as.numeric((queryYear))) + 1)
  year <- c(queryYear, newYear)
  area <- getQueryKey("geographicAreaM49")
  
  prodData <-  getProdData(animal = "buffalo", func = "energy", area = area, year = year)
  tradeData <- getTradeData(animal = "buffalo", func = "energy", area = area, year = year)

  rawData <- rbind(prodData, tradeData)
  
  namedData <- merge(rawData, codeTable[module == "buffalo" & fun == "energy",.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]

  nextyearStock <- data[,.(geographicAreaM49,
                           timePointYears = as.character(as.numeric(timePointYears) - 1),
                           Stocksnext = Stocks)]
  data <- merge(data, nextyearStock, all.x = TRUE)
    
  #If data is empty, return it
  if (nrow(data) == 0) {
    data[,energy := numeric(0)]
    return(data[!(timePointYears %in% newYear),])
  }
  
  # All missing values are to be treated as zero
  #data[is.na(data)] <- 0
  
  data <- within(data, {
   
    Milk.Production[is.na(Milk.Production)] <- 0
    Milk.Animals[is.na(Milk.Animals)] <- 0
    Imports[is.na(Imports)] <- 0
    Exports[is.na(Exports)] <- 0
    
    # Define conversion between carcass and liveweight
    lw.constant <- .55
    # Beef animals are all animals which are not dairy
    Beef.Animals <- Stocks - Milk.Animals
    
    #Live cattle are heavier than carcasses
    ## thousand factor is to convert tonnes to kilograms
    liveweight <- Carcass.Wt / lw.constant
    
    milkpercow <- Milk.Production * 1000 / Milk.Animals
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks - Stocks * 0.032) 
                    * liveweight) 
                   / Beef.Animals) / 365
    
    #`which` protects it from NAs
    weightgain[which(weightgain < 0)] <- 0 
    
    milkenergy <- (((365 * 0.077 * metabolicweight) + (0.74 * milkpercow)) * 4.184) / 0.6/ 35600
    beefenergy <- (365 * 4.184 * (0.077 * metabolicweight + (0.063 * (0.96 * liveweight) ^ 0.75 *                                                          
                                                               weightgain ^ 1.097)))/0.6/35600
    #alternatively
    #beefenergy[weightgain == 0] <- (365*(8.3 + (0.091 * Carcass.Wt[weightgain == 0] * 2)))/35600
    
    energy <- (milkenergy * Milk.Animals + beefenergy * Beef.Animals) / Stocks
    
  })
  
  data[!(timePointYears %in% newYear), .(
    geographicAreaM49, timePointYears, energy, Milk.Animals, milkenergy,  Beef.Animals, beefenergy, liveweight, weightgain, milkpercow
  )]
  
  
}

