buffalo_energy_factor <- function() {
  
  queryYear <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  #queryYear <- as.character(1990:2012)
  year <- c(queryYear, max(as.numeric((queryYear))) + 1)
  area <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  #area <- na.omit(fs2m49(as.character((1:299)[-22])))
  
  prodData <-  getProdData(animal = "buffalo", fun = "energy", area = area, year = year)
  tradeData <- getTradeData(animal = "buffalo", fun = "energy", area = area, year = year)


  rawData <- rbind(prodData, tradeData)
  
  namedData <- merge(rawData, codeTable[module == "buffalo" & fun == "energy",.(measuredItemCPC, measuredElement, variable)], 
                     by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)
  
  data <- dcast.data.table(namedData, geographicAreaM49 + timePointYears ~ variable, value.var = "Value")
  #remove any full NA rows
  data <- data[!apply(data, 1, function(x) all(is.na(x))),]
  # All missing values are to be treated as zero
  data[is.na(data)] <- 0
  
  data <- within(data, {
    
    # Define conversion between carcass and liveweight
    lw.constant <- .55
    # Beef animals are all animals which are not dairy
    Beef.Animals <- Stocks - Milk.Animals
    
    # Change in stocks
    Stocksnext <- c(Stocks[2:length(Stocks)], NA)
    
    #Live cattle are heavier than carcasses
    ## thousand factor is to convert tonnes to kilograms
    liveweight <- Carcass.Wt / lw.constant
    # If there's no carcass weight, estimate it
    liveweight[liveweight == 0] <- Meat.Production * 1000 / lw.constant / Slaughtered
    # If there's no regular meat production, use indigenous
    liveweight[liveweight == 0] <- Meat.Production.Ind * 1000 / lw.constant / Slaughtered.Ind
    #If there's still no liveweight, use live cattle. Meat.Production.Bio is live weight so doesn't need the .55 conversion factor.
    liveweight[liveweight == 0] <- Meat.Production.Bio * 1000 / Slaughtered.Bio
    
    milkpercow <- Milk.Production * 1000 / Milk.Animals
    metabolicweight <- liveweight ^ 0.75
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks - Stocks * 0.032) 
                    * liveweight) 
                   / Beef.Animals) / 365
    
    weightgain[weightgain < 0] <- 0 
    milkenergy <- (((365 * 0.077 * metabolicweight) + (0.74 * milkpercow)) * 4.184) / 0.6/ 35600
    beefenergy <- (365 * 4.184 * (0.077 * metabolicweight + (0.063 * (0.96 * liveweight) ^ 0.75 *                                                          
                                                               weightgain ^ 1.097)))/0.6/35600
    #alternatively
    #beefenergy[weightgain == 0] <- (365*(8.3 + (0.091 * Carcass.Wt[weightgain == 0] * 2)))/35600
    
    energy <- (milkenergy * Milk.Animals + beefenergy * Beef.Animals) / Stocks
  })
  
  data[timePointYears != max(as.numeric(timePointYears)), .(
    geographicAreaM49, timePointYears, energy, Milk.Animals, milkenergy,  Beef.Animals, beefenergy, liveweight, weightgain, milkpercow
  )]
  
  
}

