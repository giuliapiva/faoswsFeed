goat_protein_factor <- function(area, year) {
  
  
  queryYear <- getQueryKey("timePointYears")
  newYear <- as.character(max(as.numeric((queryYear))) + 1)
  year <- c(queryYear, newYear)
  area <- getQueryKey("geographicAreaM49")
  
  
  prodData <-  getProdData(animal = "goat", func = "protein", area = area, year = year)
  tradeData <- getTradeData(animal = "goat", func = "protein", area = area, year = year)
  
  rawData <- rbind(prodData, tradeData)
  
  namedData <- merge(rawData, codeTable[module == "goat" & fun == "protein",.(measuredItemCPC, measuredElement, variable)], 
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
    data[,protein := numeric(0)]
    return(data[!(timePointYears %in% newYear), .(geographicAreaM49, timePointYears, protein)])
  }
  
  # All missing values are to be treated as zero
  #data[is.na(data)] <- 0
  
  data <- within(data, {
    
    Imports[is.na(Imports)] <- 0
    Exports[is.na(Exports)] <- 0
    Production[is.na(Production)] <- 0
    
    milkpergoat <- Production * 1000 / Stocks
    energy <- (365 * (1.8 + 0.1 * Carcass.Wt * 2) + 4.6 * milkpergoat) / 35600
    
    liveweight <- Carcass.Wt / .43
    
    milkpergoat <- Production * 1000 / Stocks
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks 
                        - Stocks * 0.044) * liveweight) / Stocks) / 365
    
    me <- ((energy * 35600) / 365)
    rdp <- ((energy * 35600) / 365) * 7.8
    
    eup <- 0.147 * liveweight + 3.375
                 
    ig <- ifelse(is.na(weightgain) | weightgain <= 0, 0, 
                          weightgain * (160.4 - 1.22 * liveweight + 0.0105 * 
                                          liveweight ^ 2) 
                                  )
    
    
    mp <- 6.25 * 7.66 * (milkpergoat/365)
                      
  
   
    dlp <- 0.11 * metabolicweight
    tp <- eup + dlp + mp + ig
                       
    
    
    
    udp <- ifelse(1.91 * tp - 6.25 * me < 0, 0, 1.91 * tp - 6.25 * me)
    
   
    protein <- (rdp + udp) / 874.1886
  })
  
  data[!(timePointYears %in% newYear), .(geographicAreaM49, timePointYears, protein)]

}


