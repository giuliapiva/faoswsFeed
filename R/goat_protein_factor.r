source('R/sws_query_2.r')


goat_protein_factor <- function(area, year) {
  
  
  year[length(year) + 1] <- year[length(year)] + 1

  energy <- goat_energy_factor(area, year)
  
  
  vars <- list(c(31, 1017), c(91, 1016), c(61, 1016))
 
  data <- sws_query(area = area, year = year, 
                     pairs = vars)
  
  data <- merge(energy, data, by=c('area', 'year'))
  
  data <- within(data, {
    
    Stocksnext <- c(Stocks[2:length(Stocks)], NA)
    Stocksnext[year==2011] <- 0
  
    
    if(!exists("Exports")) 
    {Exports <- 0}
    Exports[is.na(Exports)] <- 0
    
    if(!exists("Imports")) 
    {Imports <- 0}
    Imports[is.na(Imports)] <- 0
    
    liveweight <- Carcass.Wt / .43
    Production <- ifelse(!is.na(Production), Production, 0)
    milkpergoat <- Production * 1000 / Stocks
    metabolicweight <-liveweight^0.75
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks 
                        - Stocks * 0.044) * liveweight) / Stocks) / 365
    
    me <- ((energy * 35600) / 365)
    rdp <- ((energy * 35600) / 365) * 7.8
    
    eup <- 0.147 * liveweight + 3.375
                 
  
    
    ig <- ifelse(is.na(weightgain) | weightgain <= 0, 0, 
                          weightgain * (160.4 - 1.22 * liveweight + 0.0105 * 
                                          liveweight^2) 
                                  )
    
    
    mp <- 6.25 * 7.66 * (milkpergoat/365)
                      
  
   
    dlp <-0.11 * metabolicweight
    tp <- eup + dlp + mp + ig
                       
    
    
    
    udp <- ifelse(1.91 * tp - 6.25 * me < 0, 0, 1.91 * tp - 6.25 * me)
    
   
    protein <- (rdp + udp) / 874.1886
  })
  
  data[data$year != max(data$year), c("area", "year", "protein")]

}


