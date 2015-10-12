source('functions/sws_query_2.r')


sheep_protein_factor <- function(area, year) {

  
  year[length(year) + 1] <- year[length(year)] + 1

  energy <- sheep_energy_factor(area, year)
  
  
  vars <- list(c(31, 977), c(91, 976), c(61, 976))
 
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
    Production <- ifelse(is.na(Production), 0, Production)
    milkpersheep <- Production * 1000 / Stocks
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
    
    
    mp <- 6.25 * 7.66 * (milkpersheep/365)
                      
  
   
    dlp <-0.11 * metabolicweight
    tp <- eup + dlp + mp + ig
                       
    
    
    
    udp <- ifelse(1.91 * tp - 6.25 * me < 0, 0, 1.91 * tp - 6.25 * me)
    
   
    protein <- (rdp + udp) / 874.1886
  })
  
  data[data$year != max(data$year), c("area", "year", "protein")]

}


