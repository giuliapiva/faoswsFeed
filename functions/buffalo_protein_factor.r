source('T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions/buffalo_energy_factor.r')


buffalo_protein_factor <- function(area, year) {
  
  #   if(length(year) < 2) stop('You have to select more than one year')


  data <- buffalo_energy_factor(area, year)
  


  
  data <- within(data, {
    metabolicweight <- liveweight^0.75
    re <- ((beefenergy * 35600)/4.184/365 - 0.077*metabolicweight)*0.6
    beefprotein <- (3.8 * (0.96 * liveweight)^0.75 + (weightgain *(268-(29.4*(re / weightgain)))))/874.1886
    beefprotein[weightgain = 0] <- (3.8 * (0.96 * liveweight[weightgain = 0])^0.75)/874.1886
    
    mp <- 5.17 * 6.25 * (milkpercow/365)
    me <- ((milkenergy * 35600) / 365)
    rdp <- ((milkenergy * 35600) / 365) * 7.8
    
    eup <- 37 * log10(liveweight) - 42.2
    
    
    dlp <- 0.11 * metabolicweight
    tp <- eup + dlp + mp
    
    
    
    udp <- ifelse(1.91 * tp - 6.25 * me < 0, 0, 1.91 * tp - 6.25 * me)
    
    
    milkprotein <- (rdp + udp) / 874.1886
    protein <- (milkprotein * Milk.Animals + beefprotein * Beef.Animals)/(Beef.Animals + Milk.Animals)
  })
  
  data[, c("area", "year", "protein") ]
  
}