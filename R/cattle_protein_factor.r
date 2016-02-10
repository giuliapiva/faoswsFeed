cattle_protein_factor <- function() {

  data <- cattle_energy_factor()
  
  if(nrow(data) == 0){
    setnames(data, "energy", "protein")
    return(data[, .(geographicAreaM49, timePointYears, protein)])
  }
  
  #data[is.na(data)] <- 0
  
  data <- within(data, {
    kleiberconstant <- 0.75
    metabolicweight <- liveweight ^ kleiberconstant
    re <- 0.0635 * (0.96 * liveweight) * 0.75 * (weightgain * 0.96) * 1.097
    beefprotein <- (3.8 * (0.96 * liveweight) ^ 0.75 + (weightgain * (268 - (29.4 * (re / weightgain))))) / 874.1886
    
    beefprotein[which(weightgain == 0)] <- (3.8 * (0.96 * liveweight[which(weightgain == 0)]) ^ 0.75) / 874.1886
    
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
  
  data[, .(geographicAreaM49, timePointYears, protein)]
  
}
