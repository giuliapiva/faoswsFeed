source('T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions/sws_query_2.r')


pig_energy_factor <- function(area, year) {
  #   
  #   if(length(year) > 1) {
  #     library(plyr)
  #     return(ldply(year, cattle_energy_factor, area = area))
  #   }
  
  vars <- list(heads = c(11, 1034), carcass = c(41, 1035))
  
  
  
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    liveweight <- Carcass.Wt / 10 / 0.75
    metabolicweight <- liveweight^0.75
    
    proteingain <- ((0.4767 + (0.02147 * liveweight) - (0.0002376 * liveweight^2) + 
                      (0.000000713 * liveweight^3)) * liveweight) / 2.55
    
    energy <- (((106 * metabolicweight + (proteingain * 10.6)) * 
                 (1+(0.0614 * liveweight * 0.0165))) * 0.0041868 * 365) / 35600
  })
  
  
}