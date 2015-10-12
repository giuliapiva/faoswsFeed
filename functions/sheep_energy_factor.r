source('T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions/sws_query_2.r')



sheep_energy_factor <- function(area, year) {
    
#    if(length(year) > 1) {
#      library(plyr)
 #      return(ldply(year, cattle_energy_factor, area = area))
  # }
  
  vars <- list(heads = c(11, 976), carcass = c(41, 977), milk = c(51, 982))
  

  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    Carcass.Wt <- Carcass.Wt / 10
    if(!exists("Production")) 
    {Production <- 0}
    Production[is.na(Production)] <- 0
    milkpersheep <- Production * 1000 / Stocks
    energy <- (365 * (1.8 + 0.1 * Carcass.Wt * 2) + 4.6 * milkpersheep) / 35600
  })
  
  
}
