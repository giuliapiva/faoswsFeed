source('functions/sws_query_2.r')


cattle_energy_factor <- function(area, year) {
  
#  if(length(year) > 1) {
 #   library(plyr)
  #  return(ldply(year, cattle_energy_factor, area = area))
  #}

  year[length(year) + 1] <- year[length(year)] + 1
  vars <- list(c(11, 866), c(41, 867), c(51, 882), c(31, 882), c(31, 867), 
               c(91, 866), c(61, 866))
  
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  data <- within(data, {
    Carcass.Wt <- Carcass.Wt / 10
    Beef.Animals <- Stocks - Milk.Animals
    if(!exists("Production")) 
    {Production <- 0}
    Production[is.na(Production)] <- 0
    Stocksnext <- c(Stocks[2:length(Stocks)], NA)
    #Stocksnext[year==2011] <- 0
      
    if(!exists("Exports")) 
    {Exports <- 0}
    Exports[is.na(Exports)] <- 0
    
    if(!exists("Imports")) 
    {Imports <- 0}
    Imports[is.na(Imports)] <- 0
    
    liveweight <- Carcass.Wt / .55
    milkpercow <- Production * 1000 / Milk.Animals
    metabolicweight <- liveweight^0.75
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks 
                     - Stocks * 0.032) * liveweight) / Beef.Animals) / 365
    
    weightgain[weightgain < 0] <- 0 
    milkenergy <- (((365 * 0.077 * metabolicweight) + ( 0.74 * milkpercow))*4.184) / 0.6/ 35600
    beefenergy <- (365 * 4.184 * (0.077 * metabolicweight +(0.063 * (0.96*liveweight)^0.75 *                                                          
                                                          weightgain^1.097)))/0.6/35600
    #alternatively
    #beefenergy[weightgain == 0] <- (365*(8.3 + (0.091 * Carcass.Wt[weightgain == 0] * 2)))/35600
    
    energy <- (milkenergy * Milk.Animals + beefenergy * Beef.Animals)/Stocks
  })
  
  data[data$year != max(data$year), c("area", "year", "energy","Milk.Animals", "milkenergy", "Beef.Animals", 
                                      "beefenergy", "liveweight", "weightgain", "milkpercow") ]
  
}
