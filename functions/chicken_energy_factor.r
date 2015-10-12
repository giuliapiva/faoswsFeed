source('T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions/sws_query_2.r')

chicken_energy_factor <- function(area, year) {
  #   
  #   if(length(year) > 1) {
  #     library(plyr)
  #     return(ldply(year, cattle_energy_factor, area = area))
  #   }

  vars <- list(c(11, 1057), c(41, 1058), c(41, 1062), c(31, 1062) )
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    Stocks <- Stocks * 1000
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 10 / 0.68
    metabolicweight <- liveweight^0.75
    
    if(!exists("Laying"))  
    {energy <- (metabolicweight * 78.3 *2.5 * 365 * 0.0041868) / 35600
    
     }else{
      Laying <- Laying * 1000
      Yield <- Yield/ 1000
    layingenergy <- ((metabolicweight*120 + 2.07 * Yield)*0.0041868*365)/35600
    chickenenergy <- (metabolicweight * 78.3 *2 * 365 * 0.0041868) / 35600
    energy <- (Laying * layingenergy + (Stocks - Laying) * chickenenergy)/Stocks
    }
    energy[is.na(Laying)] <- (metabolicweight[is.na(Laying)] * 78.3 *2.5 * 365 * 0.0041868) / 35600
  })
  
  
}
