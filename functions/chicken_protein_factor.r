source('T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions/sws_query_2.r')

chicken_protein_factor <- function(area, year) {
  #   
  #   if(length(year) > 1) {
  #     library(plyr)
  #     return(ldply(year, cattle_energy_factor, area = area))
  #   }
  
  vars <- list(heads = c(11, 1057), carcass = c(41, 1058))

  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  data <- within(data, {
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 10 / 0.68
    metabolicweight <- liveweight^0.75
    protein <- (liveweight*0.06*0.19*1000)/874.1886
  })
  
  data[ ,c("area", "year", "protein")]
}
