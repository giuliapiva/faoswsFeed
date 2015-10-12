source('T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions/sws_query_2.r')


pig_protein_factor <- function(area, year) {
 
          
         
  year[length(year) + 1] <- year[length(year)] + 1

  data <- pig_energy_factor(area, year)
  
  
  data <- within(data, {
    
    liveweight <- Carcass.Wt / 10/ .75

    metabolicweight <-liveweight^0.75
    
    
    protein <-(1.0522*15.51*((0.036*liveweight^0.75)+
                               (0.12*proteingain)))/874.1886 
  })
  
  data[data$year != max(data$year), c("area", "year", "protein")]

}


