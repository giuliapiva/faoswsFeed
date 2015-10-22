
pig_protein_factor <- function(area, year) {
 

data <- pig_energy_factor()
  
  
  data <- within(data, {
    
    liveweight <- Carcass.Wt / 10/ .75

    metabolicweight <-liveweight^0.75
    
    
    protein <-(1.0522*15.51*((0.036*liveweight^0.75)+
                               (0.12*proteingain)))/874.1886 
  })
  
  data[data$year != max(data$year), c("area", "year", "protein")]

}


