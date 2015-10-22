pig_protein_factor <- function() {
 

data <- pig_energy_factor()
  
  
  data <- within(data, {
    #no conversion: Carcass.Wt comes in kg
    liveweight <- Carcass.Wt / .75

    metabolicweight <-liveweight^0.75
    
    
    protein <-(1.0522*15.51*((0.036*liveweight^0.75)+
                               (0.12*proteingain)))/874.1886 
  })
  
  data[, .(geographicAreaM49, timePointYears, protein)]

}
  