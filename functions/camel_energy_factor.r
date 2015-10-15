source('functions/sws_query_2.r')

camel_energy_factor <- function(area, year) {

  
  vars <- list(heads = c(11, 1126), carcass = c(41, 1127))
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    
    liveweight <- Carcass.Wt / 10 / 0.62
    metabolicweight <- liveweight^0.75
    protein <- ((0.12 * metabolicweight * 4.184) *365) / 35600
  })
  
}
