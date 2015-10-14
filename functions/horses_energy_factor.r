source('functions/sws_query_2.r')

horses_energy_factor <- function(area, year) {

  
  vars <- list(heads = c(11, 1096), carcass = c(41, 1097))
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    
    liveweight <- Carcass.Wt / 10 / 0.62
    metabolicweight <- liveweight^0.75
    energy <- ((0.55 * metabolicweight + 0.00495 * liveweight + 4.184) *365) / 35600
  })
  
}
