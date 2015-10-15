source('functions/sws_query_2.r')

rabbits_energy_factor <- function(area, year) {

  
  vars <- list(heads = c(11, 1140), carcass = c(41, 1141))
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 10 / 0.55
    energy <- (200 * 0.0041868 * liveweight * 365) / 35600
  })
  
}
