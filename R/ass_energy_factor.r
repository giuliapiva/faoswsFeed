source('R/sws_query_2.r')

ass_energy_factor <- function(area, year) {

  
  vars <- list(heads = c(11, 1110), carcass = c(41, 1111))
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 10 / 0.55
    energy <- 2.7 * liveweight^0.75
  })
  
}
