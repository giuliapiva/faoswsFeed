source('T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions/sws_query_2.r')

camels_protein_factor <- function(area, year) {

  
  vars <- list(heads = c(11, 1126), carcass = c(41, 1127))
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    
    liveweight <- Carcass.Wt / 10 / 0.60
    metabolicweight <- liveweight^0.75
    protein <- (0.96 * liveweight + 0.06 * liveweight^0.75) / 874.1886
  })
  
}


