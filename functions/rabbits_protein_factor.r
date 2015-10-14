source('functions/sws_query_2.r')

horses_protein_factor <- function(area, year) {

  
  vars <- list(heads = c(11, 1096), carcass = c(41, 1141))
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    Carcass.Wt <- Carcass.Wt / 1000
    liveweight <- Carcass.Wt / 10 / 0.79
    protein <- (8.4 * liveweight) / 874.1886
  })
  
}
