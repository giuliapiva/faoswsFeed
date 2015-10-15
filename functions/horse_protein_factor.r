source('functions/sws_query_2.r')

horses_protein_factor <- function(area, year) {

  
  vars <- list(heads = c(11, 1096), carcass = c(41, 1097))
  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    
    liveweight <- Carcass.Wt / 10 / 0.62
    metabolicweight <- liveweight^0.75
    protein <- (1.08 * liveweight + 0.08 * liveweight) / 874.1886
  })
  
}
