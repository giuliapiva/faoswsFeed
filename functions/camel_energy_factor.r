source('functions/sws_query_2.r')



camel_energy_factor <- function(area, year) {
    

  
  vars <- list(heads = c(11, 1126), carcass = c(41, 1127), milk = c(51, 1130))
  

  
  
  data <- sws_query(area = area, year = year, 
                    pairs = vars)
  
  within(data, {
    Carcass.Wt <- Carcass.Wt / 10
    if(!exists("Production")) 
    {Production <- 0}
    Production[is.na(Production)] <- 0
    milkpercamel <- Production * 1000 / Stocks
    energy <- (365 * (1.8 + 0.1 * Carcass.Wt * 2) + 4.6 * milkpercamel) / 35600
  })
  
  
}
