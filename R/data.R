#' Code table for animal functions
#' 
#' 

"codeTable"

#' Coefficients for intensity factor calculations for each animal group
#' 

"animalCoefficients"

#' Demand table for aquaculture demand
#' 
#' Eventually, when fisheries is integrated into the new working system, this
#' will be replaced with calculations from live data. For now it's a flat table.
#' It's based on n csvs which come from FAOSTAT aquaculture
#' (\url{http://www.fao.org/fishery/statistics/global-aquaculture-production/query/en})
#' 
#' @format a data.table with 4632 rows and 4 columns
#' \itemize{
#'  \item geographicAreaM49, key, M49 code for country
#'  \item timepointYears, key, Year
#'  \item aquaEnergyDemand, energy demand for given country and year in MJ
#'  \item aquaProteinDemand protein demand for a given country and year in Mt?
#'  }
#' 

"aquaDemandTable"