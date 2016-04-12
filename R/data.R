#' Code table for animal functions
#' 
#' Different codes and classifications for animals and products derived from
#' them (meat and milk).
#' 
#' @format a data.table with 54 rows and 8 columns
#' \itemize{
#'  \item module, Name of animal and animal function
#'  \item fun, Demand estimation type (energy or protein)
#'  \item table, Dataset from which data are to be retrieved
#'  \item measuredItemCPC, key, CPC code for animal species, meat or milk
#'  \item CPCDescription, Description of CPC element
#'  \item measuredElement, key, Code for element
#'  \item elementDescription, Name of element
#'  \item variable, Variable name used in animal requrirements functions
#'  }
#'  

"codeTable"

#' Coefficients for intensity factor calculations 
#' 
#' Coefficients which are used in the estimation equations of livestock
#' intensity of each animal group.
#' 
#' @format a data.table with 54 rows and 8 columns
#' \itemize{
#'  \item animalGroup, key, Animal group
#'  \item coefficient, Coefficient 
#' }
#' 

"animalCoefficients"

#' Demand table for aquaculture demand
#' 
#' Eventually, when fisheries is integrated into the new working system, this 
#' will be replaced with calculations from live data. For now it's a flat table.
#' It's based on 5 csvs which come from FAOSTAT aquaculture. 
#' (\url{http://www.fao.org/fishery/statistics/global-aquaculture-production/query/en})
#' 
#' @format a data.table with 4632 rows and 4 columns
#' \itemize{
#'  \item geographicAreaM49, key, M49 code for country
#'  \item timepointYears, key, Year
#'  \item aquaEnergyDemand, Energy demand for given country and year in MJ
#'  \item aquaProteinDemand, Protein demand for a given country and year in Mt?
#'  }
#' 

"aquaDemandTable"

#' Animal Species CPC codes and corresponding animal group 
#' 
#' The table links livestock species to animal groups which share the same 
#' characteristics in husbandry. Intensity factors are derived for each animal
#' group.
#' 
#' 
#' @format a data.table with 14 rows and 2 columns
#' \itemize{
#'  \item measuredItemCPC, key, CPC code for animal species
#'  \item animalGroup, Animal Group 
#'  }
#' 

"animalCPCGroup"

#' Nutrient table and classification of feedstuffs
#' 
#' For each item which has been allocated to feed in FAOSTAT in the past the
#' respective energy and protein contents as well as their classification as
#' feed item are provided. (\url{http://www.feedipedia.org/})
#' 
#' @format a data.table with 206 rows and 4 columns
#' \itemize{
#'  \item measuredItemCPC, key, CPC code of items 
#'  \item energyContent, key, Energy content in megajoules (MJ) per metric tonne
#'  (MT) dry matter
#'  \item proteinContent, Share of protein content per unit
#'  \item feedClassification, Extent to which the item is used as feed 
#'  }
#' 

"feedNutrients"

#' Survey data for intensification of animal husbandry
#' 
#' The table is derived from feed basket data on country level which has been
#' collected in the framework of the Global Livestock Environmental Assessment
#' Model (GLEAM) for the year of 2005 and is not publicly available. For more
#' information: (\url{http://www.fao.org/gleam/en/})
#' 
#' @format a data.table with 1848 rows and 4 columns
#' \itemize{
#'  \item geographicAreaM49, key, M49 code for country
#'  \item animalGroup, key, Animal Group
#'  \item timepointYears, key, Year
#'  \item measuredItemCPC, key, CPC code for items 
#'  \item intensity, Intensity factor for 2005
#' }
#' 

"s2005"

#' Animal names and Codes
#' 
#' The table links animal names included in the model to CPC codes. 
#' 
#' @format a data.table with 14 rows and 2 columns
#' \itemize{
#'  \item module, key, Name of animal species 
#'  \item measuredItemCPC, key, CPC code for animal species
#' }
#'

"stockCodes"

#' Labor productivity in agriculture (World Bank)
#' 
#' The indicator is part of the World Development Incicators (WDI), published by
#' the World Bank and describes value added per worker. Its based on a
#' downloaded flat csv. 
#' (\url{http://data.worldbank.org/indicator/EA.PRD.AGRI.KD})
#' 
#' @format a data.table with 7208 rows and 3 columns
#' \itemize{
#'  \item geographicAreaM49, key, M49 code for country
#'  \item timepointYears, key, Year
#'  \item productivity, Labour productivty in agriculture as value added per
#'  worker in constant 2005 US$
#' }
#' 

"wdi"

#' Land data taken from FAOSTAT regarding meadows and pastures
#' 
#' This data is a set taken from FAOSTAT 
#' (\url{http://faostat3.fao.org/download/R/RL/E}) with all countries for the 
#' years 1990-2013. It contains area (1000 ha, code: 5110) of meadows and
#' pastures (code: 6655). This dataset contains imputed values which were
#' calculated based on lending group in the World Bank country classification
#' table ('class.csv', 
#' \url{http://data.worldbank.org/about/country-and-lending-groups}). For the 
#' calculation, small islands are excluded from the imputation model. This is 
#' defined by 'FaostatAreaGroupLists.csv' 
#' (\url{http://faostat.fao.org/site/371/default.aspx}).
#' 
#' @format a data.table with 5160 rows and 3 columns
#' #' \itemize{
#'  \item geographicAreaM49, key, M49 code for country
#'  \item landElement, Area (1000 ha) which is code 5110
#'  \item itemLand, meadows and pastures item code 6655
#'  \item timepointYears, key, Year
#'  \item permanentMeadows, Value
#' }

"landData"
