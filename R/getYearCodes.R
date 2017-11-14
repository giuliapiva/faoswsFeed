#' Get years corresponding to the source coverage
#'
#' In November 2017, a request was made to use old trade data for 2010-2013 and
#' new trade data for 2014-2015... On the face of it, this is the wrong way to
#' go about the problem. It's messy and bad practice. A parameter should be made
#' to switch between the two and it should be run as two separate modules.
#' BUT... The years are not all entirely independent. The gap between 2013-2014
#' is tricky because there are animal functions that need to look at the year
#' after the the current one. As a result, it's best to make a function able to
#' combine these cases.
#' 
#' @param source character. 
#' @param allyears character.
#' 
#' @return character. Years that belong to both the source set and allyears, if provided
#' 
#' @export

getYearCodes <- function(source = c("faostat", "trade_module"), allyears = NULL){
  source <- match.arg(source)
  years <- switch(source,
                  faostat = as.character(2010:2013),
                  trade_module = as.character(2014:2015))
  
  if(!is.null(allyears)){
    years <- intersect(allyears, c(years, "2016")) # include 2016 if it's in allyears, otherwise, ignore it
  }
  
  return(years)
}
