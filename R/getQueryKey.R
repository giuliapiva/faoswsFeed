#' Get Query Key
#' 
#' Get a data key from the query
#' 
#' @export

getQueryKey <- function(dimension){
  slot(swsContext.datasets[[1]]@dimensions[[dimension]], "keys")
}