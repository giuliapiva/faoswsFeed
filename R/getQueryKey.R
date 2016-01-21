#' Get Query Key
#' 
#' Get a data key from the query
#' 
#' @param dimension character. Name of dimension you wish to extract from
#'   DatasetKey
#' @param datasetkey Datasetkey. DatasetKey from which you wish to extract
#'   dimensions
#'   
#' @export

getQueryKey <- function(dimension, datasetkey = swsContext.datasets[[1]]){
  slot(datasetkey@dimensions[[dimension]], "keys")
}