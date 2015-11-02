getQueryKey <- function(dimension){
  slot(swsContext.datasets[[1]]@dimensions[[dimension]], "keys")
}