getKids <- function(yieldcode, dimension="measuredElement", domain = "agriculture", dataset = "aproduction"){
  
  kids <- unlist(strsplit(GetCodeTree(domain, dataset, dimension, yieldcode)$children, ", "))
  
  if (is.null(kids)) {stop("code has no children")}
  
  GetCodeList(domain, dataset, dimension, kids)[,.(code, description)]
}