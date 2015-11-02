calculateAquaDemand = function() {
  
  areaKeys <- slot(swsContext.datasets[[1]]@dimensions$geographicAreaM49, "keys")
  timeKeys <- slot(swsContext.datasets[[1]]@dimensions$timePointYears, "keys")
  
  aquaDemand <- aquaDemandTable
  
  keys <- c("geographicAreaM49", "timePointYears")
  setkeyv(aquaDemand, keys)
  
  aquaDemand[geographicAreaM49 %in% areaKeys & timePointYears %in% timeKeys,]
  
}