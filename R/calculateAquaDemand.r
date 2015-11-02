calculateAquaDemand = function() {
  
  areaKeys <- getQueryKey("geographicAreaM49")
  timeKeys <- getQueryKey("timePointYears")
  
  aquaDemand <- aquaDemandTable
  
  keys <- c("geographicAreaM49", "timePointYears")
  setkeyv(aquaDemand, keys)
  
  aquaDemand[geographicAreaM49 %in% areaKeys & timePointYears %in% timeKeys,]
  
}