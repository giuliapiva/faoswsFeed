calculateAquaDemand = function() {
  
  areaKeys <- getQueryKey("geographicAreaM49")
  timeKeys <- getQueryKey("timePointYears")
  
  aquaDemand <- aquaDemandTable
  
  aquaDemand[geographicAreaM49 %in% areaKeys & timePointYears %in% timeKeys,]
  
}
