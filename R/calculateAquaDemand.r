calculateAquaDemand = function() {
  
  aquaDemand <- aquaDemandTable
  
  keys <- c("geographicAreaM49", "timePointYears")
  setkeyv(aquaDemand, keys)
  
  aquaDemand
  
}