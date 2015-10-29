calculateAquaDemand = function() {
  
  keys <- c("geographicAreaM49", "timePointYears")
  
  aquaDemand = setkeyv(aquaDemandTable, keys)
  
  aquaDemand
  
}