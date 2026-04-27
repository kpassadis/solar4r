#Silence R CMD check warnings about "no visible binding for global variable" for columns used inside dplyr pipelines.
utils::globalVariables(c(
  "interval", 
  "rid", 
  "weight", 
  "is_daylight", 
  "yhat",            
  "electricity",     
  "irradiance_direct", 
  "irradiance_diffuse", 
  "temperature",
  "time"
))