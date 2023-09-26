##################################################
### Function to calculate NPP anomaly for SSPs ###
##################################################

# use for plotting

sspAnomalyNPP <- function(path_to_present, path_to_future, scenario){
  
  fn <- list.files(path_to_present, pattern = "5percent", full.names = T)
  fn <- fn |> substr(1, nchar(fn)-4) |> unique()
  present_bloom <- stack(fn)
  
  fn <- list.files(path_to_future, pattern = scenario, full.names = T)
  fn <- fn |> substr(1, nchar(fn)-4) |> unique()
  fn <- fn[grep("v2", fn)]
  future_bloom <- stack(fn, bands = 2)

  present_bloom[is.na(present_bloom)] <- 246
#  future_bloom[is.na(future_bloom)] <- 245
  
  # anomaly is future - present 
  npp_anom <- future_bloom - present_bloom
  npp_anom <- calc(npp_anom, mean)
  
  return(npp_anom)
}

# ends
