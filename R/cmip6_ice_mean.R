###
### Function to calculate daily mean concentration across years from CMIP6 data
###

cmip6_ice_mean <- function(filenames){
  
  pb1 <- txtProgressBar(min = 1, max = 245, style = 3)
  out <- stack()
  
  for(j in 1:245){
    day <- j
    setTxtProgressBar(pb1, j) # update progress bar
    
    x <- raster::stack()
    
    for (i in 1:length(filenames)){
      ice <- stack(filenames[i], varname = "mean")
      ice <- subset(ice, day)
      x <- raster::stack(x, ice)
    }
    
    x <- mean(x)
    out <- stack(out, x)
  }
  names(out) <- 1:245
  
  return(out)
}
