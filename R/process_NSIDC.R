##########################################
### Helper functions for NSIDC sea ice ###
##########################################

# sea ice and NPP data reworked for revised analysis 2023-09-04
# these are a series of new helper functions

# load and create a raster stack of daily sea ice data
process_NSIDC <- function(year){
  
  # Use RCurl library to query FTP server and list files
  url = "ftp://anonymous:wjg5@sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/geotiff/"
  
  # Time series is split into yearly and monthly folders
  months <- c("01_Jan", "02_Feb", "03_Mar", "04_Apr", "05_May", "06_Jun", "07_Jul", "08_Aug", "09_Sep")
  
  # generate vector of unique days for filenames
  days <- format(seq(as.Date(ISOdate(year, 1, 1)), as.Date(ISOdate(year, 9, 30)), 'days'), format="%Y%m%d", tz="UTC")
  
  fn <- c(paste0(url, year, "/", months[1], "/", "N_", days[month(ymd(days)) == 1], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[2], "/", "N_", days[month(ymd(days)) == 2], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[3], "/", "N_", days[month(ymd(days)) == 3], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[4], "/", "N_", days[month(ymd(days)) == 4], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[5], "/", "N_", days[month(ymd(days)) == 5], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[6], "/", "N_", days[month(ymd(days)) == 6], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[7], "/", "N_", days[month(ymd(days)) == 7], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[8], "/", "N_", days[month(ymd(days)) == 8], "_concentration_v3.0.tif"),
          paste0(url, year, "/", months[9], "/", "N_", days[month(ymd(days)) == 9], "_concentration_v3.0.tif"))
  
  pb1 <- txtProgressBar(min = 1, max = length(fn), style = 3)
  
  cat("loading ice data")
  
  # Loop through days and load ice data into stack - this will get BIG
  x <- raster::stack()
  
  for (i in 1:length(fn)){
    setTxtProgressBar(pb1, i) # update progress bar
    ice <- raster::raster(fn[i])
    # 0 is ocean; 2510 pole hole; 2530 coast line; 2540 land; 2550 missing
    # 0-1000 so divide by 10 to get percentage
    ice[ice == 2510] <- 1000 # make pole hole 100% ice cover
    ice[ice>1000] <- NA
    ice <- ice/10
    x <- raster::stack(x, ice)
  }
  raster::projection(x) = "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
  
  return(x)
}

shiftRotate <- function(input_raster){
  r_df <- as.data.frame(rasterToPoints(input_raster))
  r_df$x[r_df$x > 360] <- r_df$x[r_df$x > 360] - 360 # sort out longitude from 20-380 to -180-180
  r <- rasterFromXYZ(r_df)
  r <- rotate(r)
  return(r)
}

format_future <- function(present_ice, future_ice){
  project_future <- projectRaster(future_ice, present_ice)
  present_ice_ssp <- present_ice + project_future
  present_ice_ssp <- clamp(present_ice_ssp, lower = 0, upper = 100, useValues = T)
}

ice_phenology <- function(ice_stack){
  
  # convert daily raster stack to tibble
  ice_df <- values(ice_stack) %>% as_tibble()
  
  # receiving vector for indicies of days that represent loss of ice
  day_id <- rep(NA, nrow(ice_df))
  
  pb2 <- txtProgressBar(min = 1, max = length(day_id), style = 3) 
  
  # loop through rows of ice dataframe
  # find first instance when ice value is less than specified contour (i.e. 15%)
  # AFTER local maximum
  find_contour_day <- function(x){
    if (all(is.na(x))){NA}
    else if (all(x<15)){0} # make all values lower than 15% open water
    else{
      max_id <- which(x == max(x, na.rm = T)) # find index of max values
      max_id <- last(max_id) # if maximum value occurs more than once take last one 
      contour_id <- which(x < 15) # find index of all values less than 15%
      id <- first(contour_id[contour_id > max_id]) # now need first value from contour_id AFTER max_id
      return(id)
    }
  }
  
  for(j in 1:nrow(ice_df)){
    setTxtProgressBar(pb2, j) # update progress bar
    day_id[j] <- find_contour_day(ice_df[j,])
  }
  
  # take indices and append them to a raster with same structure as ice data
  day_ras <- raster(subset(ice_stack, 1))
  values(day_ras) <- day_id
  
  return(day_ras)
}

