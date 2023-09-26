##################################################
### Helper functions to process CMIP6 NPP data ###
##################################################

# 2023-08-17

# function to format .nc from Pearse
# returns raster stack for specified model containing daily anomaly
# these can then be passes to mean intpp nc function

format_intpp_nc <- function(path, model){
  
  my_nc <- ncdf4::nc_open(path)
  
  lat <- ncvar_get(my_nc, "ETOPO60Y")
  lon <- ncvar_get(my_nc, "ETOPO60X")
  lon[lon > 360] <- lon[lon > 360] - 360
  
  time <- ncvar_get(my_nc, "time")
  models <- ncvar_get(my_nc, "model")
  
  time_lon_lat <- expand.grid(time, lon, lat) |> as_tibble()
  names(time_lon_lat) <- c("time", "lon", "lat")
  
  intpp_array <- ncvar_get(my_nc, "intpp") 
  
  intpp_vec <- as.vector(intpp_array[ , , , model]) 
  
  intpp_obs <- time_lon_lat |> bind_cols(intpp = intpp_vec)
  intpp_obs <- intpp_obs |> mutate(time = time + 1) # shift days to 1-365
  
  xytz <- intpp_obs |> pivot_wider(names_from = time, values_from = intpp)
  r <- rasterFromXYZ(xytz)
  r <- rotate(r)
  projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  return(r)
}

# load in outputs from previous function
# return the ensemble mean
mean_intpp_nc <- function(path){
  
  my_nc <- ncdf4::nc_open(path)
  
  lat <- ncvar_get(my_nc, "ETOPO60Y")
  lon <- ncvar_get(my_nc, "ETOPO60X")
  lon[lon > 360] <- lon[lon > 360] - 360 # sort out longitude from 20-380 to -180-180
  
  time <- ncvar_get(my_nc, "time")
  models <- ncvar_get(my_nc, "model")
  
  time_lon_lat_model <- expand.grid(time, lon, lat, models) |> as_tibble()
  names(time_lon_lat_model) <- c("time", "lon", "lat", "model")
  
  intpp_array <- ncvar_get(my_nc, "intpp") 
  
  intpp_vec <- as.vector(intpp_array[ , , , ]) 
  
  intpp_obs <- time_lon_lat_model |> bind_cols(intpp = intpp_vec)
  intpp_obs <- intpp_obs |> mutate(time = time + 1) # shift days to 1-365
  
  intpp_mean <- intpp_obs |> group_by(time, lon, lat) |> summarise (mean_intpp = mean(intpp, na.rm = T))
  
  xytz <- intpp_mean |> pivot_wider(names_from = time, values_from = mean_intpp)
  
  r <- rasterFromXYZ(xytz)
  r <- rotate(r)
  projection(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  return(r)
}

# take the present day npp data and mean anomaly data
# add anomaly to each layer representing a single day 
# recalculate the timing of the spring bloom with the adjusted NPP anomaly
# make sure they are the same projection and dimensions
# make sure they are the same number of days

future_npp <- function(path_to_present, path_to_future){
  
  # load NPP satellite data
  path <- list.files(path_to_present,
                     full.names = T)
  path <- strsplit(path, split = ".", fixed = TRUE)
  path <- unlist(path)
  path <- unique(path)
  path <- sort(path)
  
  # pull out Jan 1st to Sept 1st
  # using whole year forces inclusion of autumn bloom
  path <- path[1:245] 
  
  # load all the NPP rasters into a single stack
  pb1 <- txtProgressBar(min = 1, max = length(path), style = 3)
  
  # Loop through days and load ice data into stack - this will get BIG
  present_npp <- raster::stack()
  
  writeLines("\nloading data...")
  for (i in 1:length(path)){
    setTxtProgressBar(pb1, i) # update progress bar
    NPP <- raster::raster(path[i])
    present_npp <- raster::stack(present_npp, NPP)
  }
  
  # load raster stack of SSP anomalies (either mean or GCM specific)
  future_npp <- stack(path_to_future) 
  future_npp <- subset(future_npp, 1:245) # subset to days 1:245
  future_npp <- projectRaster(future_npp, present_npp)
  
  # future NPP is in (milli?) moles per m^2 per second
  # not in mg per m^2 per day
  # multiply by 1000 x 12 x 86400
  future_npp <- future_npp * 1000 * 12 * 86400
  
  writeLines("\ncombining data layers with climate anomalies...")
  
  # work through each layer and combine observed NPP with SSP anomaly
  # Vectorized layer-wise summation using lapply
  output_layers <- lapply(1:nlayers(present_npp), function(i) {
    layer1 <- present_npp[[i]]
    layer2 <- future_npp[[i]]
    s <- stack(layer1, layer2)
    summed_layer <- calc(s, sum, na.rm = T)
    summed_layer <- mask(summed_layer, layer2) # mask by layer 2 not layer 1
    return(summed_layer)
  })
  
  # Create a new raster stack from the output layers
  output_stack <- stack(output_layers)
  
  # if adding the climate anomaly has made npp negative, shift to 0
  output_stack[output_stack < 0] <- 0
  
  # trim NPP by user defined quantile
  # remove top 95% percentile of data - these are mainly inshore coastal sediment driven
  output_stack[output_stack > quantile(raster::getValues(output_stack), .95, na.rm = T)] <- NA
  
  # convert to tibble to calculate rowwise medians (ie for the time series at each xy location)
  npp_df <- values(output_stack) %>% as_tibble()
  
  # function to calculate first time NPP is greater than 5% over the median for that cell
  which.median5.na <- function(x){
    if (all(is.na(x))){NA}
    else{first(which(x > (median(as.numeric(x), na.rm = T) * 1.05)))}
  }
  
  writeLines("\nindexing NPP peaks...")
  
  # apply function to each row in the dataframe
  pb2 <- txtProgressBar(min = 1, max = nrow(npp_df), style = 3)
  zoo <- NA
  for(j in 1:nrow(npp_df)){
    setTxtProgressBar(pb2, j) # update progress bar
    zoo[j] <- which.median5.na(npp_df[j,])
  }
  
  writeLines("\nextracting NPP peaks...")
  
  # use day indices to find npp value
  # use tryCatch to process NA values
  pb3 <- txtProgressBar(min = 1, max = length(zoo), style = 3)
  bloom <- NA
  for (i in 1:length(zoo)){
    setTxtProgressBar(pb3, i) # update progress bar
    bloom[i] <- tryCatch(as.numeric(npp_df[i, zoo[i]]),
                         error = function(err) NA)
  }
  
  # wrap up object to export
  npp_bloom <- raster(subset(output_stack, 1))  # base for output raster
  npp_days <- raster(subset(output_stack, 1))
  values(npp_bloom) <- bloom                    # append values
  values(npp_days) <- zoo
  out <- stack(npp_bloom, npp_days)             # stack to outputs together
  names(out) <- c("npp", "days")
  return(out)
}
