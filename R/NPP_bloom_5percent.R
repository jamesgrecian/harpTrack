##################################################################
### Create NPP bloom intensity raster based on 5% above median ###
##################################################################

# function to load daily raster files and calculate timing of peak spring bloom
# simply using maximum value may highlight blooms later in year
# Instead calculate first time NPP is more than 5% above median
# Suggestion from Jo Hopkins based on Henson et al. 2009

# take the day of the peak bloom and
# use that value to index the actual value of peak NPP

NPP_bloom_5percent <- function(fn){
  # list all files
  path <- list.files(paste0("data/NPP/", fn),
                     pattern = fn,
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
  x <- raster::stack()
  
  writeLines("\nloading data...")
  for (i in 1:length(path)){
    setTxtProgressBar(pb1, i) # update progress bar
    NPP <- raster::raster(path[i])
    x <- raster::stack(x, NPP)
  }
  
  # trim by user defined quantile
  # remove top 95% percentile of data - these are mainly inshore coastal sediment driven
  x[x > quantile(raster::getValues(x), .95, na.rm = T)] <- NA
  
  # convert to tibble to calculate rowwise medians (ie for the time series at each xy location)
  npp_df <- values(x) %>% as_tibble()
  
  # function to calculate first time NPP is greater than 5% over the median for that cell
  which.median5.na <- function(x){
    if (all(is.na(x))){NA}
    else{first(which(x > median(as.numeric(x), na.rm = T) * 1.05))}
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

  npp_bloom <- raster(subset(x, 1))
  values(npp_bloom) <- bloom

  # save raster output
  writeRaster(npp_bloom, paste0("data/NPP/", fn, "_phenology_bloom"))

}

# ends
