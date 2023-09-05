##################################
### Extract ice data to points ###
##################################

# Given a location data frame and a set of ice rasters
# Import ice retreat data and extract for specified locations

# 2022-07-20
# modified 2023-09-05 for new ice retreat calculated from peak ice not 1st March

extract_ice_retreat <- function(df, path){
  
  ## build a file-db
  files <- tibble(fullname = list.files(normalizePath(path), recursive = T, full.names = T, pattern = "ice_phenology"), 
                  year = substr(basename(fullname), 15, 18))
  files$fullname <- substr(files$fullname, 1, nchar(files$fullname)-4)
  files <- files %>% distinct()
  files <- files |> slice(1:n()-1)

  # check that all the years exist in the data set
  foo <- df %>% filter(year(date) %in% files$year)
  
  ## map our data to this file-db (we might have multiple points per file)
  foo$fullname <- files$fullname[findInterval(year(foo$date), files$year)]
  
  message("NSIDC data not available for ", nrow(df) - nrow(foo), " locations")
  
  ## set up progress bar based on number of files that will be loaded
  pb <- progress_estimated(length(unique(foo$fullname)))
  
  (rdummy <- raster(foo$fullname[1]))
  
  ## project the query points
  foo[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(foo[c("lon", "lat")]), projection(rdummy)))
  
  ## now, extract per file 
  foo <- purrr::map_df(split(foo, foo$fullname)[unique(foo$fullname)], 
                       function(.x) {
                         pb$tick()$print()
                         .x["IceDay"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
                         .x
                       })
  
  # recombine dataframes, filling NA for unavailable years
  foo <- foo %>% dplyr::select(-c("fullname", "X", "Y"))
  suppressMessages(df <- df %>% left_join(foo))
  df <- df %>% arrange(id, date)
  
  return(df)
}


## old function when ice was from 1st March
#extract_ice_retreat <- function(df){
#  
#  # load ice data
#  ice <- raster::stack("data/phenology_test")
#  names(ice) <- paste0("ice_retreat_", 1995:2020)
#  
#  # find indices of raster that match tracking years
#  ys <- sort(unique(year(df$date)))
#  id <- which(1995:2020 %in% ys)
#  
#  # define native projection for ice data and extract to locations
#  df[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(dat[c("lon", "lat")]), projection(ice)))
#  
#  # loop through ice layers and extract to year matched locations
#  df$ice_day <- NA
#  
#  for (i in 1:length(ys)){
#    df$ice_day[year(df$date) == ys[i]] <- raster::extract(subset(ice, id[i]),
#                                                          cbind(df$X[year(df$date) == ys[i]],
#                                                                df$Y[year(df$date) == ys[i]]))
#  }
#  
#  # drop projected XY
#  df <- df %>% dplyr::select(-c("X", "Y"))
#  df <- df %>% arrange(id, date)
#  
#  return(df)
#}
