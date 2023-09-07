##################################
### Extract npp data to points ###
##################################

# Given a location data frame and a path to npp bloom rasters
# Import npp bloom data and extract for specified locations

# 2022-07-19
# revise 2023-09-05

extract_npp_bloom <- function(df, path){
  
  ## build a file-db
  files <- tibble(fullname = list.files(normalizePath(path), recursive = T, full.names = T, pattern = "5percent.grd"), 
                  year = substr(basename(fullname), 5, 8))
  files$fullname <- substr(files$fullname, 1, nchar(files$fullname)-4)
  
  # check that all the years exist in the data set
  foo <- df %>% filter(year(date) %in% files$year)
  
  ## map our data to this file-db (we might have multiple points per file)
  foo$fullname <- files$fullname[findInterval(year(foo$date), files$year)]
  
  message("npp data not available for ", nrow(df) - nrow(foo), " locations")

  ## set up progress bar based on number of files that will be loaded
  pb <- progress_estimated(length(unique(foo$fullname)))
  
  (rdummy <- raster(foo$fullname[1]))
  
  ## project the query points
  foo[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(foo[c("lon", "lat")]), projection(rdummy)))
  
  ## now, extract per file 
  foo <- purrr::map_df(split(foo, foo$fullname)[unique(foo$fullname)], 
                      function(.x) {
                        pb$tick()$print()
                        .x["BloomDay"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
                        .x
                      })
  
  # some coastal points excluded here
  # move to nearest available ice cell within 50 km
  source("R/nearestLand.R")
  foo2 <- foo |> filter(is.na(BloomDay))
  foo2[c("X2", "Y2")] <- nearestLand(cbind(foo2$X, foo2$Y), rdummy, 50000)
  ## now, re-extract per file 
  foo2 <- purrr::map_df(split(foo2, foo2$fullname)[unique(foo2$fullname)], 
                        function(.x) {
                          .x["BloomDay"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X2", "Y2")]))
                          .x
                        })
  foo$BloomDay[is.na(foo$BloomDay)] <- foo2$BloomDay
  
  # recombine dataframes, filling NA for unavailable years
  foo <- foo %>% dplyr::select(-c("fullname", "X", "Y"))
  suppressMessages(df <- df %>% left_join(foo))
  df <- df %>% arrange(id, date)
  
  return(df)
}

extract_phenology_bloom <- function(df, path){

  ## build a file-db
  files <- tibble(fullname = list.files(normalizePath(path), recursive = T, full.names = T, pattern = "bloom"), 
                  year = substr(basename(fullname), 5, 8))
  files$fullname <- substr(files$fullname, 1, nchar(files$fullname)-4)
  files <- files %>% distinct()
  
  # check that all the years exist in the data set
  foo <- df %>% filter(year(date) %in% files$year)
  
  ## map our data to this file-db (we might have multiple points per file)
  foo$fullname <- files$fullname[findInterval(year(foo$date), files$year)]
  
  message("npp data not available for ", nrow(df) - nrow(foo), " locations")
  
  ## set up progress bar based on number of files that will be loaded
  pb <- progress_estimated(length(unique(foo$fullname)))
  
  (rdummy <- raster(foo$fullname[1]))
  
  ## project the query points
  foo[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(foo[c("lon", "lat")]), projection(rdummy)))
  
  ## now, extract per file 
  foo <- purrr::map_df(split(foo, foo$fullname)[unique(foo$fullname)], 
                       function(.x) {
                         pb$tick()$print()
                         .x["BloomValue"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
                         .x
                       })
  
  # some coastal points excluded here
  # move to nearest available ice cell within 50 km
  source("R/nearestLand.R")
  foo2 <- foo |> filter(is.na(BloomValue))
  foo2[c("X2", "Y2")] <- nearestLand(cbind(foo2$X, foo2$Y), rdummy, 50000)
  ## now, re-extract per file 
  foo2 <- purrr::map_df(split(foo2, foo2$fullname)[unique(foo2$fullname)], 
                        function(.x) {
                          .x["BloomValue"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X2", "Y2")]))
                          .x
                        })
  foo$BloomValue[is.na(foo$BloomValue)] <- foo2$BloomValue
  
  # recombine dataframes, filling NA for unavailable years
  foo <- foo %>% dplyr::select(-c("fullname", "X", "Y"))
  suppressMessages(df <- df %>% left_join(foo))
  df <- df %>% arrange(id, date)
  
  return(df)
}


