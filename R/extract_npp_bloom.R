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
                        .x["SpringBloom"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
                        .x
                      })
  
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
                         .x["bloomValue"] <- raster::extract(raster(.x$fullname[1]), as.matrix(.x[c("X", "Y")]))
                         .x
                       })
  
  # recombine dataframes, filling NA for unavailable years
  foo <- foo %>% dplyr::select(-c("fullname", "X", "Y"))
  suppressMessages(df <- df %>% left_join(foo))
  df <- df %>% arrange(id, date)
  
  return(df)
}


