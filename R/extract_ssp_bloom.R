#####################################################
### Function to extract SSP estimated ice retreat ###
#####################################################

# 2023-09-05

# function based on extract_ice_retreat
# modified to pull from difference layer
extract_ssp_bloom <- function(df, path, scenario){
  
  ## build a file-db
  files <- tibble(fullname = list.files(normalizePath(path), recursive = T, full.names = T, pattern = scenario), 
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
                         .x["sspBloom"] <- raster::extract(stack(.x$fullname[1]), as.matrix(.x[c("X", "Y")]), layer = 2, nl = 1)
                         .x
                       })

  # recombine dataframes, filling NA for unavailable years
  foo <- foo %>% dplyr::select(-c("fullname", "X", "Y"))
  suppressMessages(df <- df %>% left_join(foo))
  df <- df %>% arrange(id, date)
  
  return(df)
}
