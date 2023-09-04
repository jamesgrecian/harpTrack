##############################################################################
### Function to convert files from Jo Hopkins to single daily raster files ###
##############################################################################

mat_to_raster <- function(fn){
  
  # load coordinates from seperate file
  coords <- read_mat('data/NPP/nav_mat.mat', ram = T)
  
  # load seperate productivity data
  dat <- read_mat(paste0("data/NPP/", fn), ram = T)
  
  # dat is a list with an array for productivity data
  # rows of PP are lat lon cells, and columns are days
  # the files are stored as lat lon but when plotted are not on a regular grid
  # looking at the paper the data are in a stereographic projection:
  # https://science.sciencemag.org/content/369/6500/198
  
  # convert points to raster using the rasterise function
  # need a raster to melt the data into...
  
  coords_xy <- tibble(lon = as.numeric(coords$lon),
                      lat = as.numeric(coords$lat)) %>%
    st_as_sf(coords = c("lon", "lat")) %>%
    sf::st_set_crs(4326) %>%
    st_transform("+proj=stere +lat_0=90") %>%
    st_coordinates()
  
  dummy <- raster(ext = extent(coords_xy),
                  resolution = 25000, # downgrade to the same as NSIDC sea ice
                  crs = "+proj=stere +lat_0=90")
  
  pb1 <- txtProgressBar(min = 1, max = ncol(dat$PP), style = 3)
  
  # now use rasterise to melt data into dummy raster extent...
  for (i in 1:ncol(dat$PP)){
    setTxtProgressBar(pb1, i)
    out <- rasterize(coords_xy, dummy, field = dat$PP[,i], fun = mean, na.rm = T)
    writeRaster(out,
                paste0("data/NPP/NPP_", substr(fn, 1, 4),
                       "/NPP_", substr(fn, 1, 4), str_pad(i, width = 3, side = "left", pad = 0)))
  }
# this has now saved the large 13 GB dataframe as 365 individual daily rasters
# from this I can export the NPP associated with a particular day
# and calculate the day of the year with peak NPP
  rm(dat)
  rm(coords)
  rm(out)
}

#ends
