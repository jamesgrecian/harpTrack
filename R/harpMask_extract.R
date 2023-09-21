##########################################################################
### Helper function to extract ice and npp phenology data to harp mask ###
##########################################################################

harpMask_extract_ice <- function(path_to_mask, path_to_ice){
  
  # load shapefiles for distribution of each breeding population
  # use density mask shapes generated for Camille's paper:
  harpMask <- stack(path_to_mask)
  names(harpMask) <-c("population_95ud", "northwest_95ud", "westice_95ud", "eastice_95ud",
                      "population_dens", "northwest_dens", "westice_dens", "eastice_dens")
  
  # pull out the shapes for each dataframe and convert to tibble
  harpMask <- rasterToPoints(subset(harpMask, 2:4)) %>% 
    as_tibble() %>% 
    pivot_longer(cols = 3:5, names_to = "population", values_to = "presence")
  harpMask <- harpMask %>% filter(!is.na(presence))
  harpMask <- harpMask %>% dplyr::select("population", "x", "y")
  names(harpMask)[2:3] <- c("lon", "lat")
  
  # load ice data
  fn <- list.files(path_to_ice, pattern = "phenology", full.names = T)
  fn <- fn %>% substr(1, nchar(fn)-4) %>% unique()
  fn <- fn[-28]
  fn <- fn[-27]
  ice_phenology <- raster::stack(fn, bands = 1)
  
  names(ice_phenology) <- c("DOY_1995", "DOY_1996", "DOY_1997", "DOY_1998", "DOY_1999",
                            "DOY_2000", "DOY_2001", "DOY_2002", "DOY_2003", "DOY_2004",
                            "DOY_2005", "DOY_2006", "DOY_2007", "DOY_2008", "DOY_2009",
                            "DOY_2010", "DOY_2011", "DOY_2012", "DOY_2013", "DOY_2014",
                            "DOY_2015", "DOY_2016", "DOY_2017", "DOY_2018", "DOY_2019",
                            "DOY_2020")

  # project for ice extraction
  harpMask[c("x", "y")] <- as_tibble(rgdal::project(as.matrix(harpMask[c("lon", "lat")]), projection(ice_phenology)))

  # extract covariates
  ice_extract <- raster::extract(ice_phenology, cbind(harpMask$x, harpMask$y)) %>% as_tibble()

  # format output dataframe
  ice_extract <- harpMask %>% 
    dplyr::select(population, lon, lat, x, y) %>% 
    bind_cols(ice_extract) %>%
    pivot_longer(cols = 6:31, names_to = "Year", values_to = "DOY") %>%
    mutate(Year = as.numeric(substr(Year, 5, 8)))
  
  return(ice_extract)
}

harpMask_extract_npp <- function(path_to_mask, path_to_npp){
    
    # load shapefiles for distribution of each breeding population
    # use density mask shapes generated for Camille's paper:
    harpMask <- stack(path_to_mask)
    names(harpMask) <-c("population_95ud", "northwest_95ud", "westice_95ud", "eastice_95ud",
                        "population_dens", "northwest_dens", "westice_dens", "eastice_dens")
    
    # pull out the shapes for each dataframe and convert to tibble
    harpMask <- rasterToPoints(subset(harpMask, 2:4)) %>% 
      as_tibble() %>% 
      pivot_longer(cols = 3:5, names_to = "population", values_to = "presence")
    harpMask <- harpMask %>% filter(!is.na(presence))
    harpMask <- harpMask %>% dplyr::select("population", "x", "y")
    names(harpMask)[2:3] <- c("lon", "lat")
    
    # load npp data
    fn <- list.files(path = path_to_npp, pattern = "5percent", full.names = T)
    fn <- fn %>% substr(1, nchar(fn)-4) %>% unique()
    npp_phenology <- stack(fn)
    names(npp_phenology) <- c("DOY_1998", "DOY_1999", "DOY_2000", "DOY_2001", "DOY_2002",
                              "DOY_2003", "DOY_2004", "DOY_2005", "DOY_2006", "DOY_2007",
                              "DOY_2008", "DOY_2009", "DOY_2010", "DOY_2011", "DOY_2012",
                              "DOY_2013", "DOY_2014", "DOY_2015", "DOY_2016", "DOY_2017",
                              "DOY_2018", "DOY_2019", "DOY_2020")
    
    # project for ice extraction and npp extraction
    harpMask[c("x", "y")] <- as_tibble(rgdal::project(as.matrix(harpMask[c("lon", "lat")]), projection(npp_phenology)))
    
    # extract covariates
    npp_extract <- raster::extract(npp_phenology, cbind(harpMask$x, harpMask$y)) %>% as_tibble()
    
    # format output dataframe
    npp_extract <- harpMask %>% 
      dplyr::select(population, lon, lat, x, y) %>% 
      bind_cols(npp_extract) %>%
      pivot_longer(cols = 6:28, names_to = "Year", values_to = "DOY") %>%
      mutate(Year = as.numeric(substr(Year, 5, 8)))
}

    
    
  
  