#########################################################################
### Functions that output change in phenology for npp and ice rasters ###
#########################################################################

# 2022-07-21

npp_phenology_change <- function(path){

  fn <- list.files(path, pattern = "5percent", full.names = T)
  fn <- fn %>% substr(1, nchar(fn)-4) %>% unique()
  npp_phenology_bloom <- stack(fn)
  npp_phenology_bloom[is.na(npp_phenology_bloom)] <- 365
  
  # create land mask by summing across stack
  # then divide by number of layers
  # any remaining cell that is 365 has a NA for each of the layers
  # this is either permanent ice or land
  land_mask <- calc(npp_phenology_bloom, fun = sum)
  land_mask <- land_mask/23
  land_mask[land_mask == 365] <- NA

  change <- stack()
  for(i in 1:(nlayers(npp_phenology_bloom)-1)){
    foo <- subset(npp_phenology_bloom, i + 1) - subset(npp_phenology_bloom, i)
    names(foo) <- i
    change <- stack(change, foo)
  }
  
  meanChange_npp <- calc(change, mean)
  meanChange_npp[is.na(land_mask)] <- NA
  return(meanChange_npp)
}

meanChange_ice <- ice_phenology_change(path = "data/NSIDC")

ice_phenology_change <- function(path){
  
  fn <- list.files(path, pattern = "phenology", full.names = T)
  fn <- fn %>% substr(1, nchar(fn)-4) %>% unique()
  fn <- fn[-28]
  fn <- fn[-27]
  ice <- raster::stack(fn, bands = 1)
  ice[is.na(ice)] <- 365
  
  land_mask <- calc(ice, fun = sum)
  land_mask <- land_mask/26
  land_mask[land_mask == 365] <- NA
  
  change <- stack()
  for(i in 1:(nlayers(ice)-1)){
    foo <- subset(ice, i + 1) - subset(ice, i)
    names(foo) <- i
    change <- stack(change, foo)
  }
  
  meanChange_ice <- calc(change, mean)
  meanChange_ice[is.na(land_mask)] <- NA
  
  return(meanChange_ice)
}

# ends


