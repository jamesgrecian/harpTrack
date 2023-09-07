###################################################
### Extract sea ice and NPP data to seal tracks ###
###################################################

# 2023-09-04

# load libraries
require(tidyverse)
require(sf)
require(aniMotum)
require(raster)
require(lubridate)

# load harp seal foieGras output
fit <- readRDS("data/harp_fit_20230904.rds")

# reroute around land...
fit <- route_path(fit,
                  what = "predicted",
                  map_scale = 50,
                  dist = 50000,
                  append = T)

# calculate move persistence
fmpm <- fit_mpm(fit |> filter(id != "hp6-L004-19"), what = "rerouted", model = "mpm")
pmpm <- fit_mpm(fit |> filter(id == "hp6-L004-19"), what = "predicted", model = "mpm")

# combine
dat <- join(fit |> filter(id != "hp6-L004-19"), fmpm, what.ssm = "rerouted")
lft <- join(fit |> filter(id == "hp6-L004-19"), pmpm, what.ssm = "predicted")
dat <- dat |> bind_rows(lft)
dat <- dat |> arrange(id, date)

# append day of year
# filter to only consider days 50 to 250
dat <- dat %>% mutate(day = lubridate::yday(date))
dat <- dat %>% filter(day >= 50)
dat <- dat %>% filter(day <= 250)

# five animals tracked over winter but failed late winter/ early spring (moult)
# remove locations from the incomplete second spring
dat <- dat |> mutate(year = lubridate::year(date))
dat <- dat |> filter(year != 1997)
dat <- dat |> filter(year != 2000)
dat <- dat |> filter(year != 2005)

# format tibble
dat <- dat |> dplyr::select(id, date, lon, lat, g, day)

# use custom functions to append year-specific processed ice and npp data
source("R/extract_ice_retreat.R")
dat <- extract_ice_retreat(df = dat, path = "data/NSIDC")

source("R/extract_npp_bloom.R")
dat <- extract_npp_bloom(df = dat, path = "data/NPP")
dat <- extract_phenology_bloom(df = dat, path = "data/NPP")

# append climatological ice retreat
ice_clim <- raster("data/NSIDC/ice_phenology_climatology")
dat[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(dat[c("lon", "lat")]), projection(ice_clim)))
dat <- dat %>% mutate(IceDayClim = raster::extract(ice_clim, cbind(X, Y)),
                      IceID = raster::cellFromXY(ice_clim, cbind(X, Y)))

sub <- dat |> filter(is.na(IceDayClim))
sub[c("X2", "Y2")] <- nearestLand(cbind(sub$X, sub$Y), ice_clim, 50000)
sub$IceDayClim <- raster::extract(ice_clim, cbind(sub$X2, sub$Y2))
dat$IceDayClim[is.na(dat$IceDayClim)] <- sub$IceDayClim
dat$IceDayClim[dat$lat < 43] <- 0 # couple of points below extent of NSIDC raster
dat <- dat %>% dplyr::select(-c("X", "Y"))

# append climatological spring bloom
npp_clim <- raster("data/NPP/npp_phenology_climatology")
dat[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(dat[c("lon", "lat")]), projection(npp_clim)))
dat <- dat %>% mutate(BloomDayClim = raster::extract(npp_clim, cbind(X, Y)),
                      BloomID = raster::cellFromXY(npp_clim, cbind(X, Y)))

sub <- dat |> filter(is.na(BloomDayClim))
sub[c("X2", "Y2")] <- nearestLand(cbind(sub$X, sub$Y), npp_clim, 50000)
sub$BloomDayClim <- raster::extract(npp_clim, cbind(sub$X2, sub$Y2))
dat$BloomDayClim[is.na(dat$BloomDayClim)] <- sub$BloomDayClim
dat <- dat %>% dplyr::select(-c("X", "Y"))

# make sure that bloom values aren't interpolated from areas where there is no
# npp data
# i.e. there's a value for BloomDayClim but not BloomID
dat$BloomDayClim[is.na(dat$BloomID)] <- NA
# climatology extract has also appended to year where there is no npp data...
dat$BloomDayClim[lubridate::year(dat$date) == 1996] <- NA

# now check against older data
p1 <- ggplot() + 
  theme_bw() +
  geom_point(aes(x = IceDay, y = day), data = dat |> filter(IceDay > 0)) +
  geom_smooth(aes(x = IceDay, y = day), method = "lm", data = dat |> filter(IceDay > 0)) +
  geom_abline(intercept = 0, linetype = "dashed") +
  coord_fixed(xlim = c(0, 250), ylim = c(50, 250)) +
  xlab("Timing of Ice Retreat")

p2 <- ggplot() + 
  theme_bw() +
  geom_point(aes(x = BloomDay, y = day), data = dat |> filter(IceDay  > 0)) +
  geom_smooth(aes(x = BloomDay, y = day), method = "lm", data = dat |> filter(IceDay  > 0)) +
  geom_abline(intercept = 0, linetype = "dashed") +
  coord_fixed(xlim = c(0, 250), ylim = c(50, 250)) +
  xlab("Timing of Ice Associated Spring Bloom")

p3 <- ggplot() + 
  theme_bw() +
  geom_point(aes(x = BloomDay, y = day), data = dat |> filter(IceDay  == 0)) +
  geom_smooth(aes(x = BloomDay, y = day), method = "lm", data = dat |> filter(IceDay  == 0)) +
  geom_abline(intercept = 0, linetype = "dashed") +
  coord_fixed(xlim = c(0, 250), ylim = c(50, 250)) +
  xlab("Timing of Open Water Spring Bloom")


require(patchwork)
p1 + p2 + p3 + plot_layout(ncol = 1)

glm(day ~ IceDay, data = dat |> filter(IceDay > 0))
glm(day ~ BloomDay, data = dat |> filter(IceDay > 0))
glm(day ~ BloomDay, data = dat |> filter(IceDay == 0))

###
### Extract SSP data 
###

# then move on to extract SSP data...
# source("R/extract_ssp_retreat.R")
# dat <- extract_ssp_retreat(df = dat, path = "data/NSIDC", lyr = 2)

# ice phenology anomaly - days difference between historical and SSP models
ice_phen_ssp245 <- raster("data/CMIP6 ice/CMIP6_ice_anomaly_ssp245_phenology")
ice_phen_ssp585 <- raster("data/CMIP6 ice/CMIP6_ice_anomaly_ssp585_phenology")

dat <- dat |> mutate(IceDay245 = raster::extract(ice_phen_ssp245, cbind(lon, lat)),
                     IceDay585 = raster::extract(ice_phen_ssp585, cbind(lon, lat)))

# load NPP bloom phenology data
# layer 1 is the estimated bloom including the SSP anomaly
# layer 2 is the estimated bloom day including the SSP anomaly

# use local function to append day of contemporaneous ice retreat
# remember if day is 0 then never ice, if 279 then always ice

# use local function to append day of contemporaneous npp spring bloom
source("R/extract_ssp_bloom.R")
dat <- extract_ssp_bloom(df = dat, path = "data/CMIP6 NPP anomaly/", scenario = "ssp245")
names(dat)[16] <- "BloomDay245"
dat <- extract_ssp_bloom(df = dat, path = "data/CMIP6 NPP anomaly/", scenario = "ssp585")
names(dat)[17] <- "BloomDay585"

summary(dat)


# trim hp4-M489-17 to less than 2017-06-05
# this removes strange artifact curve from foieGras fit
dat <- dat %>% filter(case_when(id == "hp4-M489-17" ~ date < as.Date("2017-06-05"),
                                id != "hp4-M489-17" ~ day > 0)) 

# save output
saveRDS(dat, "data/harp_locs_routed_covariates_20230907.rds")

# ends