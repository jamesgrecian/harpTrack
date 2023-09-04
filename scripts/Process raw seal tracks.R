###############################################
### Load and clean harp seal telemetry data ###
###############################################

# 2023-09-04

# load libraries
require(tidyverse)
require(lubridate)
require(sf)
require(aniMotum)
source("R/prepData.R")

# load data
dat <- readRDS("data/harp_locations_unfiltered.rds")

# reformat id from factor to character for foieGras
dat <- dat %>% mutate(id = as.character(id))
dat <- dat %>% arrange(id, date)

# automate a lot of the data cleaning with the prepData function...
dat <- prepData(dat)

# calculate tag duration for selected animals.... before trimmed to northward migration...
dat %>% group_by(id) %>% summarise(dur = last(date) - first(date)) %>% summarise(mean(dur), sd(dur))

# in this case we're only interested in the first 250 days
# this should remove southward migrations...

# pull columns needed for fit_smm
dat <- dat %>% dplyr::select("id", "date", "lc", "lon", "lat", "smaj", "smin", "eor")

# regularise ARGOS data using correlated random walk model
# several animals removed through pre-processing script due to issues with regularisation
fit <- fit_ssm(dat,
               model = "crw",
               time.step = 24,
               vmax = 4)
# check convergence
fit %>% print(n = Inf)

# pull the tracks and visualise to check...
locs <- fit %>% grab(what = "predicted", as_sf = T)
locs <- locs %>% mutate(day = lubridate::yday(date))
locs <- locs %>% filter(day < 251)

# load shapefile for mapping
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

for (i in 1:length(unique(locs$id))){
  p1 <- ggplot() +
    geom_sf(aes(), data = world %>% sf::st_transform("+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")) +
    geom_sf(aes(colour = lc),
            data = dat |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
              sf::st_transform("+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs") |>
              filter(id == unique(id)[i]) |> filter(lubridate::yday(date) < 251)) +
    geom_sf(aes(), data = locs %>% filter(id == unique(id)[i])) +
    ggtitle(unique(dat$id)[i]) +
    xlim(sf::st_bbox(locs)$xmin, sf::st_bbox(locs)$xmax) +
    ylim(sf::st_bbox(locs)$ymin, sf::st_bbox(locs)$ymax)
  print(p1)
  readline(prompt="Press [enter] to continue:")
}

saveRDS(fit, "data/harp_fit_20230904.rds")

# ends