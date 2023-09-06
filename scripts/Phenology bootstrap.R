###############################################################
### Estimate correlation between timing of seal migration,  ###
### ice retreat and spring bloom using bootstrap regression ###
###############################################################

# updated 2022-03-30 to use updated NPP data
# updated 2022-07-20 with revised/ simplified foieGras output,
# re-routed tracks, and revised covariate extraction

# updated 2023-09-06 using aniMotum
# revised ice retreat using days after local ice maximum
# include SSP scenarios for ice and NPP

# Estimate correlation between timing of migration and timing of ice retreat
# Estimate correlation between timing of migration and timing of ice-associated NPP spring bloom
# Estimate correlation between timing of migration and timing of non ice-associated NPP spring bloom
# Estimate correlation between timing of current migration and above using SSP245 and 585 data

# Initial analysis indicated temporal autocorrelation in the time series
# Randomly thin data to remove autocorrelation
# check with Durbin Watson test (values of 2 mean no autocorrelation)
# bootstrap randomisation 10,000 times

# double check correct data is filtered for each bootstrapped regression

# load libraries
require(tidyverse)
require(lubridate)
require(nlme)
require(car)
require(MuMIn)
require(patchwork)

# load data with covariates
dat <- readRDS("data/harp_locs_routed_covariates_20230906.rds")
dat <- dat |> filter(day >= 60) # drop locations before 1st March

# Add deployment year
dat <- dat |> mutate(year = lubridate::year(date),
                     age = case_when(year < 2010 ~ "adult",
                                     year > 2010 ~ "young"),
                     age = as.factor(age))

# For deployments that spanned longer than 12 months floor the year
#dat$year[dat$year == 1997] <- 1996
#dat$year[dat$year == 1999] <- 2000
#dat$year[dat$year == 2004] <- 2005

# only consider the first occurrence of animal in a cell - the day it arrives
# cell shape differs between sea ice and npp data so create dataframe for each
dat_ice <- dat |>
  group_by(id) |>
  arrange(id, date) |> # arrange by id and date
  distinct(IceID, .keep_all = TRUE) |> 
  ungroup()
dat_ice <- dat_ice |> drop_na(IceDay)

dat_npp <- dat |>
  group_by(id) |>
  arrange(id, date) |> # arrange by id and date
  distinct(BloomID, .keep_all = TRUE) |> 
  ungroup()
dat_npp <- dat_npp |> drop_na(BloomDay)

################################
### Bootstrapped regressions ###
################################
source("R/boot_regressions.R")
track_ice <- boot_regression_ice(data = dat_ice |> filter(IceDay > 0),
                                 permutations = 10000,
                                 fraction = 0.1)

track_ice_bloom <- boot_regression_bloom(data = dat_npp |> filter(IceDay > 0),
                                         permutations = 10000,
                                         fraction = 0.1)

track_open_bloom <- boot_regression_bloom(data = dat_npp |> filter(IceDay == 0),
                                         permutations = 10000,
                                         fraction = 0.1)

###################################################
### Check for change in relationships over time ###
###################################################

track_ice_years <- boot_regression_ice_years(data = dat_ice |> filter(IceDay > 0),
                                 permutations = 1000,
                                 fraction = 0.1)
track_bloom_years <- boot_regression_bloom_years(data = dat_npp |> filter(IceDay > 0),
                                    permutations = 1000,
                                    fraction = 0.1)
summary(track_ice_years[[3]])
summary(track_bloom_years[[3]])

# this needs more thought...
# different way of calculating permutations?

###################################
### Generate plots for Figure 1 ###
###################################

# custom plot theme
plot_theme <- function(){
  
  font <- "Helvetica"   #assign font family up front
  
  theme_bw(base_size = 7) %+replace%    #replace elements we want to change
    
    theme(
      # text elements
      plot.title = element_text(             #title
        size = 8,                #set font size
        face = 'bold',            #bold typeface
        hjust = -.1,                #left align
        vjust = -.1),               #lower slightly
      
      # plot elements
      plot.margin = margin(.2,.2,.2,.2, "cm")
    )
}

p1 <- ggplot() +
  plot_theme() +
  coord_cartesian(xlim = c(0, 250), ylim = c(50, 250)) +
  xlab("Timing of Ice Retreat") + ylab("Day of Arrival") +
  geom_point(aes(x = IceDay, y = day), size = 0.5, alpha = 0.8, colour = "grey50",
             data = dat_ice |> filter(IceDay > 0)) +
  geom_abline(intercept = 0, slope = 1, color = "grey40", linewidth = 1, linetype = "dashed") +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax, group = name), alpha = .25, data = track_ice[[2]]) +
  geom_line(aes(x = x, y = y), data = track_ice[[1]], colour = "black", size = 1)

p2 <- ggplot() +
  plot_theme() +
  coord_cartesian(xlim = c(50, 250), ylim = c(50, 250)) +
  xlab("Timing of Ice Associated Spring Bloom") + ylab("Day of Arrival") +
  geom_point(aes(x = BloomDay, y = day), size = 0.5, alpha = 0.8, colour = "grey50",
             data = dat_npp |> filter(IceDay > 0)) +
  geom_abline(intercept = 0, slope = 1, color = "grey40", size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax, group = name), alpha = .25, data = track_ice_bloom[[2]]) +
  geom_line(aes(x = x, y = y), data = track_ice_bloom[[1]], colour = "black", size = 1)

p3 <- ggplot() +
  plot_theme() +
  coord_cartesian(xlim = c(50, 255), ylim = c(50, 250)) +
  xlab("Timing of Open Water Spring Bloom") + ylab("Day of Arrival") +
  geom_point(aes(x = BloomDay, y = day), size = 0.5, alpha = 0.8, colour = "grey50",
             data = dat_npp |> filter(IceDay == 0)) +
  geom_abline(intercept = 0, slope = 1, color = "grey40", size = 1, linetype = "dashed") +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax, group = name), alpha = .25, data = track_open_bloom[[2]]) +
  geom_line(aes(x = x, y = y), data = track_open_bloom[[1]], colour = "black", size = 1)

quartz(title = "Panel Plot", width = 10, height = 4)
print(p1 + p2 + p3)
quartz.save(file = "plots/Arrival bootstrap 20230906.jpeg", type = "jpeg",
            dev  = dev.cur(), dpi = 500)
dev.off()

########################################################
### Combine with maps of tracks, ice retreat and npp ###
########################################################

# load libraries
require(raster)
require(sf)
library(rphylopic)
source("~/Dropbox/git_projects/harpPup/R/discrete_gradient.R")

# turn off s2 errors
sf::sf_use_s2(FALSE)

# load land shapefile for plotting and clip to study region
world_shp <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

# clip world shapefile to southern hemisphere
CP <- sf::st_bbox(c(xmin = -180,
                    xmax = 180,
                    ymin = 5,
                    ymax = 90), crs = 4326) |>
  sf::st_as_sfc()
world_shp <- world_shp |> sf::st_buffer(0) |> sf::st_crop(CP)
world_shp <- world_shp |> st_transform("+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# load ice phenology climatology
ice <- raster("data/NSIDC/ice_phenology_climatology")
ice_clim <- projectRaster(ice, res = 25000, crs = "+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# load NPP
npp_clim <- raster("data/NPP/npp_phenology_climatology")
npp_clim <- projectRaster(npp_clim, res = 25000, crs = "+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
plot(npp_clim)

# reload data including pre day 60...
dat <- readRDS("data/harp_locs_routed_covariates_20230906.rds")
dat[c("x2", "y2")] <- as_tibble(rgdal::project(as.matrix(dat[c("lon", "lat")]), projection(ice_clim)))

par(mfrow = c(1, 3))
plot(dat$x2, dat$y2)
plot(ice_clim)
plot(npp_clim)

# convert NPP and ice to tibble for plotting with ggplot
npp_df <- npp_clim |> 
  rasterToPoints() |> 
  as_tibble()
ice_df <- ice_clim |> 
  rasterToPoints() |> 
  as_tibble()

# custom map theme
# https://rpubs.com/mclaire19/ggplot2-custom-themes
map_theme <- function(){
  
  font <- "Helvetica"   #assign font family up front
  
  theme_bw(base_size = 7) %+replace%    #replace elements we want to change
    
    theme(
      # text elements
      plot.title = element_text(             #title
        size = 8,                #set font size
        face = 'bold'),            #bold typeface

      # grid elements
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      
      # legend elements
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.spacing = unit(.2, 'cm'),
      legend.box.margin = margin(0, 0, 0, 0, "cm"),
      legend.margin = margin(0, 0, 0, 0, "cm"),
      
      # plot elements
      plot.margin = margin(.1, .1, .1, -.5, "cm")
      )
}

map_colourbar <- guide_colourbar(nbin = 500,
                                 raster = T,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 frame.linewidth = .25,
                                 barwidth = .4,
                                 barheight = 8,
                                 direction = "vertical",
                                 title.position = "right", #or "right"
                                 title.theme = element_text(angle = 90,
                                                            hjust = 0.5,
                                                            size = 7))
map_colourbar_hoz <- guide_colourbar(nbin = 500,
                                 raster = T,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 frame.linewidth = .25,
                                 barwidth = 9,
                                 barheight = .5,
                                 direction = "horizontal",
                                 title.position = "bottom", #or "right"
                                 title.theme = element_text(hjust = 0.5,
                                                            size = 7))
require(smoothr)
harpMask <- stack("~/Dropbox/git_projects/harpDist/data/harpMask_WGS84.nc")
names(harpMask) <-c("population_95ud", "northwest_95ud", "westice_95ud", "eastice_95ud",
                    "population_dens", "northwest_dens", "westice_dens", "eastice_dens")
nw_poly <- rasterToPolygons(subset(harpMask, 2), dissolve = T)
gr_poly <- rasterToPolygons(subset(harpMask, 3), dissolve = T)
ws_poly <- rasterToPolygons(subset(harpMask, 4), dissolve = T)
nw_poly <- nw_poly |> st_as_sf() |> smooth(method = "ksmooth", smoothness = 10)
gr_poly <- gr_poly |> st_as_sf() |> smooth(method = "ksmooth", smoothness = 10)
ws_poly <- ws_poly |> st_as_sf() |> smooth(method = "ksmooth", smoothness = 10)

p4 <- ggplot() +
  map_theme() +
  geom_point(aes(x = x2, y = y2, colour = day), size = .25, data = dat) +
  geom_sf(aes(), colour = "grey50", fill = "grey50", data = world_shp) +
  coord_sf(xlim = c(-4030000, 2500000),
           ylim = c(-4500000, 2500000),
           crs = "+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ",
           expand = F) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 10)) +
  xlab(NULL) + ylab(NULL) +
  scale_colour_discrete_gradient("Day of Year",
                                 colours = viridis::viridis(16),
                                 bins = 16,
                                 limits = c(50, 250),
                                 breaks = seq(50, 250, 25),
                                 na.value = "black",
                                 guide = map_colourbar_hoz) +
  add_phylopic(name = "harp seal", alpha = 1, x = -3000000, y = 2000000, ysize = 500000, color = "black")

p5 <- ggplot() +
  map_theme() +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black")) +
  geom_raster(aes(x = x, y = y, fill = layer), data = ice_df) + 
  geom_sf(aes(), colour = "grey50", fill = "grey50", data = world_shp) +
  coord_sf(xlim = c(-4030000, 2500000),
           ylim = c(-4500000, 2500000),
           crs = "+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ",
           expand = F) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 10)) +
  xlab(NULL) + ylab(NULL) +
  scale_fill_discrete_gradient("Day of Year",
                               colours = rev(colorRampPalette(RColorBrewer::brewer.pal(9, 'Blues'))(22)),
                               bins = 22,
                               limits = c(0, 250),
                               breaks = seq(0, 250, 25),
                               na.value = "black",
                               guide = map_colourbar_hoz)

p6 <- ggplot() +
  map_theme() +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black")) +
  geom_raster(aes(x = x, y = y, fill = layer), data = npp_df) +
  geom_sf(aes(), colour = "grey50", fill = "grey50", data = world_shp) +
  coord_sf(xlim = c(-4030000, 2500000),
           ylim = c(-4500000, 2500000),
           crs = "+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ",
           expand = F) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 10)) +
  xlab(NULL) + ylab(NULL) +
  scale_fill_discrete_gradient("Day of Year",
                               colours = colorRampPalette(RColorBrewer::brewer.pal(9, 'Greens'))(16),
                               bins = 16,
                               limits = c(50, 250),
                               breaks = seq(50, 250, 25),
                               na.value = "black",
                               guide = map_colourbar_hoz)

# save panel plot
quartz(title = "Panel Plot", width = 7.2, height = 5.2) # full page width is 183 mm or 7.2 inches
p4 + p5 + p6 + p1 + p2 + p3 + 
  plot_layout(ncol = 3, nrow = 2, heights = c(1.2, 1), widths = c(1,1,1)) + 
  plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0, 1), plot.tag = element_text(size = 8, face = 'bold', hjust = -1, vjust = 0))
quartz.save(file = "plots/example figure 1 v10.jpeg", type = "jpeg", dev  = dev.cur(), dpi = 500)

# ends