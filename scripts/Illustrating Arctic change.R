#########################################################
### How has the phenology of these variables changed? ###
#########################################################

# load libraries
require(raster)
require(tidyverse)
require(sf)
require(patchwork)
source("R/phenology_change.R")

# generate the rasters of spatial mean change in ice retreat and spring bloom
meanChange_ice <- ice_phenology_change(path = "data/NSIDC")
meanChange_npp <- npp_phenology_change(path = "data/NPP")

plot(meanChange_ice)
plot(meanChange_npp)

# extract ice and npp phenology data to harp distribution shapefile
source("R/harpMask_extract.R")
ice_extract <- harpMask_extract_ice(path_to_mask <- "~/Dropbox/git_projects/harpDist/data/harpMask_WGS84.nc",
                                    path_to_ice <- "data/NSIDC")

npp_extract <- harpMask_extract_npp(path_to_mask <- "~/Dropbox/git_projects/harpDist/data/harpMask_WGS84.nc",
                                    path_to_npp <- "data/NPP")

# summarise data for plotting
ice_summary <- ice_extract %>% 
  group_by(population, Year) %>% 
  summarise(DOY_ice = mean(DOY, na.rm = T)) %>%
  ungroup()
  
npp_summary <- npp_extract %>% 
  group_by(population, Year) %>% 
  summarise(DOY_npp = mean(DOY, na.rm = T)) %>%
  ungroup()

combo_summary <- ice_summary %>% left_join(npp_summary) %>% ungroup()

# report statistics for paper
m1 <- lm(DOY_ice ~ Year, data = ice_summary %>% filter(population == "northwest_95ud"))
m2 <- lm(DOY_ice ~ Year, data = ice_summary %>% filter(population == "westice_95ud"))
m3 <- lm(DOY_ice ~ Year, data = ice_summary %>% filter(population == "eastice_95ud"))
summary(m1)
summary(m2)
summary(m3)

m1 <- lm(DOY_npp ~ Year, data = npp_summary %>% filter(population == "northwest_95ud"))
m2 <- lm(DOY_npp ~ Year, data = npp_summary %>% filter(population == "westice_95ud"))
m3 <- lm(DOY_npp ~ Year, data = npp_summary %>% filter(population == "eastice_95ud"))
summary(m1)
summary(m2)
summary(m3)

#########################
### Generate Figure 2 ###
#########################
require(wesanderson)

# Phenology time series plots by ice and npp
p1 <- ggplot() +
  ggtitle("c: ice retreat") +
  theme_bw(base_size = 7, base_family = "Helvetica") +
  ylim(60, 150) +
  xlab(NULL) + ylab("Day of Year") +
  geom_point(aes(x = Year, y = DOY_ice, fill = population), shape = 21, data = combo_summary) +
  geom_line(aes(x = Year, y = DOY_ice, colour = population), data = combo_summary, linetype = "dashed") +
  geom_smooth(aes(x = Year, y = DOY_ice, group = population, colour = population), method = "lm", se = F, size = .5, data = combo_summary) +
  scale_colour_manual("",
                      values = wes_palette("Darjeeling1"),
                      breaks = c("northwest_95ud", "westice_95ud", "eastice_95ud"),
                      labels = c("Northwest Atlantic", "West Ice", "White Sea")) +
  scale_fill_manual("",
                    values = wes_palette("Darjeeling1"),
                    breaks = c("northwest_95ud", "westice_95ud", "eastice_95ud"),
                    labels = c("Northwest Atlantic", "West Ice", "White Sea"))

p2 <- ggplot() +
  ggtitle("d: spring bloom") +
  ylim(135, 175) +
  theme_bw(base_size = 7, base_family = "Helvetica") +
  xlab(NULL) + ylab(NULL) +
  geom_point(aes(x = Year, y = DOY_npp, fill = population), shape = 21, data = combo_summary) +
  geom_line(aes(x = Year, y = DOY_npp, colour = population), data = combo_summary, linetype = "dashed") +
  geom_smooth(aes(x = Year, y = DOY_npp, group = population, colour = population), method = "lm", se = F, size = .5, data = combo_summary) +
  scale_colour_manual("",
                      values = wes_palette("Darjeeling1"),
                      breaks = c("northwest_95ud", "westice_95ud", "eastice_95ud"),
                      labels = c("Northwest Atlantic", "West Ice", "White Sea")) +
  scale_fill_manual("",
                    values = wes_palette("Darjeeling1"),
                      breaks = c("northwest_95ud", "westice_95ud", "eastice_95ud"),
                      labels = c("Northwest Atlantic", "West Ice", "White Sea"))
  

p1 + p2 + plot_layout(guides='collect') &
  theme(legend.position='bottom')


# Phenology time series plot by population

p1 <- ggplot() +
  theme_bw(base_size = 7, base_family = "Helvetica") +
  ggtitle("Northwest Atlantic") +
  ylim(50, 175) +
  xlab("") + ylab("Day of Year") +
  geom_line(aes(x = Year, y = DOY_ice), data = combo_summary %>% filter(population == "northwest_95ud"), linetype = "dashed") +
  geom_point(aes(x = Year, y = DOY_ice), shape = 21, fill = "white", data = combo_summary %>% filter(population == "northwest_95ud")) +
  geom_smooth(aes(x = Year, y = DOY_ice), method = "lm", se = F, size = .5, colour = "black", data = combo_summary %>% filter(population == "northwest_95ud")) +
  geom_line(aes(x = Year, y = DOY_npp), data = combo_summary %>% filter(population == "northwest_95ud"), linetype = "dashed") +
  geom_point(aes(x = Year, y = DOY_npp), data = combo_summary %>% filter(population == "northwest_95ud")) +
  geom_smooth(aes(x = Year, y = DOY_npp), method = "lm", se = F, size = .5, colour = "black", data = combo_summary %>% filter(population == "northwest_95ud")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <- ggplot() +
  theme_bw(base_size = 7, base_family = "Helvetica") +
  ggtitle("West Ice") +
  ylim(50, 175) +
  xlab("") + ylab("Day of Year") +
  geom_line(aes(x = Year, y = DOY_ice), data = combo_summary %>% filter(population == "westice_95ud"), linetype = "dashed") +
  geom_point(aes(x = Year, y = DOY_ice), shape = 21, fill = "white", data = combo_summary %>% filter(population == "westice_95ud")) +
  geom_smooth(aes(x = Year, y = DOY_ice), method = "lm", se = F, size = .5, colour = "black", data = combo_summary %>% filter(population == "westice_95ud")) +
  geom_line(aes(x = Year, y = DOY_npp), data = combo_summary %>% filter(population == "westice_95ud"), linetype = "dashed") +
  geom_point(aes(x = Year, y = DOY_npp), data = combo_summary %>% filter(population == "westice_95ud")) +
  geom_smooth(aes(x = Year, y = DOY_npp), method = "lm", se = F, size = .5, colour = "black", data = combo_summary %>% filter(population == "westice_95ud")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p3 <- ggplot() +
  theme_bw(base_size = 7, base_family = "Helvetica") +
  ggtitle("White Sea") +
  ylim(50, 175) +
  xlab("") + ylab("Day of Year") +
  geom_line(aes(x = Year, y = DOY_ice), data = combo_summary %>% filter(population == "eastice_95ud"), linetype = "dashed") +
  geom_point(aes(x = Year, y = DOY_ice, fill = "ice retreat"), shape = 21, data = combo_summary %>% filter(population == "eastice_95ud")) +
  geom_smooth(aes(x = Year, y = DOY_ice), method = "lm", se = F, size = .5, colour = "black", data = combo_summary %>% filter(population == "eastice_95ud")) +
  geom_line(aes(x = Year, y = DOY_npp), data = combo_summary %>% filter(population == "eastice_95ud"), linetype = "dashed") +
  geom_point(aes(x = Year, y = DOY_npp, fill = "spring bloom"), shape = 21, data = combo_summary %>% filter(population == "eastice_95ud")) +
  geom_smooth(aes(x = Year, y = DOY_npp), method = "lm", se = F, size = .5, colour = "black", data = combo_summary %>% filter(population == "eastice_95ud")) +
  scale_fill_manual(name = "",
                    values = c(
                      `ice retreat` = "white",
                      `spring bloom` = "black"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top",
                      label.position = "right")) +
  theme(legend.position = c(0.4, 0.2),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

quartz(title = "Panel Plot", width = 4, height = 8)
print(p1 / p2 / p3)
dev.off()

###
### Create maps
###

# load land shapefile for plotting and clip to study region
world_shp <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
# turn off s2 errors
sf::sf_use_s2(FALSE)
# clip world shapefile to southern hemisphere
CP <- sf::st_bbox(c(xmin = -180,
                    xmax = 180,
                    ymin = 5,
                    ymax = 90), crs = 4326) %>%
  sf::st_as_sfc()
world_shp <- world_shp %>% sf::st_buffer(0) %>% sf::st_crop(CP)
world_shp <- world_shp %>% st_transform("+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")


# convert NPP and ice to tibble for plotting with ggplot
npp_df <- meanChange_npp %>% 
  rasterToPoints() %>% 
  as_tibble()
ice_df <- meanChange_ice %>% projectRaster(meanChange_npp) %>% 
  rasterToPoints() %>% 
  as_tibble()
source("~/Dropbox/git_projects/harpPup/R/discrete_gradient.R")


require(smoothr)
harpMask <- stack("~/Dropbox/git_projects/harpDist/data/harpMask_WGS84.nc")
names(harpMask) <-c("population_95ud", "northwest_95ud", "westice_95ud", "eastice_95ud",
                    "population_dens", "northwest_dens", "westice_dens", "eastice_dens")
nw_poly <- rasterToPolygons(subset(harpMask, 2), dissolve = T)
gr_poly <- rasterToPolygons(subset(harpMask, 3), dissolve = T)
ws_poly <- rasterToPolygons(subset(harpMask, 4), dissolve = T)
nw_poly <- nw_poly %>% st_as_sf() %>% smooth(method = "ksmooth", smoothness = 10)
gr_poly <- gr_poly %>% st_as_sf() %>% smooth(method = "ksmooth", smoothness = 10)
ws_poly <- ws_poly %>% st_as_sf() %>% smooth(method = "ksmooth", smoothness = 10)

library(rphylopic)
#Honk Vignette
#harp <- name_search(text = "harp seal", options = "namebankID")
#harp_img <- image_data(name_images(uuid = harp$canonicalName[1])$same[[1]]$uid, size = 1024)[[1]]

# custom map theme
# https://rpubs.com/mclaire19/ggplot2-custom-themes

p4 <- ggplot() +
  theme_minimal(base_size = 7, base_family = "Helvetica") +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black")) +
  geom_raster(aes(x = x, y = y, fill = layer), data = ice_df) + 
  geom_sf(aes(), fill = NA, colour = "white", linetype = "dashed", data = nw_poly) +
  geom_sf(aes(), fill = NA, colour = "white", linetype = "dashed", data = gr_poly) +
  geom_sf(aes(), fill = NA, colour = "white", linetype = "dashed", data = ws_poly) +
  geom_sf(aes(), colour = "grey50", fill = "grey50", data = world_shp) +
  geom_text(aes(x = -3000000, y = -2500000, label = "NA"), colour = "white") + 
  geom_text(aes(x = -500000, y = -2000000, label = "WI"), colour = "white") + 
  geom_text(aes(x = 1500000, y = -1700000, label = "WS"), colour = "white") + 
  coord_sf(xlim = c(-4030000, 2500000),
           ylim = c(-4500000, 2500000),
           crs = "+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ",
           expand = F) +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 10)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("a: ice retreat") +
  scale_fill_discrete_gradient(#name = expression("Change in Sea Ice retreat"~(d~y^{-1})),
                               name = expression("Mean Rate of Change"~(d~y^{-1})),
                               colours = pals::warmcool(20),
                               bins = 20,
                               limits = c(-10, 10),
                               breaks = seq(-10, 10, 2),
                               na.value = "black",
                               guide = guide_colourbar(nbin = 500,
                                                       raster = T,
                                                       frame.colour = "black",
                                                       ticks.colour = "black",
                                                       frame.linewidth = .25,
                                                       barwidth = .5,
                                                       barheight = 15,
                                                       direction = "vertical",
                                                       title.position = "right", #or "right"
                                                       title.theme = element_text(angle = 90,
                                                                                  hjust = 0.5,
                                                                                  size = 7))) +
  theme(legend.position = "right",
        legend.box = "vertical",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  add_phylopic(name = "harp seal", alpha = 1, x = -3000000, y = 2000000, ysize = 500000, color = "white")

p5 <- ggplot() +
  theme_minimal(base_size = 7, base_family = "Helvetica") +
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
  ggtitle("b: spring bloom") +
  scale_fill_discrete_gradient(#name = expression("Change in Spring Bloom"~(d~y^{-1}))
                              name = expression("Mean Rate of Change"~(d~y^{-1})),
                               colours = pals::warmcool(20),
                               bins = 20,
                               oob = scales::squish,
                               limits = c(-10, 10),
                               breaks = seq(-10, 10, 2),
                               na.value = "black",
                               guide = guide_colourbar(nbin = 500,
                                                       raster = T,
                                                       frame.colour = "black",
                                                       ticks.colour = "black",
                                                       frame.linewidth = .25,
                                                       barwidth = .5,
                                                       barheight = 15,
                                                       direction = "vertical",
                                                       title.position = "right", #or "right"
                                                       title.theme = element_text(angle = 90,
                                                                                  hjust = 0.5,
                                                                                  size = 7))) +
  theme(legend.position = "right",
        legend.box = "vertical",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

trial_1 <- p4 / p5
trial_2 <- p1 / p2
(trial_1) | (trial_2)

quartz(title = "Panel Plot", width = 7, height = 7)
(trial_1) | (trial_2)
quartz.save(file = "plots/example arctic change 2022-09-23.jpeg", type = "jpeg",
            dev  = dev.cur(), dpi = 500)
dev.off()

quartz(title = "Panel Plot", width = 7, height = 6.5)
(p4 + p5 + plot_layout(guides='collect')) /
  (p1 + p2 + plot_layout(guides='collect') & theme(legend.position='bottom')) +
  plot_layout(heights = c(2, 1), widths = c(1,1)) 
quartz.save(file = "plots/example arctic change 2023-09-21.jpeg", type = "jpeg",
            dev  = dev.cur(), dpi = 500)
dev.off()






# ends
