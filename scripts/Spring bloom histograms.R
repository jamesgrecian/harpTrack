#########################################################################
### Test differences in NPP between ice and non-ice associated blooms ###
#########################################################################

# revised with updated foieGras output 2022-07-21
# revised with updated aniMotum and NPP data 2023-09-06

# load libraries
require(tidyverse)
require(lubridate)
require(patchwork)
require(raster)
require(sf)

source("R/extract_npp_bloom.R")

# load data with covariates
dat <- readRDS("data/harp_locs_routed_covariates_20230906.rds")
dat <- dat %>% filter(doy >= 60) # drop locations before 1st March

# use local function to append NPP value for contemporaneous NPP spring bloom
dat <- extract_phenology_bloom(df = dat)

# append climatology
fn <- list.files("data/revised NPP", pattern = "bloom", full.names = T)
fn <- fn %>% substr(1, nchar(fn)-4) %>% unique()
npp_phenology_bloom <- stack(fn)
meanBloom <- calc(npp_phenology_bloom, mean, na.rm = T)
sdBloom <- calc(npp_phenology_bloom, sd, na.rm = T)

# project lon lat and extract climatology values
dat[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(dat[c("lon", "lat")]), projection(meanBloom)))
dat$bloomMean <- NA
dat$bloomSD <- NA
dat <- dat %>% mutate(bloomMean = raster::extract(meanBloom, cbind(X, Y)),
                      bloomSD = raster::extract(sdBloom, cbind(X, Y)))
dat <- dat %>% dplyr::select(-c("X", "Y"))

# append factor for ice vs non-ice
# 0 when influenced by ice
# 1 when not influenced by ice
dat <- dat %>% mutate(ice_infl = factor(as.numeric(ice_doy == 0)))

# compare with t test
t.test(bloomValue ~ ice_infl, data = dat %>% filter(g < .5))
t.test(bloomMean ~ ice_infl, data = dat %>% filter(g < .5))
t.test(bloomSD ~ ice_infl, data = dat %>% filter(g < .5))

# try the independence permutation test from Ryan & Hindell's Nature paper
nrow(dat %>% filter(g < .5))
require(coin)
independence_test(formula = bloomValue ~ ice_infl,
                  data = dat %>% filter(!is.na(bloomValue)) %>% filter(g < .5),
                  distribution = approximate(nresample = 10000))

independence_test(formula = bloomMean ~ ice_infl,
                  data = dat %>% filter(!is.na(bloomMean)) %>% filter(g < .5),
                  distribution = approximate(nresample = 10000))

independence_test(formula = bloomSD ~ ice_infl,
                  data = dat %>% filter(!is.na(bloomSD)) %>% filter(g < .5),
                  distribution = approximate(nresample = 10000))

# summary statistics
dat %>% 
  filter(!is.na(bloomValue)) %>%
  filter(g < .5) %>%
  group_by(ice_infl) %>%
  dplyr::select(bloomValue) %>%
  summarise(mean(bloomValue),
            sd(bloomValue))

dat %>%
  filter(!is.na(bloomMean)) %>%
  filter(g < .5) %>%
  group_by(ice_infl) %>%
  dplyr::select(bloomMean) %>%
  summarise(mean(bloomMean),
            sd(bloomMean))

dat %>%
  filter(!is.na(bloomSD)) %>%
  filter(g < .5) %>%
  group_by(ice_infl) %>%
  dplyr::select(bloomSD) %>% 
  summarise(mean(bloomSD),
            sd(bloomSD))

##################
### Bean plots ###
##################

base_plot <- ggplot() +
  theme_classic(base_size = 10) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  xlab(expression(Net~Primary~Productivity~(mg~C~m^{-2}~d^{-1}))) +
  scale_fill_manual(name = "",
                    values = c(`non-ice influenced` = "#6FC16C",
                               `ice influenced` = "#8BBCD8"),
                    guide = guide_legend(direction = "vertical",
                                         title.position = "top",
                                         label.position = "right")) +
  scale_colour_manual(name = "",
                      values = c(`non-ice influenced` = "#6FC16C",
                                 `ice influenced` = "#8BBCD8"),
                      guide = guide_legend(direction = "vertical",
                                           title.position = "top",
                                           label.position = "right")) +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

# 0 when influenced by ice
# 1 when not influenced by ice

p1 <- base_plot + 
  geom_density(aes(x = bloomValue, y = ..scaled.., fill = "non-ice influenced", colour = "non-ice influenced"),
               data = dat %>% filter(ice_infl == 1) %>% filter(g < .5)) +
  geom_density(aes(x = bloomValue, y = -..scaled.., fill = "ice influenced", colour = "ice influenced"),
               data = dat %>% filter(ice_infl == 0) %>% filter(g < .5)) +
  coord_flip(xlim = c(0, 2000), ylim = c(-1.1, 1.1), expand = F) +
  scale_x_continuous(limits = c(0, 2000)) + # avoid truncation
  ylab("Contemporaneous") +
  ggtitle("A")

p2 <- base_plot +
  geom_density(aes(x = bloomMean, y = ..scaled.., fill = "non-ice influenced", colour = "non-ice influenced"),
               data = dat %>% filter(ice_infl == 1) %>% filter(g < .5)) +
  geom_density(aes(x = bloomMean, y = -..scaled.., fill = "ice influenced", colour = "ice influenced"),
               data = dat %>% filter(ice_infl == 0) %>% filter(g < .5)) +
  coord_flip(xlim = c(0, 2000), ylim = c(-1.1, 1.1), expand = F) +
  scale_x_continuous(limits = c(0, 2000)) + # avoid truncation
  ylab("Climatological Mean") +
  ggtitle("B")

p3 <- base_plot +
  geom_density(aes(x = bloomSD, y = ..scaled.., fill = "non-ice influenced", colour = "non-ice influenced"),
               data = dat %>% filter(ice_infl == 1) %>% filter(g < .5)) +
  geom_density(aes(x = bloomSD, y = -..scaled.., fill = "ice influenced", colour = "ice influenced"),
               data = dat %>% filter(ice_infl == 0) %>% filter(g < .5)) +
  coord_flip(xlim = c(0, 800), ylim = c(-1.1, 1.1), expand = F) +
  scale_x_continuous(limits = c(0, 800)) + # avoid truncation
  ylab("Climatological SD") +
  ggtitle("C")

quartz(title = "Panel Plot", width = 8, height = 4.5)
print(p1 + p2 + p3)
quartz.save(file = "plots/spring bloom bean plots 2022-07-22.jpeg", type = "jpeg",
            dev  = dev.cur(), dpi = 500)
dev.off()

# ends
