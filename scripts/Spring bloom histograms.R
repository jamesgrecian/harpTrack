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

# load data with covariates
dat <- readRDS("data/harp_locs_routed_covariates_20230907.rds")

# load NPP bloom climatology
climBloom <- stack("data/NPP/npp_bloom_climatology")

# project lon lat and extract climatology values
dat[c("X", "Y")] <- as_tibble(rgdal::project(as.matrix(dat[c("lon", "lat")]), projection(climBloom)))
dat <- dat |> mutate(BloomMean = raster::extract(climBloom, cbind(X, Y), layer = 1, nl = 1),
                      BloomSD = raster::extract(climBloom, cbind(X, Y), layer = 2, nl = 1))
dat <- dat |> dplyr::select(-c("X", "Y"))

# append factor for ice vs non-ice
# 0 when influenced by ice
# 1 when not influenced by ice
dat <- dat |> mutate(IceInfluence = factor(as.numeric(IceDay == 0)))

# try the independence permutation test from Ryan & Hindell's Nature paper
nrow(dat |> filter(g < .5))
require(coin)
independence_test(formula = BloomValue ~ IceInfluence,
                  data = dat |> drop_na(BloomValue) |> drop_na(IceInfluence) |> filter(g < .5),
                  distribution = approximate(nresample = 100000))

independence_test(formula = BloomMean ~ IceInfluence,
                  data = dat |> drop_na(BloomMean) |> drop_na(IceInfluence) |> filter(g < .5),
                  distribution = approximate(nresample = 100000))

# summary statistics
dat |> 
  drop_na(BloomValue) |>
  drop_na(IceDay) |>
  filter(g < .5) |>
  group_by(IceInfluence) |>
  dplyr::select(BloomValue) |>
  summarise(mean(BloomValue),
            sd(BloomValue))

dat |>
  drop_na(BloomMean) |>
  drop_na(IceDay) |>
  filter(g < .5) |>
  group_by(IceInfluence) |>
  dplyr::select(BloomMean) |>
  summarise(mean(BloomMean),
            sd(BloomMean))

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
               data = dat |> filter(ice_infl == 1) |> filter(g < .5)) +
  geom_density(aes(x = bloomValue, y = -..scaled.., fill = "ice influenced", colour = "ice influenced"),
               data = dat |> filter(ice_infl == 0) |> filter(g < .5)) +
  coord_flip(xlim = c(0, 2000), ylim = c(-1.1, 1.1), expand = F) +
  scale_x_continuous(limits = c(0, 2000)) + # avoid truncation
  ylab("Contemporaneous") +
  ggtitle("A")

p2 <- base_plot +
  geom_density(aes(x = bloomMean, y = ..scaled.., fill = "non-ice influenced", colour = "non-ice influenced"),
               data = dat |> filter(ice_infl == 1) |> filter(g < .5)) +
  geom_density(aes(x = bloomMean, y = -..scaled.., fill = "ice influenced", colour = "ice influenced"),
               data = dat |> filter(ice_infl == 0) |> filter(g < .5)) +
  coord_flip(xlim = c(0, 2000), ylim = c(-1.1, 1.1), expand = F) +
  scale_x_continuous(limits = c(0, 2000)) + # avoid truncation
  ylab("Climatological Mean") +
  ggtitle("B")

p3 <- base_plot +
  geom_density(aes(x = bloomSD, y = ..scaled.., fill = "non-ice influenced", colour = "non-ice influenced"),
               data = dat |> filter(ice_infl == 1) |> filter(g < .5)) +
  geom_density(aes(x = bloomSD, y = -..scaled.., fill = "ice influenced", colour = "ice influenced"),
               data = dat |> filter(ice_infl == 0) |> filter(g < .5)) +
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
