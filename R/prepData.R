###########################################################
### Function to filter the harp seal telemetry data set ###
###########################################################

# Select animals that will be useful for the resource tracking approach

# Base this assessment on:
# Numbers of locations transmitted
# Minimum length of deployment
# Minimum travel distance

# drop tags that transmitted for less than 2 weeks
# drop tags that transmitted fewer than 40 locations in total
# drop animals that travelled less than 300 km

prepData <- function(dat){
  
  # hp6-L752-19 was recovered dead
  # remove locations transmitted from dead animal on beach
  # based on haul out from dive summary data
  dat <- dat %>% filter(case_when(id == "hp6-L752-19" ~ date < "2019-07-17",
                                  id != "hp6-L752-19" ~ date > min(date))) 

  # remove any locations before tag attachment
  on <- tibble(id = c("hp6-755-19", "hp6-L002-19", "hp6-749-19", "hp6-747-19",
                      "hp6-L752-19", "hp6-L004-19", "hp6-751-19", "hp6-L003-19",
                      "hp6-753-19", "hp6-756-19", "168553", "168556"),
               start = c("2019-03-21", "2019-03-21", "2019-03-21", "2019-03-21",
                         "2019-03-21", "2019-03-21", "2019-03-25", "2019-03-25",
                         "2019-03-25", "2019-03-25", "2019-03-25", "2019-03-25"))

  dat <- dat %>% filter(case_when(id == "hp6-L752-19" ~ date < "2019-07-17",
                                  id != "hp6-L752-19" ~ date > min(date))) 
  
  dat <- dat %>%
    group_by(id) %>%
    left_join(on) %>%
    filter(case_when(is.na(start) == F ~ date > start,
                     is.na(start) == T ~ date > min(date))) %>% 
    ungroup() %>%
    dplyr::select(-start)

  # drop hp3 seals as they were released in November
  dat <- dat %>% filter(id != "hp3-Notag-16")
  dat <- dat %>% filter(id != "hp3-Skinny-14")
  dat <- dat %>% filter(id != "hp3-Yellow-16")
  
  # append year day for filtering
  dat <- dat %>% mutate(day = lubridate::yday(date))
  
  # trim hp4-W716-17 to only locations before August
  # after this there's a gap in transmission of around a month
  dat <- dat %>% filter(case_when(id == "hp4-W716-17" ~ day < 201,
                                  id != "hp4-W716-17" ~ day > 0)) 
  
  # drop hp5 as they were released in July
  dat <- dat %>% filter(id != "hp5-L764-18")
  dat <- dat %>% filter(id != "hp5-L766-18")

  # Remove tags that didn't transmit well
  # These will produce rubbish data so not worth including
  # calculate travel distance and tagging duration
  dat <- dat %>%
    group_by(id) %>%
    mutate(displacement = sp::spDistsN1(pts = cbind(lon, lat),
                                        pt = cbind(first(lon), first(lat)),
                                        longlat = T),
           at_large = as.numeric(difftime(date, first(date), units = "days"))) %>% 
    ungroup()
  
  # create summary data frame from which to filter the tag ids
  dat_sum <- dat %>%
    group_by(id) %>%
    summarise(displacement = max(displacement, na.rm = T),
              at_large = max(at_large, na.rm = T),
              n = n(),
              ratio = n/at_large)
  
  ids <- dat_sum %>% 
    filter(displacement > 300) %>% 
    filter(at_large > 14) %>%
    filter(ratio > 1) %>%
    pull(id)
  
  dat <- dat %>% filter(id %in% ids)
  
  # include other pre-processing steps from previous analysis...
  # starting locations for some animals seem to mess up simulated tracks
  # drop the locations obviously on land so the simulated tracks start at sea...
  dat <- dat %>% 
    group_by(id) %>% 
    slice(which(!(id == "hp1c-196-96" & row_number() == 1))) %>%
    slice(which(!(id == "hp1c-396-96" & row_number() == 1))) %>%
    slice(which(!(id == "hp2-9320-04" & row_number() == 1))) %>%
    slice(which(!(id == "hp2-9349-04" & row_number() == 1))) %>%
    slice(which(!(id == "hp2b-01-99" & row_number() == 1))) %>%
    ungroup()
  
  # drop the first 2 locations from hp1c-596-96 as it's on land
  dat <- dat %>% 
    group_by(id) %>% 
    slice(which(!(id == "hp1c-596-96" & row_number() < 3))) %>%
    ungroup()
  
  # drop the first 5 locations from hp1c-496-96
  dat <- dat %>% 
    group_by(id) %>%
    slice(which(!(id == "hp1c-496-96" & row_number() < 6))) %>%
    ungroup()
  
  # drop first two rows from 168553
  # locations are not where animals were tagged
  # and throw simulations off by biasing them south
  dat <- dat %>% 
    group_by(id) %>% 
    slice(which(!(id == "168553" & row_number() == 2))) %>%
    slice(which(!(id == "168553" & row_number() == 1))) %>%
    ungroup()
  
  # drop a couple of points from hp2-9321-04
  # locations throw step length estimation off
  # and simulations biased them east
  dat <- dat %>% 
    group_by(id) %>% 
    slice(which(!(id == "hp2-9321-04" & row_number() == 5))) %>%
    slice(which(!(id == "hp2-9321-04" & row_number() == 3))) %>%
    ungroup()
  
  # drop a couple of points from hp4-M492-17
  # locations throw step length estimation off
  # and simulations biased
  dat <- dat %>% 
    group_by(id) %>% 
    slice(which(!(id == "hp4-M492-17" & row_number() == 2))) %>%
    slice(which(!(id == "hp4-M492-17" & row_number() == 1))) %>%
    ungroup()
  
  # add pseudo-locations to hp4-W715-17 that prevent foieGras generating weird loop in track
  dat <- dat %>% 
    add_row(id = "hp4-W715-17",
            date = seq(from = ymd_hms("2017-05-14 21:40:43"), to = ymd_hms("2017-06-17 16:19:01"), length = 10)[2:9],
            lc = rep("B", 8),
            lon = seq(from = -10.9, to = -11.9, length = 10)[2:9],
            lat = seq(from = 68.5, to = 74.5, length = 10)[2:9],
            smaj = rep(6014, 8),
            smin = rep(288, 8),
            eor = rep(89, 8)) %>%
    arrange(id, date)
  
  # filter hp1c-196-96 to remove strange loop past Svalbard
  sub <- dat |> filter(id == "hp1c-196-96")
  sub_m <- fit_ssm(sub,
                   model = "rw",
                   time.step = 96,
                   vmax = 4)
  sub_f <- sub_m |> grab("fitted", as_sf = F)
  sub_f <- sub_f |> mutate(lc = "G")
  sub_f <- sub_f |> dplyr::select(id, date, lc, lon,  lat)
  
  dat <- dat |> filter(id != "hp1c-196-96") # remove hp1c from data
  dat <- dat |> bind_rows(sub_f)           # append filtered data
  
  dat <- dat |> arrange(id, date)
  
  return(dat)
}


