##################################
### Re-work CMIP6 sea ice data ###
##################################

# 2023-08-18

# append climate anomaly sea ice concentration onto the observations
# then recalculate the timing of the sea ice retreat...

# need to first calculate sea ice anomaly...
# double check years for ice against NPP values...
# 1995-2014
# 2081-2100

# load libraries
require(ncdf4)
require(raster)
require(tidyverse)
require(lubridate)

source("R/cmip6_ice_mean.R")
source("R/process_NSIDC.R")

# list files
filenames <- list.files("data/CMIP6 ice/hist/", full.names = T)
fn_hist <- filenames[76:95]
filenames <- list.files("data/CMIP6 ice/ssp245/", full.names = T)
fn_ssp245 <- filenames[67:86]
filenames <- list.files("data/CMIP6 ice/ssp585/", full.names = T)
fn_ssp585 <- filenames[67:86]

ice_hist <- cmip6_ice_mean(fn_hist)
ice_ssp245 <- cmip6_ice_mean(fn_ssp245)
ice_ssp585 <- cmip6_ice_mean(fn_ssp585)

# calculate daily ice anomaly
ssp245_anom <- ice_ssp245 - ice_hist 
ssp585_anom <- ice_ssp585 - ice_hist

# once anomaly is calculated then sort longitudes ready to apply to NSIDC observations...
ssp245_anom <- shiftRotate(ssp245_anom)
ssp585_anom <- shiftRotate(ssp585_anom)
names(ssp245_anom) <- 1:245
names(ssp585_anom) <- 1:245

ice_hist <- shiftRotate(ice_hist)
ice_ssp245 <- shiftRotate(ice_ssp245)
ice_ssp585 <- shiftRotate(ice_ssp585)

ice_hist_phen <- ice_phenology(ice_hist)
ice_ssp245_phen <- ice_phenology(ice_ssp245)
ice_ssp585_phen <- ice_phenology(ice_ssp585)

plot(ice_hist_phen)
plot(ice_ssp245_phen)
plot(ice_ssp585_phen)

par(mfrow = c(3, 1))
plot(ice_hist_phen)
plot(ice_ssp245_phen)
plot(ice_ssp585_phen)

ice_hist_phen2 <- ice_hist_phen
ice_ssp245_phen2 <- ice_ssp245_phen
ice_ssp585_phen2 <- ice_ssp585_phen


# make everything in hist and ssp245 that is NA 365
# then do maths
# then take all NAs in ssp585 and convert back to NAs
ice_hist_phen[is.na(ice_hist_phen)] <- 365
ice_ssp245_phen[is.na(ice_ssp245_phen)] <- 365
# phenology anomaly
ssp245_anom_phen <- ice_ssp245_phen - ice_hist_phen 
# put land and permanent ice back
ssp245_anom_phen[is.na(ice_ssp245_phen2)] <- NA
plot(ssp245_anom_phen)
ssp585_anom_phen <- ice_ssp585_phen - ice_hist_phen
plot(ssp585_anom_phen)

# save raster ouputs
writeRaster(ssp245_anom, "data/CMIP6 ice/CMIP6_ice_anomaly_ssp245_mean")
writeRaster(ssp585_anom, "data/CMIP6 ice/CMIP6_ice_anomaly_ssp585_mean")

writeRaster(ssp245_anom_phen, "data/CMIP6 ice/CMIP6_ice_anomaly_ssp245_phenology_incl_retreat")
writeRaster(ssp585_anom_phen, "data/CMIP6 ice/CMIP6_ice_anomaly_ssp585_phenology_incl_retreat")


# then need new function that takes daily NSIDC data and appends anomaly to it...
# processing takes a long time
# download NSDIC data first and then process it
# less of an issue on new computer
nsidc_1995 <- process_NSIDC(year = 1995)
writeRaster(nsidc_1995, "data/NSIDC/daily_ice_1995_v2")
rm(nsidc_1995)
nsidc_1996 <- process_NSIDC(year = 1996)
writeRaster(nsidc_1996, "data/NSIDC/daily_ice_1996")
rm(nsidc_1996)
nsidc_1997 <- process_NSIDC(year = 1997)
writeRaster(nsidc_1997, "data/NSIDC/daily_ice_1997")
rm(nsidc_1997)
nsidc_1998 <- process_NSIDC(year = 1998)
writeRaster(nsidc_1998, "data/NSIDC/daily_ice_1998")
rm(nsidc_1998)
nsidc_1999 <- process_NSIDC(year = 1999)
writeRaster(nsidc_1999, "data/NSIDC/daily_ice_1999")
rm(nsidc_1999)

nsidc_2000 <- process_NSIDC(year = 2000)
writeRaster(nsidc_2000, "data/NSIDC/daily_ice_2000")
rm(nsidc_2000)
nsidc_2001 <- process_NSIDC(year = 2001)
writeRaster(nsidc_2001, "data/NSIDC/daily_ice_2001")
rm(nsidc_2001)
nsidc_2002 <- process_NSIDC(year = 2002)
writeRaster(nsidc_2002, "data/NSIDC/daily_ice_2002")
rm(nsidc_2002)
nsidc_2003 <- process_NSIDC(year = 2003)
writeRaster(nsidc_2003, "data/NSIDC/daily_ice_2003")
rm(nsidc_2003)
nsidc_2004 <- process_NSIDC(year = 2004)
writeRaster(nsidc_2004, "data/NSIDC/daily_ice_2004")
rm(nsidc_2004)
nsidc_2005 <- process_NSIDC(year = 2005)
writeRaster(nsidc_2005, "data/NSIDC/daily_ice_2005")
rm(nsidc_2005)
nsidc_2006 <- process_NSIDC(year = 2006)
writeRaster(nsidc_2006, "data/NSIDC/daily_ice_2006")
rm(nsidc_2006)
nsidc_2007 <- process_NSIDC(year = 2007)
writeRaster(nsidc_2007, "data/NSIDC/daily_ice_2007")
rm(nsidc_2007)
nsidc_2008 <- process_NSIDC(year = 2008)
writeRaster(nsidc_2008, "data/NSIDC/daily_ice_2008")
rm(nsidc_2008)
nsidc_2009 <- process_NSIDC(year = 2009)
writeRaster(nsidc_2009, "data/NSIDC/daily_ice_2009")
rm(nsidc_2009)

nsidc_2010 <- process_NSIDC(year = 2010)
writeRaster(nsidc_2010, "data/NSIDC/daily_ice_2010")
rm(nsidc_2010)
nsidc_2011 <- process_NSIDC(year = 2011)
writeRaster(nsidc_2011, "data/NSIDC/daily_ice_2011")
rm(nsidc_2011)
nsidc_2012 <- process_NSIDC(year = 2012)
writeRaster(nsidc_2012, "data/NSIDC/daily_ice_2012")
rm(nsidc_2012)
nsidc_2013 <- process_NSIDC(year = 2013)
writeRaster(nsidc_2013, "data/NSIDC/daily_ice_2013")
rm(nsidc_2013)
nsidc_2014 <- process_NSIDC(year = 2014)
writeRaster(nsidc_2014, "data/NSIDC/daily_ice_2014")
rm(nsidc_2014)
nsidc_2015 <- process_NSIDC(year = 2015)
writeRaster(nsidc_2015, "data/NSIDC/daily_ice_2015")
rm(nsidc_2015)
nsidc_2016 <- process_NSIDC(year = 2016)
writeRaster(nsidc_2016, "data/NSIDC/daily_ice_2016")
rm(nsidc_2016)
nsidc_2017 <- process_NSIDC(year = 2017)
writeRaster(nsidc_2017, "data/NSIDC/daily_ice_2017")
rm(nsidc_2017)
nsidc_2018 <- process_NSIDC(year = 2018)
writeRaster(nsidc_2018, "data/NSIDC/daily_ice_2018")
rm(nsidc_2018)
nsidc_2019 <- process_NSIDC(year = 2019)
writeRaster(nsidc_2019, "data/NSIDC/daily_ice_2019")
rm(nsidc_2019)
nsidc_2020 <- process_NSIDC(year = 2020)
writeRaster(nsidc_2020, "data/NSIDC/daily_ice_2020")
rm(nsidc_2020)

###
### Don't need to combine end of century ice with each year
### Just calculate phenology of sea ice retreat
###

for(i in 1:length(fn)){
  daily_ice <- stack(fn[i])
  ice_pheno <- ice_phenology(daily_ice)
  writeRaster(ice_pheno,
              paste0("data/NSIDC/ice_phenology_", substr(fn[i], nchar(fn[i])-3, nchar(fn[i]))))
}


####################################
### Generate climatology rasters ###
####################################

# load ice
ice_phenology_clim <- stack("data/NSIDC/ice_phenology_1995",
                            "data/NSIDC/ice_phenology_1996",
                            "data/NSIDC/ice_phenology_1997",
                            "data/NSIDC/ice_phenology_1998", 
                            "data/NSIDC/ice_phenology_1999",
                            "data/NSIDC/ice_phenology_2000",
                            "data/NSIDC/ice_phenology_2001",
                            "data/NSIDC/ice_phenology_2002",
                            "data/NSIDC/ice_phenology_2003",
                            "data/NSIDC/ice_phenology_2004",
                            "data/NSIDC/ice_phenology_2005",
                            "data/NSIDC/ice_phenology_2006",
                            "data/NSIDC/ice_phenology_2007",
                            "data/NSIDC/ice_phenology_2008",
                            "data/NSIDC/ice_phenology_2009",
                            "data/NSIDC/ice_phenology_2010",
                            "data/NSIDC/ice_phenology_2011",
                            "data/NSIDC/ice_phenology_2012",
                            "data/NSIDC/ice_phenology_2013",
                            "data/NSIDC/ice_phenology_2014",
                            "data/NSIDC/ice_phenology_2015",
                            "data/NSIDC/ice_phenology_2016",
                            "data/NSIDC/ice_phenology_2017",
                            "data/NSIDC/ice_phenology_2018",
                            "data/NSIDC/ice_phenology_2019",
                            "data/NSIDC/ice_phenology_2020",
                            bands = 1)

meanIce <- calc(ice_phenology_clim, mean, na.rm = T)
plot(meanIce)
writeRaster(meanIce, "data/NSIDC/ice_phenology_climatology", overwrite = T)

# BUT #
# if a cell remains ice covered some years and not in others
# this will be the average of when the ice leaves only
# replace NAs with 365 so that the average includes permanent ice years
ice_phenology_clim_2 <- ice_phenology_clim
ice_phenology_clim[is.na(ice_phenology_clim)] <- 365
meanIce <- calc(ice_phenology_clim, mean, na.rm = F)
meanIce[meanIce == 365] <- NA
plot(meanIce)

writeRaster(meanIce, "data/NSIDC/ice_phenology_climatology_v2", overwrite = T)


### now work through each year
### add the anomaly
### recalculate timing
### output ready for extraction
# ssp245_anom <- stack("data/CMIP6 ice/CMIP6_ice_anomaly_ssp245_mean")
# projection(ssp245_anom) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# 
# ssp585_anom <- stack("data/CMIP6 ice/CMIP6_ice_anomaly_ssp585_mean")
# projection(ssp585_anom) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# 
# # for each year...
# in_out <- function(in_path, out_path){
#   daily_ice <- stack(in_path) # load ice
#   daily_ice <- subset(daily_ice, 1:245)
#   daily_ice_ssp245 <- format_future(daily_ice, ssp245_anom) # append anomaly
#   daily_ice_ssp585 <- format_future(daily_ice, ssp585_anom)
#   ice_phen_origin <- ice_phenology(daily_ice) # calculate ice retreat timing
#   ice_phen_ssp245 <- ice_phenology(daily_ice_ssp245)
#   ice_phen_ssp585 <- ice_phenology(daily_ice_ssp585)
#   out <- stack(ice_phen_origin, ice_phen_ssp245, ice_phen_ssp585) # stack
#   names(out) <- c("origin", "ssp245", "ssp585")
#   writeRaster(out, out_path) # output
# }
# 
# in_out(in_path = "data/NSIDC/daily_ice_1995",
#        out_path = "data/NSIDC/ice_phenology_1995")
# 
# in_out(in_path = "data/NSIDC/daily_ice_1996",
#        out_path = "data/NSIDC/ice_phenology_1996")
# 
# in_out(in_path = "data/NSIDC/daily_ice_1997",
#        out_path = "data/NSIDC/ice_phenology_1997")
# 
# in_out(in_path = "data/NSIDC/daily_ice_1998",
#        out_path = "data/NSIDC/ice_phenology_1998")
# 
# in_out(in_path = "data/NSIDC/daily_ice_1999",
#        out_path = "data/NSIDC/ice_phenology_1999")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2000",
#        out_path = "data/NSIDC/ice_phenology_2000")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2001",
#        out_path = "data/NSIDC/ice_phenology_2001")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2002",
#        out_path = "data/NSIDC/ice_phenology_2002")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2003",
#        out_path = "data/NSIDC/ice_phenology_2003")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2004",
#        out_path = "data/NSIDC/ice_phenology_2004")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2005",
#        out_path = "data/NSIDC/ice_phenology_2005")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2006",
#        out_path = "data/NSIDC/ice_phenology_2006")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2007",
#        out_path = "data/NSIDC/ice_phenology_2007")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2008",
#        out_path = "data/NSIDC/ice_phenology_2008")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2009",
#        out_path = "data/NSIDC/ice_phenology_2009")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2010",
#        out_path = "data/NSIDC/ice_phenology_2010")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2011",
#        out_path = "data/NSIDC/ice_phenology_2011")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2012",
#        out_path = "data/NSIDC/ice_phenology_2012")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2013",
#        out_path = "data/NSIDC/ice_phenology_2013")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2014",
#        out_path = "data/NSIDC/ice_phenology_2014")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2015",
#        out_path = "data/NSIDC/ice_phenology_2015")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2016",
#        out_path = "data/NSIDC/ice_phenology_2016")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2017",
#        out_path = "data/NSIDC/ice_phenology_2017")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2018",
#        out_path = "data/NSIDC/ice_phenology_2018")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2019",
#        out_path = "data/NSIDC/ice_phenology_2019")
# 
# in_out(in_path = "data/NSIDC/daily_ice_2020",
#        out_path = "data/NSIDC/ice_phenology_2020")
# ends
