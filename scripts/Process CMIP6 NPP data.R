#####################################################
### Process NPP data for future bloom analysis... ###
#####################################################

# 2023-09-05

# load libraries
require(ncdf4)
require(raster)
require(tidyverse)

source("R/cmip6_npp_helpers.R")

# Process .nc file into individual model raster stacks
npp_canesm <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP245.nc", model = 1)
npp_cnrm <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP245.nc", model = 2)
npp_ipsl <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP245.nc", model = 3)
npp_noresm <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP245.nc", model = 4)
writeRaster(npp_canesm, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_canesm", overwrite = TRUE)
writeRaster(npp_cnrm, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_cnrm", overwrite = TRUE)
writeRaster(npp_ipsl, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_ipsl", overwrite = TRUE)
writeRaster(npp_noresm, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_noresm", overwrite = TRUE)

npp_canesm <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP585.nc", model = 1)
npp_ipsl <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP585.nc", model = 2)
npp_mriesm <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP585.nc", model = 3)
npp_noresm <- format_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP585.nc", model = 4)
writeRaster(npp_canesm, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_canesm", overwrite = TRUE)
writeRaster(npp_ipsl, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_ipsl", overwrite = TRUE)
writeRaster(npp_mriesm, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mriesm", overwrite = TRUE)
writeRaster(npp_noresm, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_noresm", overwrite = TRUE)

# calculate average
mean_npp <- mean_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP245.nc")
writeRaster(mean_npp, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean", overwrite = TRUE)

mean_npp <- mean_intpp_nc(path = "data/CMIP6 npp/daily_NPP_anomaly_SSP585.nc")
writeRaster(mean_npp, "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean", overwrite = TRUE)

# Now combine daily NPP anomaly with observations
# Can then recalculate the timing of the spring bloom
npp_1998_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_1998",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_1998_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_1998",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")

# 1999
npp_1999_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_1999",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_1999_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_1999",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2000
npp_2000_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2000",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2000_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2000",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2001
npp_2001_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2001",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2001_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2001",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2002
npp_2002_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2002",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2002_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2002",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2003
npp_2003_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2003",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2003_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2003",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2004
npp_2004_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2004",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2004_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2004",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2005
npp_2005_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2005",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2005_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2005",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2006
npp_2006_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2006",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2006_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2006",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2007
npp_2007_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2007",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2007_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2007",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2008
npp_2008_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2008",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2008_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2008",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2009
npp_2009_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2009",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2009_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2009",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2010
npp_2010_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2010",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2010_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2010",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2011
npp_2011_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2011",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2011_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2011",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2012
npp_2012_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2012",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2012_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2012",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2013
npp_2013_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2013",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2013_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2013",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2014
npp_2014_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2014",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2014_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2014",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2015
npp_2015_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2015",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2015_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2015",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2016
npp_2016_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2016",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2016_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2016",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2017
npp_2017_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2017",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2017_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2017",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2018
npp_2018_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2018",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2018_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2018",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2019
npp_2019_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2019",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2019_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2019",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")
# 2020
npp_2020_ssp245 <- future_npp(path_to_present = "data/NPP/NPP_2020",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp245_mean")
npp_2020_ssp585 <- future_npp(path_to_present = "data/NPP/NPP_2020",
                              path_to_future = "data/CMIP6 npp/CMIP6_npp_anomaly_ssp585_mean")

# write
writeRaster(npp_1998_ssp245, "data/CMIP6 NPP anomaly/npp_1998_ssp245_mean", overwrite = TRUE)
writeRaster(npp_1998_ssp585, "data/CMIP6 NPP anomaly/npp_1998_ssp585_mean", overwrite = TRUE)
writeRaster(npp_1999_ssp245, "data/CMIP6 NPP anomaly/npp_1999_ssp245_mean", overwrite = TRUE)
writeRaster(npp_1999_ssp585, "data/CMIP6 NPP anomaly/npp_1999_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2000_ssp245, "data/CMIP6 NPP anomaly/npp_2000_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2000_ssp585, "data/CMIP6 NPP anomaly/npp_2000_ssp585_mean", overwrite = TRUE)

writeRaster(npp_2001_ssp245, "data/CMIP6 NPP anomaly/npp_2001_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2001_ssp585, "data/CMIP6 NPP anomaly/npp_2001_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2002_ssp245, "data/CMIP6 NPP anomaly/npp_2002_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2002_ssp585, "data/CMIP6 NPP anomaly/npp_2002_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2003_ssp245, "data/CMIP6 NPP anomaly/npp_2003_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2003_ssp585, "data/CMIP6 NPP anomaly/npp_2003_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2004_ssp245, "data/CMIP6 NPP anomaly/npp_2004_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2004_ssp585, "data/CMIP6 NPP anomaly/npp_2004_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2005_ssp245, "data/CMIP6 NPP anomaly/npp_2005_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2005_ssp585, "data/CMIP6 NPP anomaly/npp_2005_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2006_ssp245, "data/CMIP6 NPP anomaly/npp_2006_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2006_ssp585, "data/CMIP6 NPP anomaly/npp_2006_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2007_ssp245, "data/CMIP6 NPP anomaly/npp_2007_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2007_ssp585, "data/CMIP6 NPP anomaly/npp_2007_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2008_ssp245, "data/CMIP6 NPP anomaly/npp_2008_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2008_ssp585, "data/CMIP6 NPP anomaly/npp_2008_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2009_ssp245, "data/CMIP6 NPP anomaly/npp_2009_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2009_ssp585, "data/CMIP6 NPP anomaly/npp_2009_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2010_ssp245, "data/CMIP6 NPP anomaly/npp_2010_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2010_ssp585, "data/CMIP6 NPP anomaly/npp_2010_ssp585_mean", overwrite = TRUE)

writeRaster(npp_2011_ssp245, "data/CMIP6 NPP anomaly/npp_2011_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2011_ssp585, "data/CMIP6 NPP anomaly/npp_2011_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2012_ssp245, "data/CMIP6 NPP anomaly/npp_2012_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2012_ssp585, "data/CMIP6 NPP anomaly/npp_2012_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2013_ssp245, "data/CMIP6 NPP anomaly/npp_2013_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2013_ssp585, "data/CMIP6 NPP anomaly/npp_2013_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2014_ssp245, "data/CMIP6 NPP anomaly/npp_2014_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2014_ssp585, "data/CMIP6 NPP anomaly/npp_2014_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2015_ssp245, "data/CMIP6 NPP anomaly/npp_2015_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2015_ssp585, "data/CMIP6 NPP anomaly/npp_2015_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2016_ssp245, "data/CMIP6 NPP anomaly/npp_2016_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2016_ssp585, "data/CMIP6 NPP anomaly/npp_2016_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2017_ssp245, "data/CMIP6 NPP anomaly/npp_2017_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2017_ssp585, "data/CMIP6 NPP anomaly/npp_2017_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2018_ssp245, "data/CMIP6 NPP anomaly/npp_2018_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2018_ssp585, "data/CMIP6 NPP anomaly/npp_2018_ssp585_mean", overwrite = TRUE)
writeRaster(npp_2019_ssp245, "data/CMIP6 NPP anomaly/npp_2019_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2019_ssp585, "data/CMIP6 NPP anomaly/npp_2019_ssp585_mean", overwrite = TRUE)

writeRaster(npp_2020_ssp245, "data/CMIP6 NPP anomaly/npp_2020_ssp245_mean", overwrite = TRUE)
writeRaster(npp_2020_ssp585, "data/CMIP6 NPP anomaly/npp_2020_ssp585_mean", overwrite = TRUE)

