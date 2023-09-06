######################################################################################
### Loop through the files from Jo and convert to single phenology raster per year ###
######################################################################################

# files are MATLAB format v7.3
# use raveio package to import
#install.packages("raveio")
require(raveio)
require(tidyverse)
require(raster)
require(sf)
library(stringr)

# load function to convert mat files to day rasters
# create folder manually...
source("R/mat_to_raster.R")

mat_to_raster(fn = "1998_S.mat")
mat_to_raster(fn = "1999_S.mat")
mat_to_raster(fn = "2000_S.mat")
mat_to_raster(fn = "2001_S.mat")
mat_to_raster(fn = "2002_S.mat")
mat_to_raster(fn = "2003_A.mat")
mat_to_raster(fn = "2004_A.mat")
mat_to_raster(fn = "2005_A.mat")
mat_to_raster(fn = "2006_A.mat")
mat_to_raster(fn = "2007_A.mat")
mat_to_raster(fn = "2008_A.mat")
mat_to_raster(fn = "2009_A.mat")
mat_to_raster(fn = "2010_A.mat")
mat_to_raster(fn = "2011_A.mat")
mat_to_raster(fn = "2012_A.mat")
mat_to_raster(fn = "2013_A.mat")
mat_to_raster(fn = "2014_A.mat")
mat_to_raster(fn = "2015_A.mat")
mat_to_raster(fn = "2016_A.mat")
mat_to_raster(fn = "2017_A.mat")
mat_to_raster(fn = "2018_A.mat")
mat_to_raster(fn = "2019_A.mat")
mat_to_raster(fn = "2020_A.mat")

# generate phenology rasters based on the 5% above median rule from Jo Hopkins
source("R/NPP_phenology_5percent.R")

NPP_phenology_5percent(fn = "NPP_1998")
NPP_phenology_5percent(fn = "NPP_1999")
NPP_phenology_5percent(fn = "NPP_2000")
NPP_phenology_5percent(fn = "NPP_2001")
NPP_phenology_5percent(fn = "NPP_2002")
NPP_phenology_5percent(fn = "NPP_2003")
NPP_phenology_5percent(fn = "NPP_2004")
NPP_phenology_5percent(fn = "NPP_2005")

NPP_phenology_5percent(fn = "NPP_2006")
NPP_phenology_5percent(fn = "NPP_2007")
NPP_phenology_5percent(fn = "NPP_2008")
NPP_phenology_5percent(fn = "NPP_2009")
NPP_phenology_5percent(fn = "NPP_2010")
NPP_phenology_5percent(fn = "NPP_2011")
NPP_phenology_5percent(fn = "NPP_2012")
NPP_phenology_5percent(fn = "NPP_2013")
NPP_phenology_5percent(fn = "NPP_2014")
NPP_phenology_5percent(fn = "NPP_2015")

NPP_phenology_5percent(fn = "NPP_2016")
NPP_phenology_5percent(fn = "NPP_2017")
NPP_phenology_5percent(fn = "NPP_2018")
NPP_phenology_5percent(fn = "NPP_2019")
NPP_phenology_5percent(fn = "NPP_2020")

# For climatology comparison need the actual NPP values associated with the 5% idea
# use which median function to identify day of year/ layer associated with peak...
# pull NPP value associated with that cell
source("R/NPP_bloom_5percent.R")

NPP_bloom_5percent(fn = "NPP_1998")
NPP_bloom_5percent(fn = "NPP_1999")
NPP_bloom_5percent(fn = "NPP_2000")
NPP_bloom_5percent(fn = "NPP_2001")
NPP_bloom_5percent(fn = "NPP_2002")
NPP_bloom_5percent(fn = "NPP_2003")
NPP_bloom_5percent(fn = "NPP_2004")
NPP_bloom_5percent(fn = "NPP_2005")
NPP_bloom_5percent(fn = "NPP_2006")
NPP_bloom_5percent(fn = "NPP_2007")
NPP_bloom_5percent(fn = "NPP_2008")

NPP_bloom_5percent(fn = "NPP_2009")
NPP_bloom_5percent(fn = "NPP_2010")
NPP_bloom_5percent(fn = "NPP_2011")
NPP_bloom_5percent(fn = "NPP_2012")
NPP_bloom_5percent(fn = "NPP_2013")
NPP_bloom_5percent(fn = "NPP_2014")
NPP_bloom_5percent(fn = "NPP_2015")
NPP_bloom_5percent(fn = "NPP_2016")
NPP_bloom_5percent(fn = "NPP_2017")
NPP_bloom_5percent(fn = "NPP_2018")
NPP_bloom_5percent(fn = "NPP_2019")
NPP_bloom_5percent(fn = "NPP_2020")

####################################
### Generate climatology rasters ###
####################################

# load NPP
npp_phenology_clim <- stack("data/NPP/NPP_1998_phenology_5percent",
                            "data/NPP/NPP_1999_phenology_5percent",
                            "data/NPP/NPP_2000_phenology_5percent",
                            "data/NPP/NPP_2001_phenology_5percent",
                            "data/NPP/NPP_2002_phenology_5percent",
                            "data/NPP/NPP_2003_phenology_5percent",
                            "data/NPP/NPP_2004_phenology_5percent",
                            "data/NPP/NPP_2005_phenology_5percent",
                            "data/NPP/NPP_2006_phenology_5percent",
                            "data/NPP/NPP_2007_phenology_5percent",
                            "data/NPP/NPP_2008_phenology_5percent",
                            "data/NPP/NPP_2009_phenology_5percent",
                            "data/NPP/NPP_2010_phenology_5percent",
                            "data/NPP/NPP_2011_phenology_5percent",
                            "data/NPP/NPP_2012_phenology_5percent",
                            "data/NPP/NPP_2013_phenology_5percent",
                            "data/NPP/NPP_2014_phenology_5percent",
                            "data/NPP/NPP_2015_phenology_5percent",
                            "data/NPP/NPP_2016_phenology_5percent",
                            "data/NPP/NPP_2017_phenology_5percent",
                            "data/NPP/NPP_2018_phenology_5percent",
                            "data/NPP/NPP_2019_phenology_5percent",
                            "data/NPP/NPP_2020_phenology_5percent")

meanPhen <- calc(npp_phenology_clim, mean, na.rm = T)
plot(meanPhen)
writeRaster(meanPhen, "data/NPP/npp_phenology_climatology", overwrite = T)


npp_bloom_clim <- stack("data/NPP/NPP_1998_phenology_bloom",
                        "data/NPP/NPP_1999_phenology_bloom",
                        "data/NPP/NPP_2000_phenology_bloom",
                        "data/NPP/NPP_2001_phenology_bloom",
                        "data/NPP/NPP_2002_phenology_bloom",
                        "data/NPP/NPP_2003_phenology_bloom",
                        "data/NPP/NPP_2004_phenology_bloom",
                        "data/NPP/NPP_2005_phenology_bloom",
                        "data/NPP/NPP_2006_phenology_bloom",
                        "data/NPP/NPP_2007_phenology_bloom",
                        "data/NPP/NPP_2008_phenology_bloom",
                        "data/NPP/NPP_2009_phenology_bloom",
                        "data/NPP/NPP_2010_phenology_bloom",
                        "data/NPP/NPP_2011_phenology_bloom",
                        "data/NPP/NPP_2012_phenology_bloom",
                        "data/NPP/NPP_2013_phenology_bloom",
                        "data/NPP/NPP_2014_phenology_bloom",
                        "data/NPP/NPP_2015_phenology_bloom",
                        "data/NPP/NPP_2016_phenology_bloom",
                        "data/NPP/NPP_2017_phenology_bloom",
                        "data/NPP/NPP_2018_phenology_bloom",
                        "data/NPP/NPP_2019_phenology_bloom",
                        "data/NPP/NPP_2020_phenology_bloom")

meanBloom <- calc(npp_bloom_clim, mean, na.rm = T)
sdBloom <- calc(npp_bloom_clim, sd, na.rm = T)

bloom_out <- stack(meanBloom, sdBloom)
names(bloom_out) <- c("meanBloom", "sdBloom")

writeRaster(bloom_out, "data/NPP/npp_bloom_climatology", overwrite = T)

# ends