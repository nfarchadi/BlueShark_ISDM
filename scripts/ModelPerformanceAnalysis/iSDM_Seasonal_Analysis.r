# INLA seasonal iSDM
library(tidyverse)
library(raster)
library(zoo)
library(here)
library(inlabru)
library(INLA)
library(sn)
library(Metrics)
library(caret)
library(ecospat)
library(dismo)
library(sf)
library(terra)
sf_use_s2(FALSE) # need to do this to remove spherical geometry
print(bru_safe_inla())



# input GLORYS raster, scale them, and use it to make a polygon of the NWA region
GLORYS_NWA <- here("data","GLORYS", "GLORYS_2014-09.grd") %>% stack() %>% scale()
GLORYS_NWA <- dropLayer(GLORYS_NWA, c(2,3,5))
GLORYS_NWA <- terra::rast(GLORYS_NWA) # inlabru uses SpatRaster objects

NWA <- as.polygons(GLORYS_NWA > -Inf)
NWA <- NWA %>% sf::st_as_sf("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


##########
# bsh data
##########
bsh_all <- here("data","bsh_data_subset.csv") %>% 
                read.csv() 

# scale environmental data
bsh_all <- bsh_all %>%
               mutate_at(.vars = c("sst", "sst_sd", "bathy"),
                         .funs = scale)

# add season term
yq <- as.yearqtr(bsh_all$year_mon + 1/12)
bsh_all$season <- format(yq, "%q") %>% as.numeric()


##########################################################
# 5 fold cross-validation, repeated 10 times iSDM Seasonal
##########################################################
source(here("functions", "INLA_seasonal_skill.r"))

bsh_iSDM_seasonal <- INLA_seasonal_skill(dataInput = bsh_all, 
                            inla.x = c(5,6,7),
                            inla.y=1,
                            shp = NWA, 
                            k_folds = 5, repeats = 10,
                            cores = 20, n_samples = 1000)

saveRDS(bsh_iSDM_seasonal, here("bsh_iSDM_seasonal_results.rds"))