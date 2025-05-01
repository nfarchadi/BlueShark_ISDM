# Fitting each model with the full data 

library(tidyverse)
library(raster)
library(zoo)
library(here)
library(dismo)
library(Metrics)
library(caret)
library(inlabru)
library(INLA)
library(sn)
library(dismo)
library(fmesher)
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

# scale environmental data for iSDMs
bsh_all_scaled <- bsh_all %>%
               mutate_at(.vars = c("sst", "sst_sd", "bathy"),
                         .funs = scale)

# add season term
yq <- as.yearqtr(bsh_all_scaled$year_mon + 1/12)
bsh_all_scaled$season <- format(yq, "%q") %>% as.numeric()



##############
# BRT ensemble
##############
source(here("functions", "BRT_ensemble_fullmodel.r"))

bsh_ensemble <- BRT_ensemble_fullmodel(dataInput = bsh_all, 
                            gbm.x = c(5,6,7),
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            n_trees = 2000)

saveRDS(bsh_ensemble, here("BRT_ensemble_fullmodel.rds"))


#############
# BRT pooling
#############
source(here("functions", "BRT_pooling_fullmodel.r"))

bsh_pooling <- BRT_pooling_fullmodel(dataInput = bsh_all, 
                            gbm.x = c(5,6,7), 
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            n_trees = 2000)

saveRDS(bsh_pooling, here("BRT_pooling_fullmodel.rds"))


###############
# iSDM Constant
###############
# this code will just retrieve and save the data for the marginal effects (ME), GMRF, and spatial predictions because model is too big to save

source(here("functions", "INLA_constant_fullmodel.r"))

bsh_ISDM_constant <- INLA_constant_fullmodel(dataInput = bsh_all_scaled, 
                            inla.x = c(5,6,7), 
                            inla.y=1,
                            shp = NWA,
                            cores = 20, n_samples = 1000,
                            env_data = GLORYS_NWA)


saveRDS(bsh_ISDM_constant, here("bsh_ISDM_constant_ME_GMRF_preds.rds"))

###############
# iSDM seasonal
###############
# this code will just retrieve and save the data for the marginal effects (ME), GMRF, and spatial predictions because model is too big to save

source(here("functions", "INLA_seasonal_fullmodel.r"))

bsh_ISDM_seasonal <- INLA_seasonal_fullmodel(dataInput = bsh_all_scaled, 
                            inla.x = c(5,6,7), 
                            inla.y=1,
                            shp = NWA,
                            cores = 20, n_samples = 1000,
                            env_data = GLORYS_NWA)

saveRDS(bsh_ISDM_seasonal, here("bsh_ISDM_seasonal_ME_GMRF_preds.rds"))