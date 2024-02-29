# Fitting each model with the full data 

library(tidyverse, lib.loc = "~/bsh_ISDM/R_packages")
library(raster, lib.loc = "~/bsh_ISDM/R_packages")
library(zoo, lib.loc = "~/bsh_ISDM/R_packages")
library(here, lib.loc = "~/bsh_ISDM/R_packages")
library(dismo, lib.loc = "~/bsh_ISDM/R_packages")
library(Metrics, lib.loc = "~/bsh_ISDM/R_packages")
library(caret, lib.loc = "~/bsh_ISDM/R_packages")
library(inlabru, lib.loc = "~/bsh_ISDM/R_packages")
library(INLA, lib.loc = "~/bsh_ISDM/R_packages")
library(sn, lib.loc = "~/bsh_ISDM/R_packages")
library(dismo, lib.loc = "~/bsh_ISDM/R_packages")
library(fmesher, lib.loc = "~/bsh_ISDM/R_packages")
library(sf)
library(terra)
sf_use_s2(FALSE) # need to do this to remove spherical geometry
source(here("scripts", "functions","collinearity.r")) # find out how here works on the farm
print(bru_safe_inla())

# input GLORYS climotology rasters, scale them, and use it to make a polygon of the NWA region
# Not as fine as other NWA shapefile which is better for making mesh in INLA
GLORYS_NWA <- here("data","GLORYS", "GLORYS_clim.grd") %>% stack() %>% scale()
GLORYS_NWA <- dropLayer(GLORYS_NWA, c(2,3,5))
#GLORYS_NWA <- projectRaster(GLORYS_NWA, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=laea +lat_0=32.5 +lon_0=-52.5 +x_0=0 +y_0=0 +units=km +no_defs +ellps=WGS84"
GLORYS_NWA <- terra::rast(GLORYS_NWA) # inlabru uses SpatRaster objects

NWA <- as.polygons(GLORYS_NWA > -Inf)
NWA <- NWA %>% sf::st_as_sf("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


### first making all the data pres:abs ratio 1:1 --- using the same method as the BRT script

##########
# bsh etag
##########
etag <- here("data","bsh_data","bsh_etag_enhanced.rds") %>% # or here("data","bsh_data","bsh_etag_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry() %>% unique()


# getting the data in a 1:1 ratio by year_mon
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per year_mon
pres_n <- etag %>% 
        filter(pres_abs == 1) %>% group_by(year_mon) %>% 
        summarise(count = n()) %>% slice(rep(1:n(), each = 2)) %>% 
        mutate(pres_abs = rep(c(0,1),length(etag$year_mon %>% unique())))

# create a nested dataframe
nested_etag <- etag %>%
              group_by(year_mon,pres_abs) %>% 
              nest() %>% 
              ungroup() %>%
              left_join(.,pres_n, by = c("year_mon","pres_abs"))
  
# sample by n for each nested group
sampled_etag <- nested_etag %>%
  mutate(samp = map2(data, count, sample_n))

# unnest the dataframe back
etag <- sampled_etag %>%
  dplyr::select(-data) %>%
  unnest(samp) %>% 
  mutate(pres_abs = as.integer(pres_abs)) %>% 
  mutate(dataset = "etag")




############
# bsh marker
############
marker <- here("data","bsh_data","bsh_marker_enhanced.rds") %>% # or here("data","bsh_data","bsh_marker_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry() %>% unique()
                

# getting the data in a 1:1 ratio by year_mon
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per year_mon
pres_n <- marker %>% 
        filter(pres_abs == 1) %>% group_by(year_mon) %>% 
        summarise(count = n()) %>% slice(rep(1:n(), each = 2)) %>% 
        mutate(pres_abs = rep(c(0,1),length(marker$year_mon %>% unique())))

# create a nested dataframe
nested_marker <- marker %>%
              group_by(year_mon,pres_abs) %>% 
              nest() %>% 
              ungroup() %>%
              left_join(.,pres_n, by = c("year_mon","pres_abs"))
  
# sample by n for each nested group
sampled_marker <- nested_marker %>%
  mutate(samp = map2(data, count, sample_n))

# unnest the dataframe back
marker<-sampled_marker %>% 
  dplyr::select(-data) %>%
  unnest(samp) %>% 
  mutate(pres_abs = as.integer(pres_abs)) %>% 
  mutate(dataset = "marker")


##############
# bsh observer
##############
observer <- here("data","bsh_data","bsh_observer_enhanced.rds") %>%
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry() %>% unique()

# getting the data in a 1:1 ratio year_mon
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per year_mon
pres_n <- observer %>% 
        filter(pres_abs == 1) %>% group_by(year_mon) %>% 
        summarise(count = n()) %>% slice(rep(1:n(), each = 2)) %>%
        mutate(pres_abs = rep(c(0,1),length(observer$year_mon %>% unique())))

# create a nested dataframe
nested_observer <- observer %>%
              group_by(year_mon,pres_abs) %>% 
              nest() %>% 
              ungroup() %>%
              left_join(.,pres_n, by = c("year_mon","pres_abs")) %>% na.omit()
  
# sample by n for each nested group
sampled_observer <- nested_observer %>%
  mutate(samp = map2(data, count, sample_n))

# unnest the dataframe back
observer <- sampled_observer %>% 
  dplyr::select(-data) %>%
  unnest(samp) %>% 
  mutate(pres_abs = as.integer(pres_abs)) %>% 
  mutate(dataset = "observer")


### Combine
bsh_all <- rbind(etag %>% dplyr::select(-c(longitudeError, latitudeError)), marker, observer)

# scale environmental data for ISDM models
bsh_all_scaled <- bsh_all %>%
               mutate_at(.vars = c("sst", "sss", "ssh", "mld", "log_eke", "sst_sd", "ssh_sd",
                                   "sss_sd", "bathy", "rugosity"),
                         .funs = scale)

# add season term
yq <- as.yearqtr(bsh_all_scaled$year_mon + 1/12)
bsh_all_scaled$season <- format(yq, "%q") %>% as.numeric()

# checking for collinearity 
collinearity(na.omit(bsh_all %>% as.data.frame() %>% dplyr::select(sst:rugosity))) #remove sss, ssh, and log_eke maybe? --- I get the same result if I do it by dataset too

# ##############
# # BRT ensemble
# ##############
# source(here("scripts","functions", "BRT_ensemble_fullmodel.r"))

# bsh_ensemble <- BRT_ensemble_fullmodel(dataInput = bsh_all, 
#                             gbm.x = c(8,13,16), # make sure this is right!
#                             gbm.y=1, learning.rate = 0.005, 
#                             bag.fraction = 0.75, tree.complexity = 5,
#                             n_trees = 2000)

# saveRDS(bsh_ensemble, here("results","BRT_ensemble_fullmodel.rds"))
# print("BRT ensemble done")

# #############
# # BRT pooling
# #############
# source(here("scripts","functions", "BRT_pooling_fullmodel.r"))

# bsh_pooling <- BRT_pooling_fullmodel(dataInput = bsh_all, 
#                             gbm.x = c(8,13,16), # make sure this is right!
#                             gbm.y=1, learning.rate = 0.005, 
#                             bag.fraction = 0.75, tree.complexity = 5,
#                             n_trees = 2000)

# saveRDS(bsh_pooling, here("results","BRT_pooling_fullmodel.rds"))
# print("BRT pooling done")

# ##############
# # ISDM Spatial
# ##############
# # this code will just retrieve and save the data for the marginal effects (ME) and GMRF because model is too big to save

# source(here("scripts","functions", "INLA_spatial_fullmodel.r"))

# bsh_ISDM_spatial <- INLA_spatial_fullmodel(dataInput = bsh_all_scaled, 
#                             inla.x = c(8,13,16), # make sure this is right! 
#                             inla.y=1,
#                             shp = NWA,
#                             cores = 20, n_samples = 1000)


# saveRDS(bsh_ISDM_spatial, here("results","bsh_ISDM_spatial_ME_GMRF.rds"))

#####################
# ISDM Spatiotemporal
#####################
# this code will just retrieve and save the data for the marginal effects (ME) and GMRF because model is too big to save

source(here("scripts","functions", "INLA_spatiotemporal_fullmodel.r"))

bsh_ISDM_spatiotemporal <- INLA_spatiotemporal_fullmodel(dataInput = bsh_all_scaled, 
                            inla.x = c(8,13,16), # make sure this is right!
                            inla.y=1,
                            shp = NWA,
                            cores = 20, n_samples = 1000)

saveRDS(bsh_ISDM_spatiotemporal, here("results","bsh_ISDM_spatiotemporal_ME_GMRF.rds"))