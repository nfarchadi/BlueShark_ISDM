# BRT ensemble and pooling

library(tidyverse, lib.loc = "~/bsh_ISDM/R_packages")
library(raster, lib.loc = "~/bsh_ISDM/R_packages")
library(zoo, lib.loc = "~/bsh_ISDM/R_packages")
library(here, lib.loc = "~/bsh_ISDM/R_packages")
library(dismo, lib.loc = "~/bsh_ISDM/R_packages")
library(Metrics, lib.loc = "~/bsh_ISDM/R_packages")
library(caret, lib.loc = "~/bsh_ISDM/R_packages")
library(sf)
sf_use_s2(FALSE) # need to do this to remove spherical geometry
source(here("scripts", "functions","collinearity.r")) # find out how here works on the farm

# load NWA shapefile
NWA <- here("data","shapefiles","NWA.shp") %>% 
        sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

### first making all the data pres:abs ratio 1:1 and using climotological env data

##########
# bsh etag
##########
etag <- here("data","bsh_data","bsh_etag_enhanced_clim.rds") %>% # or here("data","bsh_data","bsh_etag_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()
            

# getting the data in a 1:1 ratio by day 
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per day
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
etag<-sampled_etag %>% 
  dplyr::select(-data) %>%
  unnest(samp) %>% 
  mutate(pres_abs = as.integer(pres_abs)) %>% 
  mutate(dataset = "etag")




############
# bsh marker
############
marker <- here("data","bsh_data","bsh_marker_enhanced_clim.rds") %>% # or here("data","bsh_data","bsh_marker_enhanced_seasonal.rds")
            readRDS() %>%
            sf::st_drop_geometry()
                

# getting the data in a 1:1 ratio by day 
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per day
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
observer <- here("data","bsh_data","bsh_observer_enhanced_clim.rds") %>%
            readRDS() %>%
            sf::st_drop_geometry()
                

# getting the data in a 1:1 ratio by day 
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per day
pres_n <- observer %>% 
        filter(pres_abs == 1) %>% group_by(year_mon) %>% 
        summarise(count = n()) %>% slice(rep(1:n(), each = 2)) %>% 
        mutate(pres_abs = rep(c(0,1),length(observer$year_mon %>% unique())))

# create a nested dataframe
nested_observer <- observer %>%
              group_by(year_mon,pres_abs) %>% 
              nest() %>% 
              ungroup() %>%
              left_join(.,pres_n, by = c("year_mon","pres_abs"))
  
# sample by n for each nested group
sampled_observer <- nested_observer %>%
  mutate(samp = map2(data, count, sample_n))

# unnest the dataframe back
observer<-sampled_observer %>% 
  dplyr::select(-data) %>%
  unnest(samp) %>% 
  mutate(pres_abs = as.integer(pres_abs)) %>% 
  mutate(dataset = "observer")


### Combine
bsh_all <- rbind(etag %>% dplyr::select(-c(longitudeError, latitudeError)), marker, observer)

# checking for collinearity 
collinearity(na.omit(bsh_all %>% as.data.frame() %>% dplyr::select(sst:rugosity))) #remove sss, ssh, and log_eke maybe? --- I get the same result if I do it by dataset too


#########################################################
# 2 fold cross-validation, repeated 10 times BRT ensemble
#########################################################
source(here("scripts","functions", "BRT_ensemble_skill.r"))

bsh_ensemble <- BRT_ensemble_skill(dataInput = bsh_all, 
                            gbm.x = c(8,11,13:17), 
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            k_folds = 2, repeats = 10,
                            n_trees = 2000)

saveRDS(bsh_ensemble, here("results","BRT_ensemble_skill_clim.rds"))



########################################################
# 2 fold cross-validation, repeated 10 times BRT pooling
########################################################
source(here("scripts","functions", "BRT_pooling_skill.r"))

bsh_pooling <- BRT_pooling_skill(dataInput = bsh_all, 
                            gbm.x = c(8,11,13:17), 
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            k_folds = 2, repeats = 10,
                            n_trees = 2000)

saveRDS(bsh_pooling, here("results","BRT_pooling_skill_clim.rds"))










### Same as above but now on the seasonal env data

##########
# bsh etag
##########
etag <- here("data","bsh_data","bsh_etag_enhanced_seasonal.rds") %>%
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()
            

# getting the data in a 1:1 ratio by day 
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per day
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
etag<-sampled_etag %>% 
  dplyr::select(-data) %>%
  unnest(samp) %>% 
  mutate(pres_abs = as.integer(pres_abs)) %>% 
  mutate(dataset = "etag")




############
# bsh marker
############
marker <- here("data","bsh_data","bsh_marker_enhanced_seasonal.rds") %>%
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()
                

# getting the data in a 1:1 ratio by day 
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per day
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
observer <- here("data","bsh_data","bsh_observer_enhanced_seasonal.rds") %>%  
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()
                

# getting the data in a 1:1 ratio by day 
set.seed(24)
# setting the number of absences to be sampled to be the same as the number of presences per day
pres_n <- observer %>% 
        filter(pres_abs == 1) %>% group_by(year_mon) %>% 
        summarise(count = n()) %>% slice(rep(1:n(), each = 2)) %>% 
        mutate(pres_abs = rep(c(0,1),length(observer$year_mon %>% unique())))

# create a nested dataframe
nested_observer <- observer %>%
              group_by(year_mon,pres_abs) %>% 
              nest() %>% 
              ungroup() %>%
              left_join(.,pres_n, by = c("year_mon","pres_abs"))
  
# sample by n for each nested group
sampled_observer <- nested_observer %>%
  mutate(samp = map2(data, count, sample_n))

# unnest the dataframe back
observer<-sampled_observer %>% 
  dplyr::select(-data) %>%
  unnest(samp) %>% 
  mutate(pres_abs = as.integer(pres_abs)) %>% 
  mutate(dataset = "observer")


### Combine
bsh_all <- rbind(etag %>% dplyr::select(-c(longitudeError, latitudeError)), marker, observer)

# checking for collinearity 
collinearity(na.omit(bsh_all %>% as.data.frame() %>% dplyr::select(sst:rugosity))) #remove sss, ssh, and log_eke maybe? --- I get the same result if I do it by dataset too


#########################################################
# 2 fold cross-validation, repeated 10 times BRT ensemble
#########################################################
source(here("scripts","functions", "BRT_ensemble_skill.r"))

bsh_ensemble <- BRT_ensemble_skill(dataInput = bsh_all, 
                            gbm.x = c(8,11,13:17), 
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            k_folds = 2, repeats = 10,
                            n_trees = 2000)

saveRDS(bsh_ensemble, here("results","BRT_ensemble_skill_seasonal.rds"))



########################################################
# 2 fold cross-validation, repeated 10 times BRT pooling
########################################################
source(here("scripts","functions", "BRT_pooling_skill.r"))

bsh_pooling <- BRT_pooling_skill(dataInput = bsh_all, 
                            gbm.x = c(8,11,13:17), 
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            k_folds = 2, repeats = 10,
                            n_trees = 2000)

saveRDS(bsh_pooling, here("results","BRT_pooling_skill_seasonal.rds"))