# Fig. Spatial Predictions for each model

library(here)
library(raster)
library(tidyverse)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean) 
library(gbm)
library(sf) 
world <- ne_countries(scale = "medium", returnclass = "sf")

# load in environmental data for July, 2019
env_stack <- here("data", "GLORYS", "GLORYS_2014-09.grd") %>% stack()

# only need sst, sst_sd, & Bathymetry
env_stack <- subset(env_stack, c(1,6,9))

#########
# Pooling
#########
# load in BRT pooling model
BRT_pooling <- here("results", "FullModel", "BRT_pooling_fullmodel.rds") %>% readRDS()

# prediction
pred_pooling <- raster::predict(env_stack, BRT_pooling, n.trees = BRT_pooling$gbm.call$best.trees, type = "response")



##########
# Ensemble
##########
# load in BRT pooling model
BRT_ensemble <- here("results", "FullModel", "BRT_ensemble_fullmodel.rds") %>% readRDS()
BRT_ensemble.marker <- BRT_ensemble$brt.marker
BRT_ensemble.observer <- BRT_ensemble$brt.observer
BRT_ensemble.etag <- BRT_ensemble$brt.etag


# prediction
pred.marker <- raster::predict(env_stack, BRT_ensemble.marker, n.trees = BRT_ensemble.marker$gbm.call$best.trees, type = "response")
pred.observer <- raster::predict(env_stack, BRT_ensemble.observer, n.trees = BRT_ensemble.observer$gbm.call$best.trees, type = "response")
pred.etag <- raster::predict(env_stack, BRT_ensemble.etag, n.trees = BRT_ensemble.etag$gbm.call$best.trees, type = "response")

# averaging the predictions
pred_ensemble <- mean(pred.marker, pred.observer, pred.etag)



###############
# iSDM Constant
###############

# INLA model results
ISDM_spatial <- here("results", "FullModel", "bsh_ISDM_spatial_ME_GMRF_preds_norm_withintercepts.rds") %>% readRDS()
ISDM_spatial_preds <- ISDM_spatial[[3]]
ISDM_spatial_preds <- ISDM_spatial_preds %>% 
    mutate(x = st_coordinates(ISDM_spatial_preds)[,1],
        y = st_coordinates(ISDM_spatial_preds)[,2]) %>%
    dplyr::select(x,y,mean) %>% 
    st_drop_geometry()
ISDM_spatial[[4]]
ISDM_spatial[[5]] %>% pull(range) %>% mean()
ISDM_spatial[[6]] %>% pull(log.variance) %>% mean()

###############
# iSDM Seasonal
###############

# INLA model results
ISDM_spatiotemporal <- here("results", "FullModel", "bsh_ISDM_spatiotemporal_ME_GMRF_preds_withintercepts.rds") %>% readRDS()
ISDM_spatiotemporal_preds <- ISDM_spatiotemporal[[3]]
ISDM_spatiotemporal_preds <- ISDM_spatiotemporal_preds %>% 
    mutate(x = st_coordinates(ISDM_spatiotemporal_preds)[,1],
           y = st_coordinates(ISDM_spatiotemporal_preds)[,2]) %>%
    dplyr::select(x,y,mean) %>% 
    st_drop_geometry()
ISDM_spatiotemporal[[4]]
ISDM_spatiotemporal[[5]] %>% pull(range) %>% mean()
ISDM_spatiotemporal[[6]] %>% pull(log.variance) %>% mean()

##################################
# Plotting the Spatial Predictions
##################################
pred_pooling <- raster::rasterToPoints(pred_pooling) %>% 
    as.data.frame() %>% 
    mutate(model = "Pooling") %>% 
    rename("Prediction" = "layer")

pred_ensemble <- raster::rasterToPoints(pred_ensemble) %>% 
    as.data.frame() %>% 
    mutate(model = "Ensemble") %>% 
    rename("Prediction" = "layer")

preds_iSDM_Constant <- ISDM_spatial_preds %>% 
    mutate(model = "iSDM Constant") %>% 
    rename("Prediction" = "mean")
    
preds_iSDM_Seasonal <- ISDM_spatiotemporal_preds %>% 
    mutate(model = "iSDM Seasonal") %>% 
    rename("Prediction" = "mean")

combine_pred <- rbind(pred_pooling, pred_ensemble, preds_iSDM_Constant, preds_iSDM_Seasonal) %>% 
    mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal")))

res <- 1

# need to get bsh_all or bsh_all_scaled from other scripts
bsh_Sep_2014_presence <- bsh_all_scaled %>% mutate(year_mon = zoo::as.yearmon(year_mon)) %>% 
    # filter(year_mon == "Sep 2014"& pres_abs == 1) %>%
    filter(month == 9 & pres_abs == 1) %>%
    mutate(dataset = case_when(dataset == "etag" ~ "Electronic Tag",
                                dataset == "marker" ~ "Marker",
                                dataset == "observer" ~ "Observer"),
            dataset = factor(dataset, levels = c("Marker", "Observer", "Electronic Tag"))) %>% 
    mutate(lon = floor(lon/res) * res + 0.5 * res,
           lat = floor(lat/res) * res + 0.5 * res) %>%
    group_by(lon,lat) %>% 
    summarise(presence = sum(pres_abs, na.rm = T), .groups = "drop")

bsh_Sep_2014_absence <- bsh_all_scaled %>% mutate(year_mon = zoo::as.yearmon(year_mon)) %>% 
    filter(year_mon == "Sep 2014" & pres_abs == 0) %>%
    mutate(dataset = case_when(dataset == "etag" ~ "Electronic Tag",
                                dataset == "marker" ~ "Marker",
                                dataset == "observer" ~ "Observer"),
            dataset = factor(dataset, levels = c("Marker", "Observer", "Electronic Tag")),
            pres_abs = as.factor(pres_abs))

combine_pred_plot <- combine_pred %>% #filter(model %in% c("iSDM Constant")) %>% 
ggplot() + 
geom_raster(aes(x = x, y = y, fill = Prediction)) +
geom_tile(data = bsh_Sep_2014_presence, aes(x = lon, y = lat), fill = NA, color = 'darkorchid1', size = 0.5) + 
# geom_point(data = bsh_Sep_2014_absence, aes(x = lon, y = lat), shape = 3, color = "grey") +
# geom_point(data = bsh_Sep_2014 %>% filter(pres_abs == 1), aes(x = lon, y = lat, shape = dataset), color = "darkorchid1") +
# geom_point(data = bsh_Sep_2014 %>% filter(pres_abs == 0), aes(x = lon, y = lat), shape = 3, color = "grey") +
geom_sf(data = world, color = "black", fill = "grey") +
coord_sf(xlim = c(-100, -5), ylim = c(10, 56), expand = FALSE) +
theme_bw() +
theme(legend.position = "bottom") +
guides(fill = guide_colorbar(title.position = "bottom"),
       shape = guide_legend(title.position = "bottom", title.hjust = 0.5, title.vjust = -6)) +
facet_wrap(~model, nrow = 2) +
theme(strip.text = element_text(size = 15),
      strip.background = element_blank()) +
cmocean::scale_fill_cmocean(name = "haline", limits = c(0,1), breaks = c(0, 0.5, 1)) + 
labs(x = "", y = "", fill = "Habitat Suitability") +
guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                             title.position = "bottom", legend.position = "bottom",
                             barwidth = 15, barheight = 1.5, title.hjust = 0.5))

ggsave(here("plots","ModelPredictions.png"),
       width = 10, height = 8, units = "in", dpi = 300)





###----Downsampled---###


#########
# Pooling
#########
# load in BRT pooling model
BRT_pooling <- here("results", "FullModel", "BRT_pooling_fullmodel_downsampled.rds") %>% readRDS()

# prediction
pred_pooling <- raster::predict(env_stack, BRT_pooling, n.trees = BRT_pooling$gbm.call$best.trees, type = "response")



##########
# Ensemble
##########
# load in BRT pooling model
BRT_ensemble <- here("results", "FullModel", "BRT_ensemble_fullmodel_downsampled.rds") %>% readRDS()
BRT_ensemble.marker <- BRT_ensemble$brt.marker
BRT_ensemble.observer <- BRT_ensemble$brt.observer
BRT_ensemble.etag <- BRT_ensemble$brt.etag


# prediction
pred.marker <- raster::predict(env_stack, BRT_ensemble.marker, n.trees = BRT_ensemble.marker$gbm.call$best.trees, type = "response")
pred.observer <- raster::predict(env_stack, BRT_ensemble.observer, n.trees = BRT_ensemble.observer$gbm.call$best.trees, type = "response")
pred.etag <- raster::predict(env_stack, BRT_ensemble.etag, n.trees = BRT_ensemble.etag$gbm.call$best.trees, type = "response")

# averaging the predictions
pred_ensemble <- mean(pred.marker, pred.observer, pred.etag)



###############
# iSDM Constant
###############

# INLA model results
ISDM_spatial <- here("results", "FullModel", "bsh_ISDM_spatial_ME_GMRF_preds_norm_withintercepts_downsampled.rds") %>% readRDS()
ISDM_spatial_preds <- ISDM_spatial[[3]]
ISDM_spatial_preds <- ISDM_spatial_preds %>% 
    mutate(x = st_coordinates(ISDM_spatial_preds)[,1],
        y = st_coordinates(ISDM_spatial_preds)[,2]) %>%
    dplyr::select(x,y,mean) %>% 
    st_drop_geometry()
ISDM_spatial[[4]]
ISDM_spatial[[5]] %>% pull(range) %>% mean()
ISDM_spatial[[6]] %>% pull(log.variance) %>% mean()

###############
# iSDM Seasonal
###############

# INLA model results
ISDM_spatiotemporal <- here("results", "FullModel", "bsh_ISDM_spatiotemporal_ME_GMRF_preds_withintercepts_downsampled.rds") %>% readRDS()
ISDM_spatiotemporal_preds <- ISDM_spatiotemporal[[3]]
ISDM_spatiotemporal_preds <- ISDM_spatiotemporal_preds %>% 
    mutate(x = st_coordinates(ISDM_spatiotemporal_preds)[,1],
           y = st_coordinates(ISDM_spatiotemporal_preds)[,2]) %>%
    dplyr::select(x,y,mean) %>% 
    st_drop_geometry()
ISDM_spatiotemporal[[4]]
ISDM_spatiotemporal[[5]] %>% pull(range) %>% mean()
ISDM_spatiotemporal[[6]] %>% pull(log.variance) %>% mean()

##################################
# Plotting the Spatial Predictions
##################################
pred_pooling <- raster::rasterToPoints(pred_pooling) %>% 
    as.data.frame() %>% 
    mutate(model = "Pooling") %>% 
    rename("Prediction" = "layer")

pred_ensemble <- raster::rasterToPoints(pred_ensemble) %>% 
    as.data.frame() %>% 
    mutate(model = "Ensemble") %>% 
    rename("Prediction" = "layer")

preds_iSDM_Constant <- ISDM_spatial_preds %>% 
    mutate(model = "iSDM Constant") %>% 
    rename("Prediction" = "mean")
    
preds_iSDM_Seasonal <- ISDM_spatiotemporal_preds %>% 
    mutate(model = "iSDM Seasonal") %>% 
    rename("Prediction" = "mean")

combine_pred <- rbind(pred_pooling, pred_ensemble, preds_iSDM_Constant, preds_iSDM_Seasonal) %>% 
    mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal")))

res <- 1

# need to get bsh_all or bsh_all_scaled from other scripts
# MAKE SURE TO USE DOWNSAMPLED DATA HERE #
bsh_Sep_2014_presence <- bsh_all_scaled %>% mutate(year_mon = zoo::as.yearmon(year_mon)) %>% 
    # filter(year_mon == "Sep 2014"& pres_abs == 1) %>%
    filter(month == 9 & pres_abs == 1) %>%
    mutate(dataset = case_when(dataset == "etag" ~ "Electronic Tag",
                                dataset == "marker" ~ "Marker",
                                dataset == "observer" ~ "Observer"),
            dataset = factor(dataset, levels = c("Marker", "Observer", "Electronic Tag"))) %>% 
    mutate(lon = floor(lon/res) * res + 0.5 * res,
           lat = floor(lat/res) * res + 0.5 * res) %>%
    group_by(lon,lat) %>% 
    summarise(presence = sum(pres_abs, na.rm = T), .groups = "drop")

bsh_Sep_2014_absence <- bsh_all_scaled %>% mutate(year_mon = zoo::as.yearmon(year_mon)) %>% 
    filter(year_mon == "Sep 2014" & pres_abs == 0) %>%
    mutate(dataset = case_when(dataset == "etag" ~ "Electronic Tag",
                                dataset == "marker" ~ "Marker",
                                dataset == "observer" ~ "Observer"),
            dataset = factor(dataset, levels = c("Marker", "Observer", "Electronic Tag")),
            pres_abs = as.factor(pres_abs))

combine_pred_plot <- combine_pred %>% #filter(model %in% c("iSDM Constant")) %>% 
ggplot() + 
geom_raster(aes(x = x, y = y, fill = Prediction)) +
geom_tile(data = bsh_Sep_2014_presence, aes(x = lon, y = lat), fill = NA, color = 'darkorchid1', size = 0.5) + 
# geom_point(data = bsh_Sep_2014_absence, aes(x = lon, y = lat), shape = 3, color = "grey") +
# geom_point(data = bsh_Sep_2014 %>% filter(pres_abs == 1), aes(x = lon, y = lat, shape = dataset), color = "darkorchid1") +
# geom_point(data = bsh_Sep_2014 %>% filter(pres_abs == 0), aes(x = lon, y = lat), shape = 3, color = "grey") +
geom_sf(data = world, color = "black", fill = "grey") +
coord_sf(xlim = c(-100, -5), ylim = c(10, 56), expand = FALSE) +
theme_bw() +
theme(legend.position = "bottom") +
guides(fill = guide_colorbar(title.position = "bottom"),
       shape = guide_legend(title.position = "bottom", title.hjust = 0.5, title.vjust = -6)) +
facet_wrap(~model, nrow = 2) +
theme(strip.text = element_text(size = 15),
      strip.background = element_blank()) +
cmocean::scale_fill_cmocean(name = "haline", limits = c(0,1), breaks = c(0, 0.5, 1)) + 
labs(x = "", y = "", fill = "Habitat Suitability") +
guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                             title.position = "bottom", legend.position = "bottom",
                             barwidth = 15, barheight = 1.5, title.hjust = 0.5))

ggsave(here("plots","ModelPredictions_downsampled.png"),
       width = 10, height = 8, units = "in", dpi = 300)
