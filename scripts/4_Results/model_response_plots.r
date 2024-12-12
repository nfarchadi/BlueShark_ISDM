# Fig. Response curves of each environmental variable from each model

library(here)
library(tidyverse)
library(patchwork)

###----response plots----####

#All BRT and INLA models
BRT_ensemble <- here("results", "FullModel", "BRT_ensemble_fullmodel.rds") %>% readRDS()
BRT_pooling <- here("results", "FullModel", "BRT_pooling_fullmodel.rds") %>% readRDS()
ISDM_spatial <- here("results", "FullModel", "bsh_ISDM_spatial_ME_GMRF_preds_norm_withintercepts.rds") %>% readRDS()
ISDM_spatiotemporal <- here("results", "FullModel", "bsh_ISDM_spatiotemporal_ME_GMRF_preds_withintercepts.rds") %>% readRDS()

##############
# BRT ensemble
##############

# etags #
var<-BRT_ensemble$brt.etag$var.names
response_data_etag<-data.frame()

for (i in 1:length(var)){
response_plot_data_etag <- gbm::plot.gbm(BRT_ensemble$brt.etag,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_etag<-response_plot_data_etag %>% gather("variable","x",-y)

response_data_etag<-rbind(response_data_etag, response_plot_data_etag)

}

response_data_etag <-response_data_etag %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         dataset = "Electronic Tag")
  
  
# marker #
var<-BRT_ensemble$brt.marker$var.names
response_data_marker<-data.frame()

for (i in 1:length(var)){
response_plot_data_marker <- gbm::plot.gbm(BRT_ensemble$brt.marker,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_marker<-response_plot_data_marker %>% gather("variable","x",-y)

response_data_marker<-rbind(response_data_marker, response_plot_data_marker)

}

response_data_marker <-response_data_marker %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         dataset = "Marker")


# observer #
var<-BRT_ensemble$brt.observer$var.names
response_data_observer<-data.frame()

for (i in 1:length(var)){
response_plot_data_observer <- gbm::plot.gbm(BRT_ensemble$brt.observer,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_observer<-response_plot_data_observer %>% gather("variable","x",-y)

response_data_observer<-rbind(response_data_observer, response_plot_data_observer)

}

response_data_observer <-response_data_observer %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         dataset = "Observer")

response_data_ensemble <- rbind(response_data_etag, response_data_marker, response_data_observer) %>% 
                            mutate(dataset = factor(dataset, levels = c("Marker", "Observer", "Electronic Tag")))

# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, -8000, 
#                 0, response_data_ensemble %>% filter(variable == "SST") %>% pull(x) %>% min(),
#                 0, response_data_ensemble %>% filter(variable == "SST_sd") %>% pull(x) %>% min()), 
#            y = c(0, response_data_ensemble %>% filter(variable == "Bathymetry") %>% pull(y) %>% max(), 
#                  0, response_data_ensemble %>% filter(variable == "SST") %>% pull(y) %>% max(),
#                  0, response_data_ensemble %>% filter(variable == "SST_sd") %>% pull(y) %>% max()))

ensemble_response_plot <- response_data_ensemble %>% 
  # filter(variable != 'Bathymetry' | x > -7.229547e+03) %>% # so the x axis matches with iSDMs (all the same values so does matter removing these)
  ggplot() + 
  geom_line(aes(x=x, y=y, linetype = dataset, color = dataset), size =1) +
  geom_smooth(aes(x=x, y=y, color = "Ensemble"), method = "loess", span = 0.2, se = FALSE, linetype = "solid", size = 1) + 
  scale_linetype_manual(values=c("solid", "twodash", "dotted", "solid")) + 
  scale_color_manual(values=c('#FDAE61','#FDAE61','#FDAE61', 'darkgray')) +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  # geom_blank(data = dummy, aes(x = x, y = y)) +
  labs(x = "",
       y = "Marginal Effect",
       linetype = "",
       color = "")+
  theme_bw() + 
  theme(panel.spacing = unit(.30, "lines"),
        strip.text = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(margin = margin(0,0,.05,0, "cm"))) + 
  guides(color = guide_legend(override.aes = list(color = c('#FDAE61','#FDAE61','#FDAE61', 'darkgray'), linetype = c("solid", "twodash", "dotted", "solid"))),
         linetype = "none")
# RColorBrewer::brewer.pal(n = 4, "Spectral")




#############  
# BRT Pooling
#############  
var<-BRT_pooling$var.names
response_data_pooling<-data.frame()

for (i in 1:length(var)){
response_plot_data_pooling <- gbm::plot.gbm(BRT_pooling,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_pooling<-response_plot_data_pooling %>% gather("variable","x",-y)

response_data_pooling<-rbind(response_data_pooling, response_plot_data_pooling)

}

response_data_pooling <-response_data_pooling %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         model = "Pooling"
         )

# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, -8000, 
#                 0, response_data_pooling %>% filter(variable == "SST") %>% pull(x) %>% min(),
#                 0, response_data_pooling %>% filter(variable == "SST_sd") %>% pull(x) %>% min()), 
#            y = c(0, response_data_pooling %>% filter(variable == "Bathymetry") %>% pull(y) %>% max(), 
#                  0, response_data_pooling %>% filter(variable == "SST") %>% pull(y) %>% max(),
#                  0, response_data_pooling %>% filter(variable == "SST_sd") %>% pull(y) %>% max()))

pooling_response_plot <- response_data_pooling %>%
  # filter(variable != 'Bathymetry' | x > -7.229547e+03) %>% # so the x axis matches with iSDMs (all the same values so does matter removing these) 
  ggplot() + 
  geom_line(aes(x=x, y=y, color = model), size =1) +
  # scale_linetype_manual(values=c("solid","twodash", "dotted"))+ 
  scale_color_manual(values=c('#D7191C'))+
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  # geom_blank(data = dummy, aes(x = x, y = y)) +
  labs(x = "",
       y = "Marginal Effect",
       color = "")+
  theme_bw() + 
  theme(panel.spacing = unit(.30, "lines"),
        strip.text = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(margin = margin(0,0,.05,0, "cm")))
# RColorBrewer::brewer.pal(n = 4, "Spectral")  
  


  

####################################
# ISDM spatial & ISDM spatiotemporal
####################################

# need to get the range of values for the covariates so brining in the all the data just like I did for fitting the model

# bsh etag #
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





# bsh marker #
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



# bsh observer #
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

### ISDM Spatial ###
sst_seq <- seq(bsh_all$sst %>% min() %>% round(), bsh_all$sst %>% max() %>% round(), length.out = 20) #* sd(bsh_all$sst) + mean(bsh_all$sst)
sst_sd_seq <- seq(bsh_all$sst_sd %>% min() %>% round(), bsh_all$sst_sd%>% max() %>% round(), length.out = 20) #* sd(bsh_all$sst_sd) + mean(bsh_all$sst_sd)
bathy_seq <- seq(bsh_all$bathy %>% min() %>% round(), bsh_all$bathy %>% max() %>% round(), length.out = 20) #* sd(bsh_all$bathy) + mean(bsh_all$bathy)

ISDM_spatial_sst <- ISDM_spatial[[1]]$sst %>% 
                      mutate(value = sst_seq,
                             variable = "SST")
                      
ISDM_spatial_sst_sd <- ISDM_spatial[[1]]$sst_sd %>% 
                        mutate(value = sst_sd_seq,
                               variable = "SST_sd")

ISDM_spatial_bathy <- ISDM_spatial[[1]]$bathy %>% 
                        mutate(value = bathy_seq,
                               variable = "Bathymetry")
                        
ISDM_spatial_marginaleffects <- rbind(ISDM_spatial_sst, ISDM_spatial_sst_sd, ISDM_spatial_bathy) %>% 
                                  mutate(model = "iSDM Constant")
                                
# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, 0, 
#                 0, ISDM_spatial_marginaleffects %>% filter(variable == "SST") %>% pull(value) %>% min(),
#                 0, ISDM_spatial_marginaleffects %>% filter(variable == "SST_sd") %>% pull(value) %>% min()), 
#            y = c(0, ISDM_spatial_marginaleffects %>% filter(variable == "Bathymetry") %>% pull(mean) %>% max(), 
#                  0, ISDM_spatial_marginaleffects %>% filter(variable == "SST") %>% pull(mean) %>% max(),
#                  0, ISDM_spatial_marginaleffects %>% filter(variable == "SST_sd") %>% pull(mean) %>% max()))                                 
                 
ISDM_spatial_response_plot <- ISDM_spatial_marginaleffects %>% 
ggplot() + 
geom_ribbon(aes(x = value, ymin = mean - sd, ymax = mean + sd), fill = "grey70", alpha = 0.4) +
geom_line(aes(x = value, y = mean, color = model), size =1) +
# scale_linetype_manual(values=c("solid","twodash", "dotted"))+ 
scale_color_manual(values=c('#ABDDA4'))+
facet_wrap(~variable, scales = "free", nrow = 1) +
geom_hline(yintercept = 0, linetype = 2, alpha = 0.7) +
# geom_blank(data = dummy, aes(x = x, y = y)) +
labs(x = "",
      y = "Marginal Effect",
      color = "")+
theme_bw() + 
theme(panel.spacing = unit(.30, "lines"),
      strip.text = element_text(size=10),
      strip.background = element_blank(),
      strip.text.x = element_text(margin = margin(0,0,.05,0, "cm")))
# RColorBrewer::brewer.pal(n = 4, "Spectral")  


### ISDM Spatiotemporal ###
ISDM_spatiotemporal_sst <- ISDM_spatiotemporal[[1]]$sst %>% 
                      mutate(value = sst_seq,
                             variable = "SST")
                      
ISDM_spatiotemporal_sst_sd <- ISDM_spatiotemporal[[1]]$sst_sd %>% 
                        mutate(value = sst_sd_seq,
                               variable = "SST_sd")

ISDM_spatiotemporal_bathy <- ISDM_spatiotemporal[[1]]$bathy %>% 
                        mutate(value = bathy_seq,
                               variable = "Bathymetry")
                        
ISDM_spatiotemporal_marginaleffects <- rbind(ISDM_spatiotemporal_sst, ISDM_spatiotemporal_sst_sd, ISDM_spatiotemporal_bathy) %>% 
                                  mutate(model = "iSDM Seasonal")
                                  
# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, 0, 
#                 0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST") %>% pull(value) %>% min(),
#                 0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST_sd") %>% pull(value) %>% min()), 
#            y = c(0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "Bathymetry") %>% pull(mean) %>% max(), 
#                  0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST") %>% pull(mean) %>% max(),
#                  0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST_sd") %>% pull(mean) %>% max())) 
                 
ISDM_spatiotemporal_response_plot <- ISDM_spatiotemporal_marginaleffects %>% 
ggplot() + 
geom_ribbon(aes(x = value, ymin = mean - sd, ymax = mean + sd), fill = "grey70", alpha = 0.4) +
geom_line(aes(x = value, y = mean, color = model), size =1) +
# scale_linetype_manual(values=c("solid","twodash", "dotted"))+ 
scale_color_manual(values=c('#2B83BA'))+
facet_wrap(~variable, scales = "free", nrow = 1) +
geom_hline(yintercept = 0, linetype = 2, alpha = 0.7) +
# geom_blank(data = dummy, aes(x = x, y = y)) +
labs(x = "",
      y = "Marginal Effect",
      color = "")+
theme_bw() + 
theme(panel.spacing = unit(.30, "lines"),
      strip.text = element_text(size=10),
      strip.background = element_blank(),
      strip.text.x = element_text(margin = margin(0,0,.05,0, "cm")))
# RColorBrewer::brewer.pal(n = 4, "Spectral") 

pooling_response_plot / ensemble_response_plot / ISDM_spatial_response_plot / ISDM_spatiotemporal_response_plot #& ylim(c(-6.3, 3))

ggsave(here("plots","ModelResponseCurves3.png"),
       width = 8, height = 8, units = "in", dpi = 300)






###----Downsampled model response plots----####

#All BRT and INLA models
BRT_ensemble <- here("results", "FullModel", "BRT_ensemble_fullmodel_downsampled.rds") %>% readRDS()
BRT_pooling <- here("results", "FullModel", "BRT_pooling_fullmodel_downsampled.rds") %>% readRDS()
ISDM_spatial <- here("results", "FullModel", "bsh_ISDM_spatial_ME_GMRF_preds_norm_withintercepts_downsampled.rds") %>% readRDS()
ISDM_spatiotemporal <- here("results", "FullModel", "bsh_ISDM_spatiotemporal_ME_GMRF_preds_withintercepts_downsampled.rds") %>% readRDS()

##############
# BRT ensemble
##############

# etags #
var<-BRT_ensemble$brt.etag$var.names
response_data_etag<-data.frame()

for (i in 1:length(var)){
response_plot_data_etag <- gbm::plot.gbm(BRT_ensemble$brt.etag,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_etag<-response_plot_data_etag %>% gather("variable","x",-y)

response_data_etag<-rbind(response_data_etag, response_plot_data_etag)

}

response_data_etag <-response_data_etag %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         dataset = "Electronic Tag")
  
  
# marker #
var<-BRT_ensemble$brt.marker$var.names
response_data_marker<-data.frame()

for (i in 1:length(var)){
response_plot_data_marker <- gbm::plot.gbm(BRT_ensemble$brt.marker,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_marker<-response_plot_data_marker %>% gather("variable","x",-y)

response_data_marker<-rbind(response_data_marker, response_plot_data_marker)

}

response_data_marker <-response_data_marker %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         dataset = "Marker")


# observer #
var<-BRT_ensemble$brt.observer$var.names
response_data_observer<-data.frame()

for (i in 1:length(var)){
response_plot_data_observer <- gbm::plot.gbm(BRT_ensemble$brt.observer,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_observer<-response_plot_data_observer %>% gather("variable","x",-y)

response_data_observer<-rbind(response_data_observer, response_plot_data_observer)

}

response_data_observer <-response_data_observer %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         dataset = "Observer")

response_data_ensemble <- rbind(response_data_etag, response_data_marker, response_data_observer) %>% 
                            mutate(dataset = factor(dataset, levels = c("Marker", "Observer", "Electronic Tag")))

# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, -8000, 
#                 0, response_data_ensemble %>% filter(variable == "SST") %>% pull(x) %>% min(),
#                 0, response_data_ensemble %>% filter(variable == "SST_sd") %>% pull(x) %>% min()), 
#            y = c(0, response_data_ensemble %>% filter(variable == "Bathymetry") %>% pull(y) %>% max(), 
#                  0, response_data_ensemble %>% filter(variable == "SST") %>% pull(y) %>% max(),
#                  0, response_data_ensemble %>% filter(variable == "SST_sd") %>% pull(y) %>% max()))

ensemble_response_plot_downsampled <- response_data_ensemble %>% 
  # filter(variable != 'Bathymetry' | x > -7.229547e+03) %>% # so the x axis matches with iSDMs (all the same values so does matter removing these)
  ggplot() + 
  geom_line(aes(x=x, y=y, linetype = dataset, color = dataset), size =1) +
  geom_smooth(aes(x=x, y=y, color = "Ensemble"), method = "loess", span = 0.2, se = FALSE, linetype = "solid", size = 1) + 
  scale_linetype_manual(values=c("solid", "twodash", "dotted", "solid")) + 
  scale_color_manual(values=c('#FDAE61','#FDAE61','#FDAE61', 'darkgray')) +
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  # geom_blank(data = dummy, aes(x = x, y = y)) +
  labs(x = "",
       y = "Marginal Effect",
       linetype = "",
       color = "")+
  theme_bw() + 
  theme(panel.spacing = unit(.30, "lines"),
        strip.text = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(margin = margin(0,0,.05,0, "cm"))) + 
  guides(color = guide_legend(override.aes = list(color = c('#FDAE61','#FDAE61','#FDAE61', 'darkgray'), linetype = c("solid", "twodash", "dotted", "solid"))),
         linetype = "none")
# RColorBrewer::brewer.pal(n = 4, "Spectral")




#############  
# BRT Pooling
#############  
var<-BRT_pooling$var.names
response_data_pooling<-data.frame()

for (i in 1:length(var)){
response_plot_data_pooling <- gbm::plot.gbm(BRT_pooling,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_pooling<-response_plot_data_pooling %>% gather("variable","x",-y)

response_data_pooling<-rbind(response_data_pooling, response_plot_data_pooling)

}

response_data_pooling <-response_data_pooling %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "bathy" ~ "Bathymetry"),
         model = "Pooling"
         )

# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, -8000, 
#                 0, response_data_pooling %>% filter(variable == "SST") %>% pull(x) %>% min(),
#                 0, response_data_pooling %>% filter(variable == "SST_sd") %>% pull(x) %>% min()), 
#            y = c(0, response_data_pooling %>% filter(variable == "Bathymetry") %>% pull(y) %>% max(), 
#                  0, response_data_pooling %>% filter(variable == "SST") %>% pull(y) %>% max(),
#                  0, response_data_pooling %>% filter(variable == "SST_sd") %>% pull(y) %>% max()))

pooling_response_plot_downsampled <- response_data_pooling %>%
  # filter(variable != 'Bathymetry' | x > -7.229547e+03) %>% # so the x axis matches with iSDMs (all the same values so does matter removing these) 
  ggplot() + 
  geom_line(aes(x=x, y=y, color = model), size =1) +
  # scale_linetype_manual(values=c("solid","twodash", "dotted"))+ 
  scale_color_manual(values=c('#D7191C'))+
  facet_wrap(~variable, scales = "free", nrow = 1) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  # geom_blank(data = dummy, aes(x = x, y = y)) +
  labs(x = "",
       y = "Marginal Effect",
       color = "")+
  theme_bw() + 
  theme(panel.spacing = unit(.30, "lines"),
        strip.text = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(margin = margin(0,0,.05,0, "cm")))
# RColorBrewer::brewer.pal(n = 4, "Spectral")  



####################################
# ISDM spatial & ISDM spatiotemporal
####################################

# need to get the range of values for the covariates so brining in the all the data just like I did for fitting the model

# bsh etag #
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





# bsh marker #
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



# bsh observer #
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

### ISDM Spatial ###
sst_seq <- seq(bsh_all$sst %>% min() %>% round(), bsh_all$sst %>% max() %>% round(), length.out = 20) #* sd(bsh_all$sst) + mean(bsh_all$sst)
sst_sd_seq <- seq(bsh_all$sst_sd %>% min() %>% round(), bsh_all$sst_sd%>% max() %>% round(), length.out = 20) #* sd(bsh_all$sst_sd) + mean(bsh_all$sst_sd)
bathy_seq <- seq(bsh_all$bathy %>% min() %>% round(), bsh_all$bathy %>% max() %>% round(), length.out = 20) #* sd(bsh_all$bathy) + mean(bsh_all$bathy)

ISDM_spatial_sst <- ISDM_spatial[[1]]$sst %>% 
                      mutate(value = sst_seq,
                             variable = "SST")
                      
ISDM_spatial_sst_sd <- ISDM_spatial[[1]]$sst_sd %>% 
                        mutate(value = sst_sd_seq,
                               variable = "SST_sd")

ISDM_spatial_bathy <- ISDM_spatial[[1]]$bathy %>% 
                        mutate(value = bathy_seq,
                               variable = "Bathymetry")
                        
ISDM_spatial_marginaleffects <- rbind(ISDM_spatial_sst, ISDM_spatial_sst_sd, ISDM_spatial_bathy) %>% 
                                  mutate(model = "iSDM Constant")
                                
# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, 0, 
#                 0, ISDM_spatial_marginaleffects %>% filter(variable == "SST") %>% pull(value) %>% min(),
#                 0, ISDM_spatial_marginaleffects %>% filter(variable == "SST_sd") %>% pull(value) %>% min()), 
#            y = c(0, ISDM_spatial_marginaleffects %>% filter(variable == "Bathymetry") %>% pull(mean) %>% max(), 
#                  0, ISDM_spatial_marginaleffects %>% filter(variable == "SST") %>% pull(mean) %>% max(),
#                  0, ISDM_spatial_marginaleffects %>% filter(variable == "SST_sd") %>% pull(mean) %>% max()))                                 
                 
ISDM_spatial_response_plot_downsampled <- ISDM_spatial_marginaleffects %>% 
ggplot() + 
geom_ribbon(aes(x = value, ymin = mean - sd, ymax = mean + sd), fill = "grey70", alpha = 0.4) +
geom_line(aes(x = value, y = mean, color = model), size =1) +
# scale_linetype_manual(values=c("solid","twodash", "dotted"))+ 
scale_color_manual(values=c('#ABDDA4'))+
facet_wrap(~variable, scales = "free", nrow = 1) +
geom_hline(yintercept = 0, linetype = 2, alpha = 0.7) +
# geom_blank(data = dummy, aes(x = x, y = y)) +
labs(x = "",
      y = "Marginal Effect",
      color = "")+
theme_bw() + 
theme(panel.spacing = unit(.30, "lines"),
      strip.text = element_text(size=10),
      strip.background = element_blank(),
      strip.text.x = element_text(margin = margin(0,0,.05,0, "cm")))
# RColorBrewer::brewer.pal(n = 4, "Spectral")  


### ISDM Spatiotemporal ###
ISDM_spatiotemporal_sst <- ISDM_spatiotemporal[[1]]$sst %>% 
                      mutate(value = sst_seq,
                             variable = "SST")
                      
ISDM_spatiotemporal_sst_sd <- ISDM_spatiotemporal[[1]]$sst_sd %>% 
                        mutate(value = sst_sd_seq,
                               variable = "SST_sd")

ISDM_spatiotemporal_bathy <- ISDM_spatiotemporal[[1]]$bathy %>% 
                        mutate(value = bathy_seq,
                               variable = "Bathymetry")
                        
ISDM_spatiotemporal_marginaleffects <- rbind(ISDM_spatiotemporal_sst, ISDM_spatiotemporal_sst_sd, ISDM_spatiotemporal_bathy) %>% 
                                  mutate(model = "iSDM Seasonal")
                                  
# dummy <- data.frame(variable = c("Bathymetry", "Bathymetry", "SST", "SST", "SST_sd", "SST_sd"), 
#            x = c(-8000, 0, 
#                 0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST") %>% pull(value) %>% min(),
#                 0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST_sd") %>% pull(value) %>% min()), 
#            y = c(0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "Bathymetry") %>% pull(mean) %>% max(), 
#                  0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST") %>% pull(mean) %>% max(),
#                  0, ISDM_spatiotemporal_marginaleffects %>% filter(variable == "SST_sd") %>% pull(mean) %>% max())) 
                 
ISDM_spatiotemporal_response_plot_downsampled <- ISDM_spatiotemporal_marginaleffects %>% 
ggplot() + 
geom_ribbon(aes(x = value, ymin = mean - sd, ymax = mean + sd), fill = "grey70", alpha = 0.4) +
geom_line(aes(x = value, y = mean, color = model), size =1) +
# scale_linetype_manual(values=c("solid","twodash", "dotted"))+ 
scale_color_manual(values=c('#2B83BA'))+
facet_wrap(~variable, scales = "free", nrow = 1) +
geom_hline(yintercept = 0, linetype = 2, alpha = 0.7) +
# geom_blank(data = dummy, aes(x = x, y = y)) +
labs(x = "",
      y = "Marginal Effect",
      color = "")+
theme_bw() + 
theme(panel.spacing = unit(.30, "lines"),
      strip.text = element_text(size=10),
      strip.background = element_blank(),
      strip.text.x = element_text(margin = margin(0,0,.05,0, "cm")))
# RColorBrewer::brewer.pal(n = 4, "Spectral") 

pooling_response_plot_downsampled / ensemble_response_plot_downsampled / ISDM_spatial_response_plot_downsampled / ISDM_spatiotemporal_response_plot_downsampled #& ylim(c(-6.3, 3))

ggsave(here("plots","ModelResponseCurves_downsampled.png"),
       width = 8, height = 8, units = "in", dpi = 300)
