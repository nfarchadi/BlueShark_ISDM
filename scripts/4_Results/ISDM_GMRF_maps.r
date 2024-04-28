# Fig. GMRF difference between ISDM Spatial and ISDM Spatiotemporal

library(here)
library(tidyverse)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean) 
library(sf) 
world <- ne_countries(scale = "medium", returnclass = "sf")


# INLA model results
ISDM_spatial <- here("results", "FullModel", "bsh_ISDM_spatial_ME_GMRF_preds_norm_withintercepts.rds") %>% readRDS()
ISDM_spatiotemporal <- here("results", "FullModel", "bsh_ISDM_spatiotemporal_ME_GMRF_preds_withintercepts.rds") %>% readRDS()

###############################
# Plotting GMRF of ISDM Spatial
###############################

ISDM_spatial_GMRF <- ISDM_spatial[[2]]

ISDM_spatial_GMRF_plot <- ISDM_spatial_GMRF %>% 
mutate(lon = st_coordinates(ISDM_spatial_GMRF)[,1],
       lat = st_coordinates(ISDM_spatial_GMRF)[,2]) %>% 
st_drop_geometry() %>% 
ggplot() +
geom_raster(aes(x = lon, y = lat, fill = mean)) +
geom_sf(data = world, color = "black", fill = "grey") +
coord_sf(xlim = c(-100, -5), ylim = c(10, 56), expand = FALSE) +
theme_bw() +
# facet_wrap(~season) +
cmocean::scale_fill_cmocean(name = "thermal") + 
labs(x = "", y = "", fill = "Mean") +
guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                             barwidth = 1, barheight = 15))

ggsave(here("plots","ISDM_spatial_GMRF.png"),
       width = 7, height = 5, units = "in", dpi = 300)


######################################
# Plotting GMRF of ISDM Spatiotemporal
######################################

ISDM_spatiotemporal_GMRF <- ISDM_spatiotemporal[[2]]

ISDM_spatiotemporal_GMRF_plot <- ISDM_spatiotemporal_GMRF %>% 
mutate(lon = st_coordinates(ISDM_spatiotemporal_GMRF)[,1],
       lat = st_coordinates(ISDM_spatiotemporal_GMRF)[,2]) %>% 
mutate(season = case_when(season == 1 ~ "Winter",
                          season == 2 ~ "Spring",
                          season == 3 ~ "Summer",
                          season == 4 ~ "Fall"),
       season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))  %>% 
st_drop_geometry() %>% 
ggplot() +
geom_raster(aes(x = lon, y = lat, fill = mean)) +
geom_sf(data = world, color = "black", fill = "grey") +
coord_sf(xlim = c(-100, -5), ylim = c(10, 56), expand = FALSE) +
facet_wrap(~season, nrow = 4) +
theme_bw() +
theme(strip.text = element_text(size = 15),
      strip.background = element_blank()) +
cmocean::scale_fill_cmocean(name = "thermal") + 
labs(x = "", y = "", fill = "Mean") +
guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                             barwidth = 1, barheight = 15))


ggsave(here("plots","ISDM_spatiotemporal_GMRF.png"),
       width = 6, height = 10, units = "in", dpi = 300)


############################################
# Difference (spatial - spatiotemporal) plot  
############################################
ISDM_spatial_GMRF2 <- ISDM_spatial_GMRF %>% 
mutate(lon = st_coordinates(ISDM_spatial_GMRF)[,1],
       lat = st_coordinates(ISDM_spatial_GMRF)[,2]) %>% 
st_drop_geometry() %>% 
dplyr::select(mean, lon, lat) %>% 
rename("Constant" = "mean")

ISDM_spatiotemporal_GMRF2 <- ISDM_spatiotemporal_GMRF  %>% 
mutate(lon = st_coordinates(ISDM_spatiotemporal_GMRF)[,1],
       lat = st_coordinates(ISDM_spatiotemporal_GMRF)[,2]) %>% 
st_drop_geometry() %>% 
dplyr::select(season, mean, lon, lat) %>% 
mutate(season = case_when(season == 1 ~ "Winter",
                          season == 2 ~ "Spring",
                          season == 3 ~ "Summer",
                          season == 4 ~ "Fall"),
       season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))) %>% 
    group_by(lon, lat) %>% 
    spread("season","mean")
    
# join
ISDM_GMRF_all <- ISDM_spatial_GMRF2 %>% 
left_join(., ISDM_spatiotemporal_GMRF2, by = c("lat","lon"))

# calculating the difference between constant and each season
ISDM_GMRF_diff <- ISDM_GMRF_all %>%
  mutate(across(c(-matches("Constant"), -matches("lon"), -matches("lat")), ~ Constant - ., .names = "{col}_dif"))


ISDM_GMRF_diff <- ISDM_GMRF_diff %>%
    dplyr::select(-Constant, -Winter, -Spring, -Summer, -Fall) %>% 
    gather("season","difference",-lon,-lat) %>% 
    mutate(season = case_when(season == "Winter_dif" ~ "Winter",
                              season == "Spring_dif" ~ "Spring",
                              season == "Summer_dif" ~ "Summer",
                              season == "Fall_dif" ~ "Fall"),
           season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))

   
ISDM_spatial_diff_plot <- ISDM_GMRF_diff %>%
    ggplot() +
    geom_raster(aes(x = lon, y = lat, fill = difference)) +
    geom_sf(data = world, color = "black", fill = "grey") +
    coord_sf(xlim = c(-100, -5), ylim = c(10, 56), expand = FALSE) +
    theme_bw() +
    theme(strip.text = element_text(size = 15),
      strip.background = element_blank()) +
    facet_wrap(~season, nrow = 4) +
    cmocean::scale_fill_cmocean(name = "balance", values = scales::rescale(c(-7.5, -4.5, 0, 1.5, 4.5))) + 
    labs(x = "", y = "", fill = "Difference") +
guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                             barwidth = 1, barheight = 15))

ggsave(here("plots","ISDM_GMRF_diff.png"),
       width = 6, height = 10, units = "in", dpi = 300)
