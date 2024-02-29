library(tidyverse)
library(raster)
library(zoo)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean)
library(sf)
library(patchwork)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE) # need to do this to remove spherical geometry
# source(here("scripts", "functions","collinearity.r")) # find out how here works on the farm

######
# etag
######

# reduce resolution 
res <- 0.5

etag <- here("data","bsh_data","bsh_etag_enhanced.rds") %>% # or here("data","bsh_data","bsh_etag_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry() %>% 
            filter(pres_abs == 1) %>% unique() %>% 
            mutate(lon = floor(lon/res) * res + 0.5 * res, 
                   lat = floor(lat/res) * res + 0.5 * res) %>%
            group_by(lon,lat) %>% 
            summarise(presence = sum(pres_abs, na.rm = T), .groups = "drop") %>% 
            mutate(data = "Electronic Tag")
            

########
# marker
########

marker <- here("data","bsh_data","bsh_marker_enhanced.rds") %>% # or here("data","bsh_data","bsh_marker_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry() %>% 
            filter(pres_abs == 1) %>% unique() %>% 
            mutate(lon = floor(lon/res) * res + 0.5 * res, 
                   lat = floor(lat/res) * res + 0.5 * res) %>%
            group_by(lon,lat) %>% 
            summarise(presence = sum(pres_abs, na.rm = T), .groups = "drop") %>% 
            mutate(data = "Marker")

##########
# observer
##########

observer <- here("data","bsh_data","bsh_observer_enhanced.rds") %>% # or here("data","bsh_data","bsh_observer_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry() %>% 
            filter(pres_abs == 1) %>% unique() %>% 
            mutate(lon = floor(lon/res) * res + 0.5 * res, 
                   lat = floor(lat/res) * res + 0.5 * res) %>%
            group_by(lon,lat) %>% 
            summarise(presence = sum(pres_abs, na.rm = T), .groups = "drop") %>% 
            mutate(data = "Observer") %>% 
            filter(presence >= 3)


bsh_all <- rbind(etag, marker, observer) %>% 
mutate(data = factor(data, levels = c("Marker", "Observer", "Electronic Tag")))

bsh_presence_map <- ggplot() +
geom_raster(bsh_all, mapping = aes(x=lon, y=lat, fill = presence)) +
scale_fill_cmocean(name = "deep", trans = "log",
                   breaks = c(1, 10, 100, 1000, 3000),
                   limits = c(1, 3000),
                   direction = -1) +
geom_sf(data = world, color = "black", fill = "grey") +
coord_sf(xlim = c(-100, -5), ylim = c(5, 55), expand = FALSE) +
facet_wrap(~data, ncol = 1) +
labs(fill = "No. Presences", x = "", y = "") + 
guides(fill = guide_colorbar(title.position = "right",
                             title.hjust = 0.5,
                             title.theme = element_text(angle = 90),
                             barwidth = 1.5, 
                             barheight = 15
                            )) +
theme_bw()


ggsave(here("plots","BSH_presence_maps.png"),
       width = 8, height = 10, units = "in", dpi = 300)



