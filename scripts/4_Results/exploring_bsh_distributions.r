# exploring the spatial distriubtions of each data type and within LMEs around the north Atlantic

library(tidyverse)
library(raster)
library(zoo)
library(here)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE) # need to do this to remove spherical geometry

LME <- "C:/Users/nfarc/Desktop/RCodes_Shapefiles/shapefiles/LME_66/LMEs66.shp" %>% 
        sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

GLORYS_N_Atl <- here("data","GLORYS", "GLORYS_clim.grd") %>% stack() %>% scale()
GLORYS_N_Atl <- dropLayer(GLORYS_N_Atl, c(2,3,5))
#GLORYS_NWA <- projectRaster(GLORYS_NWA, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=laea +lat_0=32.5 +lon_0=-52.5 +x_0=0 +y_0=0 +units=km +no_defs +ellps=WGS84"
GLORYS_N_Atl <- terra::rast(GLORYS_N_Atl) # inlabru uses SpatRaster objects

N_Atl <- as.polygons(GLORYS_N_Atl > -Inf)
N_Atl <- N_Atl %>% sf::st_as_sf("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

LME <- LME[which(lengths(st_intersects(LME, N_Atl)) > 0),] 

N_Atl <- st_difference(N_Atl, st_union(st_combine(LME)))

NWA <- sf::st_crop(N_Atl, xmin = -97.875, xmax = -40, ymin = 9.958333, ymax = 55.04167)

NEA <- sf::st_crop(N_Atl, xmin = -40, xmax = -4.958333, ymin = 9.958333, ymax = 55.04167)
NWA_NEA <- st_union(NWA,NEA)

LME_NAtl <- dplyr::bind_rows(LME,NWA,NEA)
LME_NAtl$LME_NAME <- c("Labrador - Newfoundland", "Celtic-Biscay Shelf", "Scotian Shelf",
                        "Northeast U.S. Continental Shelf", "Iberian Coastal", "Canary Current",
                        "Southeast U.S. Continental Shelf", "Gulf of Mexico", "Caribbean Sea",
                        "North Brazil Shelf", "Guinea Current", "Northwest Atlantic Ocean",
                        "Northeast Atlantic Ocean")

st_write(LME_NAtl, here("data", "shapefiles", "LME_NAtl.shp"))


#need to reduce the resolution to 0.25 degrees
res <- 1

bsh_all_1degree<-bsh_all %>% filter(pres_abs == 1) %>% 
  mutate(X = floor(lon/res) * res + 0.5 * res,
         Y = floor(lat/res) * res + 0.5 * res,
         Observations = 1) %>% 
  group_by(X,Y, dataset) %>% 
  summarise(Observations = sum(Observations, na.rm = T), .groups = "drop") 

# No. presences across the north atlantic
ggplot() +
    geom_sf(data = world, color = "black", fill = "grey") +
    geom_tile(bsh_all_1degree,
             mapping = aes(x=X, y=Y, fill = Observations)) +
    scale_fill_cmocean(name = "ice", trans = "log",
                       limits = c(1,10000),
                     breaks = c(1,10,100,1000,10000),
                     direction = -1) +
    coord_sf(xlim = c(-100, -0), ylim = c(-10, 55), expand = TRUE) +
    theme_bw() +
    facet_wrap(~dataset) +
    labs(fill = "No. Presences")


BSH_LME_oceans_map <- ggplot() +
    geom_sf(data = world, color = "black", fill = "grey") +
    geom_sf(data = bsh_all %>% filter(pres_abs == 1) %>% 
            sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) +
    geom_sf(data = LME_NAtl, aes(fill= LME_NAME), alpha = 0.4) +
    coord_sf(xlim = c(-100, -0), ylim = c(-10, 55), expand = TRUE) +
    theme_bw() +
    labs(fill = "LME & Oceans") +
    facet_wrap(~dataset, ncol = 3) +
    scale_color_manual(values = c('#7CAE00','#00BFC4','#C77CFF')) +
    theme(legend.position = 'none')
    


# how many of each data type in each region - this takes awhile
bsh_all_LME <- st_intersection(bsh_all %>% filter(pres_abs == 1) %>% 
            sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"),
            LME_NAtl) 

BSH_LME_oceans_total <- bsh_all_LME %>% 
sf::st_drop_geometry() %>% 
group_by(dataset, LME_NAME) %>% 
summarise(n_LME = n()) %>% 
ggplot() +
geom_bar(aes(x = LME_NAME, y = n_LME, fill = LME_NAME), stat = 'identity', alpha = 0.9) +
facet_wrap(~dataset, ncol = 3) +
theme_bw() +
labs(x = "LME", y = "Counts", fill = "") +
theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
coord_flip() +
theme(legend.position = "bottom")

map_and_bar <- BSH_LME_oceans_map / BSH_LME_oceans_total + patchwork::plot_layout(heigh = c(1, 0.8))
map_and_bar


# No. presences in the GOM
GOM <- LME_NAtl %>% filter(LME_NAME == "Gulf of Mexico")
bsh_all_GOM_1degree <- bsh_all_1degree %>% sf::st_as_sf(coords = c("X", "Y"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
bsh_all_GOM_1degree <- bsh_all_GOM_1degree[which(lengths(st_intersects(bsh_all_GOM_1degree, GOM)) > 0),] # do this only if you want to contrain the locations within the NWA shapefile

  
ggplot() +
    geom_sf(data = world, color = "black", fill = "grey") +
    geom_tile(bsh_all_GOM_1degree,
             mapping = aes(fill = Observations)) +
    scale_fill_cmocean(name = "ice", trans = "log",
                    #    limits = c(1,10000),
                     breaks = c(1,10,100,1000,10000),
                     direction = -1) +
    coord_sf(xlim = c(-100, -81.5), ylim = c(-22.5, 29.5), expand = TRUE) +
    theme_bw() +
    facet_wrap(~dataset) +
    labs(fill = "No. Presences")
