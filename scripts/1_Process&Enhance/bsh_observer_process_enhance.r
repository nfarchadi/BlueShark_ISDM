# processing and enhancing bsh observer data 

library(tidyverse)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean)
library(zoo)
library(here)
library(sf)
library(tidyverse)
library(viridis)

world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE) # need to do this to remove spherical geometry

source(here("scripts", "functions", "remove_land.r"))
source(here("scripts","functions","enhance_data.r"))

# load NWA shapefile
NWA <- here("data","shapefiles","NWA.shp") %>% 
        sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# load bsh observer data
observer <- here("data","bsh_data","observer-enhanced_pseudo_1to1.csv") %>% read.csv() %>%
        dplyr::select(date, lon, lat, pres) %>%
        filter(pres == 1) %>% 
        sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
        mutate(lon = sf::st_coordinates(.)[, 1],
               lat = sf::st_coordinates(.)[, 2],
               year_mon = zoo::as.yearmon(date),
               year = lubridate::year(date),
               month = lubridate::month(date))
        
observer <- observer[which(lengths(st_intersects(observer, NWA)) > 0),] # do this only if you want to contrain the locations within the NWA shapefile


ggplot() +
geom_sf(data = world, color = "black", fill = "grey") +
geom_sf(data = NWA, color = "blue", fill = NA) +
geom_sf(data = observer, color = "red") +
coord_sf(xlim = c(-100, -5), ylim = c(-10, 55), expand = TRUE) +
theme_bw()



#########################################
# generating background points/pseudo abs
#########################################

# Use one of your environmental rasters as a template. Be sure to have it as the same resolution as what your predicted outputs will be. Most likely this will be the raster with the lowest common resolution.

template<-raster("E:/GLORYS_NWA/monthly_0.08/cmems_mod_glo_phy_my_0.083_P1M-m_1993-01.grd", band = 9) # bathy  
# hull <- terra::convHull(terra::vect(observer)) %>% as("Spatial") # MCP to retrict locations of pseudo-abs
# template <- crop(template, hull) %>% mask(hull)
x<-template
res(x)<-0.08
template<-resample(template,x, method='bilinear')

# filter dates based on presence availability
u_yearmon <-observer$year_mon %>% unique()

# for loop will randomly choose 80 background points per day. This will produce 50,000+ background points which is recommended by Valavi et al. 2022
# background points will be chosen regardless of the presence points
for (i in 1:length(u_yearmon)) {
    uym <- u_yearmon[i]
    print(uym)
    
    # creating the monthly MCP based on all presences in that month
    mon <- lubridate::month(uym)
    mon_observer <- observer %>% filter(month == mon)
    hull <- terra::convHull(terra::vect(mon_observer)) %>% as("Spatial") # MCP to retrict locations of pseudo-abs
    mon_template <- crop(template, hull) %>% mask(hull)
 
    # see how many presences there are in that yearmon
    num_mon_pres <- observer %>% filter(year_mon == as.yearmon(uym)) %>% nrow()
    
    
    ab <- dismo::randomPoints(mon_template, num_mon_pres * 3) %>% 
            as.data.frame() %>% 
            mutate(year_mon = uym) %>% 
            rename("lon"="x", "lat"="y") %>% 
            mutate(month = lubridate::month(uym),
                   year = lubridate::year(uym))
        
    if(!exists('absences')){
        absences <- ab
        } else {
        absences <- rbind(absences, ab)
        } 
}




# need a z column to match with presence df
absences <- remove_land(input_df = absences, bathy_file = template)

absences <- absences %>% 
            sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
            mutate(
                lon = sf::st_coordinates(.)[, 1],
                lat = sf::st_coordinates(.)[, 2],
                pres_abs = 0
                ) %>% 
            dplyr::select(lon, lat, pres_abs, year_mon, year, month)
            
            
observer <- observer %>% 
            rename("pres_abs" = "pres") %>% 
            sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
            mutate(
                lon = sf::st_coordinates(.)[, 1],
                lat = sf::st_coordinates(.)[, 2],
                date = as.Date(date),
                pres_abs = 1
                ) %>% 
            dplyr::select(lon, lat, pres_abs, year_mon, year, month)


#now we combine
Pres_Abs_observer<-rbind(observer,absences)


###################################################
# enhancing AIS with hycom & bathy data for the NEP
###################################################

GLORYS_NWA_dir <- "E:/GLORYS_NWA/monthly_0.08"

# function to enhance AIS data
observer2<-enhance_data(input_df = Pres_Abs_observer, env_dir = GLORYS_NWA_dir, add_error = FALSE) #run it!

observer2 <- observer2 %>% na.omit()

saveRDS(observer2, here("data","bsh_data","bsh_observer_enhanced.rds"))
