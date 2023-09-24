# processing and enhancing bsh etag data 

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
source(here("scripts","functions","enhance_data_seasonal.r"))

# load NWA shapefile
NWA <- here("data","shapefiles","NWA.shp") %>% 
        sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# load bsh etag data
etag <- here("data","bsh_data","bsh_etag.csv") %>% read.csv() %>%
        dplyr::select(date, lon, lat,latitudeError,longitudeError) %>% na.omit() %>% 
        sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
        mutate(lon = sf::st_coordinates(.)[, 1],
               lat = sf::st_coordinates(.)[, 2],
               year_mon = zoo::as.yearmon(date),
               year = lubridate::year(date),
               month = lubridate::month(date))
        
etag <- etag[which(lengths(st_intersects(etag, NWA)) > 0),] # do this only if you want to contrain the locations within the NWA shapefile
etag %>% nrow() #21679 presences

ggplot() +
geom_sf(data = world, color = "black", fill = "grey") +
geom_sf(data = NWA, color = "blue", fill = NA) +
geom_sf(data = etag, color = "red") +
coord_sf(xlim = c(-100, -5), ylim = c(-10, 55), expand = TRUE) +
theme_bw()



#########################################
# generating background points/pseudo abs
#########################################

# Use one of your environmental rasters as a template. Be sure to have it as the same resolution as what your predicted outputs will be. Most likely this will be the raster with the lowest common resolution.

template<-raster(zz, band = 9) # bathy  
# hull <- terra::convHull(terra::vect(etag)) %>% as("Spatial") # MCP to retrict locations of pseudo-abs
# template <- crop(template, hull) %>% mask(hull)
x<-template
res(x)<-0.08
template<-resample(template,x, method='bilinear')

# filter dates based on presence availability
u_yearmon <-etag$year_mon %>% unique()

# for loop will randomly choose 80 background points per day. This will produce 50,000+ background points which is recommended by Valavi et al. 2022
# background points will be chosen regardless of the presence points
for (i in 1:length(u_yearmon)) {
    uym <- u_yearmon[i]
    print(uym)
    
    # creating the monthly MCP based on all presences in that month
    mon <- lubridate::month(uym)
    mon_etag <- etag %>% filter(month == mon)
    hull <- terra::convHull(terra::vect(mon_etag)) %>% as("Spatial") # MCP to retrict locations of pseudo-abs
    mon_template <- crop(template, hull) %>% mask(hull)
 
    # see how many presences there are in that yearmon
    num_mon_pres <- etag %>% filter(year_mon == as.yearmon(uym)) %>% nrow()
    
    
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
                pres_abs = 0,
                longitudeError = 0, 
                latitudeError = 0
                ) %>% 
            dplyr::select(lon, lat, longitudeError, latitudeError, pres_abs, year_mon, year, month)


etag <- etag %>% 
            sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
            mutate(
                lon = sf::st_coordinates(.)[, 1],
                lat = sf::st_coordinates(.)[, 2],
                date = as.Date(date),
                pres_abs = 1
                ) %>% 
            dplyr::select(lon, lat, longitudeError, latitudeError, pres_abs, year_mon, year, month)


#now we combine
Pres_Abs_etag<-rbind(etag,absences)


#############################################################
# enhancing etag with monthly GLORYS & bathy data for the NWA
#############################################################

GLORYS_NWA_dir <- "E:/GLORYS_NWA/monthly_0.08"

# function to enhance AIS data
etag2<-enhance_data(input_df = Pres_Abs_etag, env_dir = GLORYS_NWA_dir, add_error = TRUE) #run it!

etag2 <- etag2 %>% na.omit()

saveRDS(etag2, here("data","bsh_data","bsh_etag_enhanced.rds"))


####################################################################
# enhancing etag with climotological GLORYS & bathy data for the NWA
####################################################################
GLORYS_clim <- here("data","GLORYS","GLORYS_clim.grd") %>% stack()

etag <- here("data","bsh_data","bsh_etag_enhanced.rds") %>% 
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()

etag <- etag %>% dplyr::select(1:8) 
etag.pres <- etag  %>% filter(pres_abs == 1)
etag.pres.env <- data.frame()

for (j in 1:nrow(etag.pres)){
    error_grid <- raster::extent(etag.pres[j,"lon"] - etag.pres[j,"longitudeError"],
                                 etag.pres[j,"lon"] + etag.pres[j,"longitudeError"],
                                 etag.pres[j,"lat"] - etag.pres[j,"latitudeError"],
                                 etag.pres[j,"lat"] + etag.pres[j,"latitudeError"])
    
    if(!(error_grid[1] == error_grid[2] & error_grid[3] == error_grid[4])){
        etag.pres.env_mean <- raster::extract(GLORYS_clim, error_grid) %>% as.data.frame() %>% 
        summarise(across(everything(), mean, na.rm = TRUE))}
    
    etag.pres.env <- rbind(etag.pres.env, etag.pres.env_mean)
        
}

etag.pres <- cbind(etag.pres, etag.pres.env)

etag.abs <- etag %>% filter(pres_abs == 0)
etag.abs.env <- raster::extract(GLORYS_clim, etag.abs %>% sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
etag.abs <- cbind(etag.abs.env %>% sf::st_coordinates(), etag.abs, etag.abs.env)

# combine
etag.clim <- rbind(etag.pres, etag.abs)
saveRDS(etag.clim, here("data","bsh_data","bsh_etag_enhanced_clim.rds"))


##############################################################
# enhancing etag with seasonal GLORYS & bathy data for the NWA
##############################################################

etag <- here("data","bsh_data","bsh_etag_enhanced.rds") %>% 
            readRDS() %>%
            dplyr::select(-X,-Y)

etag <- etag %>% dplyr::select(1:8) %>% mutate(quarter = as.yearqtr(year_mon) + 1/12,
                                               quarter = format(quarter, "%q") %>% as.numeric())

GLORYS_NWA_dir <- here("data","GLORYS")

# function to enhance AIS data
etag_seasonal<-enhance_data_seasonal(input_df = etag, env_dir = GLORYS_NWA_dir, add_error = TRUE) #run it!

etag_seasonal <- etag_seasonal %>% na.omit()

saveRDS(etag_seasonal, here("data","bsh_data","bsh_etag_enhanced_seasonal.rds"))