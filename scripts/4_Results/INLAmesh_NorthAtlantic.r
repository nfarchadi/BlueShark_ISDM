# INLA mesh figure 

library(tidyverse)#, lib.loc = "~/bsh_ISDM/R_packages")
library(raster)#, lib.loc = "~/bsh_ISDM/R_packages")
library(zoo)#, lib.loc = "~/bsh_ISDM/R_packages")
library(here)#, lib.loc = "~/bsh_ISDM/R_packages")
library(inlabru)#, lib.loc = "~/bsh_ISDM/R_packages")
library(INLA)#, lib.loc = "~/bsh_ISDM/R_packages")
library(sn)#, lib.loc = "~/bsh_ISDM/R_packages")
library(Metrics)#, lib.loc = "~/bsh_ISDM/R_packages")
library(caret)#, lib.loc = "~/bsh_ISDM/R_packages")
library(ecospat)#, lib.loc = "~/bsh_ISDM/R_packages")
library(dismo)#, lib.loc = "~/bsh_ISDM/R_packages")
library(fmesher)#, lib.loc = "~/bsh_ISDM/R_packages")
library(sf)
library(terra)
sf_use_s2(FALSE) # need to do this to remove spherical geometry
source(here("scripts", "functions","collinearity.r"))
print(bru_safe_inla())
#INLA:::inla.binary.install(os = "Ubuntu-22.04")


# input GLORYS raster, scale them, and use it to make a polygon of the NWA region
# Not as fine as other NWA shapefile which is better for making mesh in INLA
GLORYS_NWA <- here("data","GLORYS", "GLORYS_2014-09.grd") %>% stack() %>% scale()
GLORYS_NWA <- dropLayer(GLORYS_NWA, c(2,3,5))
#GLORYS_NWA <- projectRaster(GLORYS_NWA, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=laea +lat_0=32.5 +lon_0=-52.5 +x_0=0 +y_0=0 +units=km +no_defs +ellps=WGS84"
GLORYS_NWA <- terra::rast(GLORYS_NWA) # inlabru uses SpatRaster objects

NWA <- as.polygons(GLORYS_NWA > -Inf)
NWA <- NWA %>% sf::st_as_sf("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


## Set the max length of triangles edge
max.edge = 0.8
## Set the length of the boundary extension
bound.outer = 5
bdry <- inla.sp2segment(NWA %>% as('Spatial'))
bdry$loc <- inla.mesh.map(bdry$loc)

mesh1 <- inla.mesh.2d(boundary = bdry, #using the boundry agrument since we have a shapefile
                    max.edge=c(5,8)*max.edge, #specifies the max triangle edge length in inner domain and outer extension ****see how c(3,6) how be like instead*****
                    offset = c(max.edge, bound.outer), #used to set the extension distance
                    cutoff = 0.4  # when using a boarder value is no longer affected by the distance between points but the boundary polygon itself. Thus may be need to reduce cutoff value to achieve a higher the precision of the coastline
                    )

# Following mesh recommendations from:
mesh1 <- fmesher::fm_mesh_2d(boundary = shp,
                    max.edge=c(5,8)*max.edge,
                    offset = c(max.edge, bound.outer),
                    cutoff = 0.4
                    )

mesh1$crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


ggplot() +
fmesher::geom_fm(data = mesh1, fill = "white") +
theme_bw()

ggsave(here("plots","INLAmesh_NorthAtlantic.png"),
       width = 8, height = 5, units = "in", dpi = 300)

