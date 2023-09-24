# INLA spatial only ISDM
library(tidyverse)#, lib.loc = "~/bsh_ISDM/R_packages")
library(raster)#, lib.loc = "~/bsh_ISDM/R_packages")
library(zoo)#, lib.loc = "~/bsh_ISDM/R_packages")
library(here)#, lib.loc = "~/bsh_ISDM/R_packages")
#library(inlabru)#, lib.loc = "~/bsh_ISDM/R_packages")
library(INLA)#, lib.loc = "~/bsh_ISDM/R_packages")
library(sf)
library(terra)
library(rgeos)
sf_use_s2(FALSE) # need to do this to remove spherical geometry
source(here("scripts", "functions","collinearity.r"))
print(bru_safe_inla())
#INLA:::inla.binary.install(os = "Ubuntu-22.04")


# input GLORYS climotology rasters, scale them, and use it to make a polygon of the NWA region
GLORYS_NWA <- here("data","GLORYS", "GLORYS_clim.grd") %>% stack() %>% scale()
GLORYS_NWA <- dropLayer(GLORYS_NWA, c(2,3,5))
#GLORYS_NWA <- projectRaster(GLORYS_NWA, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=laea +lat_0=32.5 +lon_0=-52.5 +x_0=0 +y_0=0 +units=km +no_defs +ellps=WGS84"
GLORYS_NWA <- terra::rast(GLORYS_NWA) # inlabru uses SpatRaster objects

NWA <- as.polygons(GLORYS_NWA > -Inf)
NWA <- NWA %>% sf::st_as_sf("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


### first making all the data pres:abs ratio 1:1

##########
# bsh etag
##########
etag <- here("data","bsh_data","bsh_etag_enhanced.rds") %>% 
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
marker <- here("data","bsh_data","bsh_marker_enhanced.rds") %>% 
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
observer <- here("data","bsh_data","bsh_observer_enhanced.rds") %>% 
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

# scale environmental data
bsh_all <- bsh_all %>%
               mutate_at(.vars = c("sst", "sss", "ssh", "mld", "log_eke", "sst_sd", "ssh_sd",
                                   "sss_sd", "bathy", "rugosity"),
                         .funs = scale)

# checking for collinearity 
collinearity(na.omit(bsh_all %>% as.data.frame() %>% dplyr::select(sst:rugosity))) #remove sss, ssh, and log_eke maybe? --- I get the same result if I do it by dataset too


dataInput = bsh_all
env_rasters = GLORYS_NWA
inla.x = c(8,11,13:17) 
inla.y=1
shp = NWA
k_folds = 2
repeats = 1
cores = 12



# make data frames for metrics
Evaluations_kfold_INLA <- as.data.frame(matrix(data=0, nrow = (k_folds*4) * repeats, ncol = 16))
colnames(Evaluations_kfold_INLA) <- c("k","WAIC","DIC","CPO", "R_squared", "AUC", "TSS", "MAE","Bias","Sensitivity","Specificity","FalsePos","FalseNeg","repeat", "eval", "time") 
counter=1



################################################
# make boundary from the shapefile and make mesh
# same mesh will be applied for all iterations
################################################
## Set the max length of triangles edge
max.edge = 0.8
## Set the length of the boundary extension
bound.outer = 5
bdry <- inla.sp2segment(NWA %>% as('Spatial'))
bdry$loc <- inla.mesh.map(bdry$loc)

mesh1 <- inla.mesh.2d(boundary = bdry, #using the boundry agrument since we have a shapefile
                      max.edge=c(5,9)*max.edge, #specifies the max triangle edge length in inner domain and outer extension ****see how c(3,6) how be like instead*****
                      offset = c(max.edge, bound.outer), #used to set the extension distance
                      cutoff = max.edge/2  # when using a boarder value is no longer affected by the distance between points but the boundary polygon itself. Thus may be need to reduce cutoff value to achieve a higher the precision of the coastline
                      )

mesh1$crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# create SPDE object
spde <- inla.spde2.matern(mesh1, alpha=2)


##############################################################
# Extract environmental data at mesh (i.e. integration) points
##############################################################

# get covariates for integration points
# for these we will use a climotological average for each season across the whole time period

env_covariates <- raster::extract(env_rasters, cbind(mesh1$loc[,1], mesh1$loc[,2])) %>%
                  as.data.frame()



###########################                
# create integration points 
###########################
# this part isn't dependent on the training data and only requires the mesh
# will reduce computation if I start this here

dmesh <- book.mesh.dual(mesh1)

w <- sapply(1:length(dmesh), function(i) {
  if (gIntersects(dmesh[i, ], as(NWA,'Spatial')))
    return(gArea(gIntersection(dmesh[i, ], as(NWA,'Spatial'))))
  else return(0)
})


i =1
set.seed(i*150) # for reproducability
dataInput$Kset <- dismo::kfold(dataInput, k_folds) # randomly allocate k groups

# separate train by dataset
train_etag <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "etag" & dataInput$pres_abs == 1,] %>% as.data.frame()
train_marker <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "marker" & dataInput$pres_abs == 1,] %>% as.data.frame()
train_observer <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "observer",] %>% as.data.frame()

test <- dataInput[dataInput$Kset==1,] %>% as.data.frame() # testing data should be all the datasets

train <- rbind(train_etag, train_marker, train_observer)


##############################################################################
# make spatial index, A matrices, &  continuing w/ integration points for etag
##############################################################################

train_etag <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "etag" & dataInput$pres_abs == 1,] %>% as.data.frame()
train_etag_coords <- train_etag[,c("lon","lat")]
n_train_etag <- length(train_etag[,inla.y])

# a spatial index that lists the spatial field index, which runs from 1 to mesh1$n for n_seasons 
# AND a spatial.field group which runs from 1 to n_years with each element replicated mesh1$n times
s_index_etag <- inla.spde.make.index(name = "etag_field",
                                n.spde = mesh1$n)


# projection (A) matrix 
etag_train_A <- inla.spde.make.A(mesh = mesh1,
                            loc = as.matrix(train_etag_coords))

# Exposure for LGCP is the weights of the dmesh replicated by the number of seasons in the time mesh
nv <- mesh1$n

# change data to include 0s for nodes and 1s for presences. 
# only necessary for "unstructured" data types (i.e. PO) - corresponds to y.pp in Suhaimi "unstructured" example
y.pp_etag <- rep(0:1, c(nv, n_train_etag)) ## nv - mesh nodes, n_alb_tag - alb presence

# add expectation vector (area for integration points/nodes and 0 for presences)
e.pp_etag <- c(w, rep(0, n_train_etag)) 

# projection matrices for the integration points and the observation points (2 steps) 
# 1. integration points is a diagonal matrix because these locations are just the mesh vertices
imat <- Diagonal(nv, rep(1, nv))

# 2. for the observed points, another projection is defined which we did earlier (line 112)
# then the entire projection matrix will be
A.pp_etag <- rbind(imat, etag_train_A)


env_etag <- terra::extract(env_rasters, train_etag %>% sf::st_as_sf(coords = c("lon", "lat"),
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) %>%
                  as.data.frame()

# Estimation stack for etag
stk.etag <- inla.stack(
    data = list(y = cbind(y.pp_etag,NA), e = e.pp_etag),
    A = list(1, A.pp_etag),
    effects = list(
        list(intercept_etag = rep(1, nv + n_train_etag),
              # mld = c(env_covariates$mld, train_etag$mld),
              # sss_sd = c(env_covariates$sss_sd, train_etag$sss_sd),
              # ssh_sd = c(env_covariates$ssh_sd, train_etag$ssh_sd),
              sst = c(env_covariates$sst, env_etag$sst)
              # sst_spde_index
              # sst_sd = c(env_covariates$sst_sd, train_etag$sst_sd),
              # bathy = c(env_covariates$bathy, train_etag$bathy),
              # rugosity = c(env_covariates$rugosity, train_etag$rugosity),
              ),
        #list(spatial_field = 1:spde$n.spde)
        s_index_etag
        ),
    tag = "train_etag_stk"
    )


# ################################################################################
# # make spatial index, A matrices, &  continuing w/ integration points for marker
# ################################################################################

# train_marker <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "marker" & dataInput$pres_abs == 1,] %>% as.data.frame()
# train_marker_coords <- train_marker[,c("lon","lat")]
# n_train_marker <- length(train_marker[,inla.y])

# # a spatial index that lists the spatial field index, which runs from 1 to mesh1$n for n_seasons 
# # AND a spatial.field group which runs from 1 to n_years with each element replicated mesh1$n times
# s_index_marker <- inla.spde.make.index(name = "marker_field",
#                                 n.spde = mesh1$n)


# # projection (A) matrix 
# marker_train_A <- inla.spde.make.A(mesh = mesh1,
#                             loc = as.matrix(train_marker_coords))

# # Exposure for LGCP is the weights of the dmesh replicated by the number of seasons in the time mesh
# nv <- mesh1$n

# # change data to include 0s for nodes and 1s for presences. 
# # only necessary for "unstructured" data types (i.e. PO) - corresponds to y.pp in Suhaimi "unstructured" example
# y.pp_marker <- rep(0:1, c(nv, n_train_marker)) ## nv - mesh nodes, n_alb_tag - alb presence

# # add expectation vector (area for integration points/nodes and 0 for presences)
# e.pp_marker <- c(w, rep(0, n_train_marker)) 

# # projection matrices for the integration points and the observation points (2 steps) 
# # 1. integration points is a diagonal matrix because these locations are just the mesh vertices
# imat <- Diagonal(nv, rep(1, nv))

# # 2. for the observed points, another projection is defined which we did earlier (line 112)
# # then the entire projection matrix will be
# A.pp_marker <- rbind(imat, marker_train_A)



##############################################
# make spatial index & A matrices for observer
##############################################
train_observer <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "observer",] %>% as.data.frame()
train_observer_coords <- train_observer[,c("lon","lat")]
n_train_observer <- length(train_observer[,inla.y])

# a spatial index that lists the spatial field index, which runs from 1 to mesh1$n for n_seasons 
# AND a spatial.field group which runs from 1 to n_years with each element replicated mesh1$n times
s_index_observer <- inla.spde.make.index(name = "observer_field",
                                n.spde = mesh1$n)


# projection (A) matrix 
observer_train_A <- inla.spde.make.A(mesh = mesh1,
                            loc = as.matrix(train_observer_coords))


# #############
# # INLA stacks
# #############

# # Estimation stack for etag
# stk.etag <- inla.stack(
#     data = list(y = cbind(y.pp_etag, NA, NA), e = e.pp_etag),
#     A = list(1, A.pp_etag),
#     effects = list(
#         list(intercept_etag = rep(1, nv + n_train_etag),
#               # mld = c(env_covariates$mld, train_etag$mld),
#               # sss_sd = c(env_covariates$sss_sd, train_etag$sss_sd),
#               # ssh_sd = c(env_covariates$ssh_sd, train_etag$ssh_sd),
#               sst = c(env_covariates$sst, train_etag$sst)
#               # sst_sd = c(env_covariates$sst_sd, train_etag$sst_sd),
#               # bathy = c(env_covariates$bathy, train_etag$bathy),
#               # rugosity = c(env_covariates$rugosity, train_etag$rugosity),
#               ),
#         #list(spatial_field = 1:spde$n.spde)
#         s_index_etag
#         ),
#     tag = "train_etag_stk"
#     )

# # Estimation stack for marker
# stk.marker <- inla.stack(
#     data = list(y = cbind(NA, y.pp_marker, NA), e = e.pp_marker),
#     A = list(1, A.pp_marker),
#     effects = list(
#         list(intercept_marker = rep(1, nv + n_train_marker),
#               # mld = c(env_covariates$mld, train_marker$mld),
#               # sss_sd = c(env_covariates$sss_sd, train_marker$sss_sd),
#               # ssh_sd = c(env_covariates$ssh_sd, train_marker$ssh_sd),
#               sst = c(env_covariates$sst, train_marker$sst)
#               # sst_sd = c(env_covariates$sst_sd, train_marker$sst_sd),
#               # bathy = c(env_covariates$bathy, train_marker$bathy),
#               # rugosity = c(env_covariates$rugosity, train_marker$rugosity),
#               ),
#         #list(spatial_field = 1:spde$n.spde)
#         s_index_marker
#         ),
#     tag = "train_marker_stk"
#     )

# Estimation stack for observer
stk.observer <- inla.stack(
    data = list(y = cbind(NA,train_observer[,inla.y]), Ntrials = rep(1, n_train_observer)),
    A = list(1, observer_train_A),
    effects = list(
        list(intercept_observer = rep(1, n_train_observer),
              # mld = train_observer$mld,
              # sss_sd = train_observer$sss_sd,
              # ssh_sd = train_observer$ssh_sd,
              sst = train_observer$sst
              # sst_sd = train_observer$sst_sd,
              # bathy = train_observer$bathy,
              # rugosity = train_observer$rugosity,
              ),
        #list(spatial_field = 1:spde$n.spde)
        s_index_observer
        ),
    tag = "train_observer_stk"
    )


stk <- inla.stack(stk.etag,stk.observer)

################################
# Smoothing Covariates with SPDE
################################
#sst
mesh_1d_sst <- inla.mesh.1d(seq(min(train$sst) %>% floor(), max(train$sst) %>% ceiling(), length.out = 20), 
                            boundary = "free",
                            degree = 1) # quadratic (default)
sst_A <- inla.spde.make.A(mesh_1d_sst, train$sst)
sst_spde <- inla.spde2.matern(mesh_1d_sst)
sst_spde_index <- inla.spde.make.index("sst", n.spde = sst_spde$n.spde)

###############
# Model Formula
###############

# form <-  formula(~ -1 + 
#         intercept_observer(1) + # observer intercept (dataset-specific)
#         intercept_marker(1) + # marker intercept (dataset-specific)
#         intercept_etag(1) + # etag intercept (dataset-specific)
#         f(sst, model = sst_spde) + 
#         f(etag_field, model = spde) + 
#         f(marker_field, copy = "etag_field", fixed = TRUE) + # fixed = F means we want to scale the field by an estimated scalar parameter
#         f(observer_field, copy = "etag_field", fixed = TRUE))

form <-  formula(y ~ -1 + 
        intercept_observer + # observer intercept (dataset-specific)
        #intercept_marker(1) + # marker intercept (dataset-specific)
        intercept_etag + # etag intercept (dataset-specific)
        f(inla.group(sst, n = 20), model = "rw2", constr=FALSE)+
        f(etag_field, model = spde) +
        #f(marker_field, copy = "etag_field", fixed = TRUE) + # fixed = F means we want to scale the field by an estimated scalar parameter
        f(observer_field, copy = "etag_field", fixed = TRUE))

###############
# Model Fitting
###############

out.inla6 <- inla(form,
                 family = c("poisson", "binomial"),
                 data = inla.stack.data(stk),
                 Ntrials = inla.stack.data(stk)$Ntrials,
                 E = inla.stack.data(stk)$e,
                 control.predictor = list(A = inla.stack.A(stk), compute = TRUE),
                 control.compute = list(dic = TRUE, cpo = TRUE,waic = TRUE),
                 control.inla = list(strategy="gaussian", int.strategy="eb"),
                  # control.inla = list(
                  #     strategy='laplace',
                  #     int.strategy = 'ccd'),
                  num.threads = 12,
                  verbose = TRUE,
                  safe = TRUE
                 )

out.inla6 %>% summary()

p1 <- bind_rows(out.inla2$summary.random, .id = "variable") %>% filter(variable %in% 'inla.group(sst, n = 20)') %>% 
rename("Value" = "ID") %>% 
ggplot() +
geom_line(aes(x = Value, y = mean)) +
facet_wrap(~ variable, scales = "free")+
theme_bw()
p1
