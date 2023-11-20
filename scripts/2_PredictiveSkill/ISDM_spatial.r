# INLA spatial only ISDM
library(tidyverse, lib.loc = "~/bsh_ISDM/R_packages")
library(raster, lib.loc = "~/bsh_ISDM/R_packages")
library(zoo, lib.loc = "~/bsh_ISDM/R_packages")
library(here, lib.loc = "~/bsh_ISDM/R_packages")
library(inlabru, lib.loc = "~/bsh_ISDM/R_packages")
library(INLA, lib.loc = "~/bsh_ISDM/R_packages")
library(sn, lib.loc = "~/bsh_ISDM/R_packages")
library(sf)
library(terra)
sf_use_s2(FALSE) # need to do this to remove spherical geometry
source(here("scripts", "functions","collinearity.r"))
print(bru_safe_inla())
#INLA:::inla.binary.install(os = "Ubuntu-22.04")


# input GLORYS climotology rasters, scale them, and use it to make a polygon of the NWA region
# Not as fine as other NWA shapefile which is better for making mesh in INLA
GLORYS_NWA <- here("data","GLORYS", "GLORYS_clim.grd") %>% stack() %>% scale()
GLORYS_NWA <- dropLayer(GLORYS_NWA, c(2,3,5))
#GLORYS_NWA <- projectRaster(GLORYS_NWA, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #"+proj=laea +lat_0=32.5 +lon_0=-52.5 +x_0=0 +y_0=0 +units=km +no_defs +ellps=WGS84"
GLORYS_NWA <- terra::rast(GLORYS_NWA) # inlabru uses SpatRaster objects

NWA <- as.polygons(GLORYS_NWA > -Inf)
NWA <- NWA %>% sf::st_as_sf("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


### first making all the data pres:abs ratio 1:1 --- using the same method as the BRT script

##########
# bsh etag
##########
etag <- here("data","bsh_data","bsh_etag_enhanced.rds") %>% # or here("data","bsh_data","bsh_etag_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()


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




############
# bsh marker
############
marker <- here("data","bsh_data","bsh_marker_enhanced.rds") %>% # or here("data","bsh_data","bsh_marker_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()
                

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


##############
# bsh observer
##############
observer <- here("data","bsh_data","bsh_observer_enhanced.rds") %>%
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry()

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

# scale environmental data
bsh_all <- bsh_all %>%
               mutate_at(.vars = c("sst", "sss", "ssh", "mld", "log_eke", "sst_sd", "ssh_sd",
                                   "sss_sd", "bathy", "rugosity"),
                         .funs = scale)

# checking for collinearity 
collinearity(na.omit(bsh_all %>% as.data.frame() %>% dplyr::select(sst:rugosity))) #remove sss, ssh, and log_eke maybe? --- I get the same result if I do it by dataset too



#########################################################
# 2 fold cross-validation, repeated 10 times INLA Spatial
#########################################################
# source(here("scripts","functions", "INLA_spatial_skill.r"))

# bsh_ISDM_spatial <- INLA_spatial_skill(dataInput = bsh_all, 
#                             inla.x = c(8,11,13:17), 
#                             inla.y=1,
#                             shp = NWA, 
#                             k_folds = 2, repeats = 10,
#                             cores = 20, n_sampled = 1000)

# saveRDS(bsh_ISDM_spatial, here("results","bsh_ISDM_spatial.rds"))








dataInput = bsh_all
env_rasters = GLORYS_NWA
inla.x = c(8,11,13:17) 
inla.y=1
shp = NWA
k_folds = 2
repeats = 1
cores = 20
n_samples = 1000



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
                      max.edge=c(5,8)*max.edge, #specifies the max triangle edge length in inner domain and outer extension ****see how c(3,6) how be like instead*****
                      offset = c(max.edge, bound.outer), #used to set the extension distance
                      cutoff = 0.4  # when using a boarder value is no longer affected by the distance between points but the boundary polygon itself. Thus may be need to reduce cutoff value to achieve a higher the precision of the coastline
                      )

mesh1$crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# create SPDE object
spde <- inla.spde2.matern(mesh1, alpha=2)


i =1
set.seed(i*150) # for reproducability
dataInput$Kset <- dismo::kfold(dataInput, k_folds) # randomly allocate k groups

# separate train by dataset
train_etag <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "etag" & dataInput$pres_abs == 1,] %>% as.data.frame() %>% sample_n(1000)
train_marker <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "marker" & dataInput$pres_abs == 1,] %>% as.data.frame() %>% sample_n(1000)
train_observer <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "observer",] %>% as.data.frame() %>% sample_n(1000)

test <- dataInput[dataInput$Kset==1,] %>% as.data.frame() # testing data should be all the datasets

train <- rbind(train_etag, train_marker, train_observer)

################################
# Smoothing Covariates with SPDE
################################
#sst
mesh_1d_sst <- inla.mesh.1d(seq(train$sst %>% min() %>% round(), train$sst %>% max() %>% round(), length.out = 20), 
                            boundary = "free",
                            degree = 1) # quadratic (default)
sst_spde <- inla.spde2.matern(mesh_1d_sst)
#mld
mesh_1d_mld <- inla.mesh.1d(seq(train$mld %>% min() %>% round(), train$mld %>% max() %>% round(), length.out = 20), 
                            boundary = "free",
                            degree = 1) 
mld_spde <- inla.spde2.matern(mesh_1d_mld)
#sst_sd
mesh_1d_sst_sd <- inla.mesh.1d(seq(train$sst_sd %>% min() %>% round(), train$sst_sd %>% max() %>% round(), length.out = 20), 
                            boundary = "free",
                            degree = 1) 
sst_sd_spde <- inla.spde2.matern(mesh_1d_sst_sd)
#ssh_sd
mesh_1d_ssh_sd <- inla.mesh.1d(seq(train$ssh_sd %>% min() %>% round(), train$ssh_sd %>% max() %>% round(), length.out = 20), 
                            boundary = "free",
                            degree = 1) 
ssh_sd_spde <- inla.spde2.matern(mesh_1d_ssh_sd)
#sss_sd
mesh_1d_sss_sd <- inla.mesh.1d(seq(train$sss_sd %>% min() %>% round(), train$sss_sd %>% max() %>% round(), length.out = 20), 
                            boundary = "free",
                            degree = 1) 
sss_sd_spde <- inla.spde2.matern(mesh_1d_sss_sd)
#bathy
mesh_1d_bathy <- inla.mesh.1d(seq(train$bathy %>% min() %>% round(), train$bathy %>% max() %>% round(), length.out = 20), 
                            boundary = "free",
                            degree = 1) 
bathy_spde <- inla.spde2.matern(mesh_1d_bathy)
#rugosity
mesh_1d_rugosity <- inla.mesh.1d(seq(train$rugosity %>% min() %>% round(), train$rugosity %>% max() %>% round(), length.out = 20), 
                            boundary = "free",
                            degree = 1) 
rugosity_spde <- inla.spde2.matern(mesh_1d_rugosity)


###############
# Model Formula
###############

form <-  ~ -1 + 
        # intercept_observer(1) + # observer intercept (dataset-specific)
        # intercept_marker(1) + # marker intercept (dataset-specific)
        # intercept_etag(1) + # etag intercept (dataset-specific)
        sst(sst, model = sst_spde) + 
        # mld(mld, model = mld_spde) +
        sst_sd(sst_sd, model = sst_sd_spde) +
        # ssh_sd(ssh_sd, model = ssh_sd_spde) +
        # sss_sd(sss_sd, model = sss_sd_spde) +
        bathy(bathy, model = bathy_spde) +
        # rugosity(rugosity, model = rugosity_spde) +
        etag_field(geometry, model = spde) + 
        marker_field(geometry, copy = "etag_field", fixed = FALSE) + # fixed = F means we want to scale the field by an estimated scalar parameter
        observer_field(geometry, copy = "etag_field", fixed = FALSE)
        

#####################
# Set the likelihoods
#####################

# need to be in sf or sp format
train_etag <- train_etag %>% sf::st_as_sf(coords = c("lon", "lat"),
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
train_marker <- train_marker %>% sf::st_as_sf(coords = c("lon", "lat"),
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
train_observer <- train_observer %>% sf::st_as_sf(coords = c("lon", "lat"),
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
test <- test  %>% sf::st_as_sf(coords = c("lon", "lat"),
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# like_etag <- like(family = "binomial",
#                   formula = pres_abs ~ sst + bathy + sst_sd + etag_field,
#                   data = train_etag,
#                   samplers = NWA,
#                   domain = list(geometry = mesh1),
#                 #   exclude = c("Intercept_marker", "Intercept_observer", 
#                 #               "marker_field", "observer_field", 
#                 #               "sst")
#                   )

# like_marker <- like(family = "binomial",
#                     formula = pres_abs ~ sst + bathy + sst_sd + marker_field,
#                     data = train_marker,
#                     samplers = NWA,
#                     domain = list(geometry = mesh1),
#                     # exclude = c("Intercept_etag", "Intercept_observer", 
#                     #             "etag_field", "observer_field", 
#                     #             "sst")
#                     )

# like_observer <- like(family = "binomial",
#                     formula = pres_abs ~ sst + bathy + sst_sd + observer_field,
#                     data = train_observer,
#                     # exclude = c("Intercept_etag", "Intercept_marker", 
#                     #             "etag_field", "marker_field", 
#                     #             "sst_lgcp")
#                     )
# like_all <- like_list(like_etag, like_marker, like_observer)

# ###############
# # Model Fitting
# ###############
# #inla.setOption("num.threads", cores)

# t1 <- Sys.time()
# out.inla <- bru(
#     components = form,
#     like_all,
#     options = 
#       list(bru_max_iter = 25,
#            bru_verbose = TRUE,
#           #  control.family = list(link = "logit"),
#            control.inla = list(int.strategy = "eb", strategy = "gaussian"),
#            control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE),
#            num.threads = 12
#            )
#       )

# ##################################
# # Prediction & Preformance Metrics
# ##################################
# # preds_bru <- predict(out.inla, data = test, formula = ~ plogis(sst + mld + sst_sd + ssh_sd + sss_sd + bathy + rugosity + etag_field + marker_field + observer_field), 
# #                      n.samples = n_samples, num.threads = cores)
# t2 <- Sys.time()
# t3 <- t2-t1 #curious how each fold takes


# # # calculate and return performance metrics on test data
# # # Get predicted and observed
# # preds <- preds_bru$mean
# # #obs <- train %>% st_drop_geometry() %>% pull(inla.y)

# print(t3)
# print(summary(out.inla))
# saveRDS(out.inla,here("results","bsh_ISDM_spatial.rds"))

# print(mean(preds_bru$mean,na.rm=TRUE))


# out.inla <- here("results","bsh_ISDM_spatial.rds") %>% readRDS()
# summary(out.inla)

p1 <- bind_rows(out.inla$summary.random, .id = "variable") %>% filter(variable %in% c("sst","sst_sd", "bathy")) %>% 
rename("Value" = "ID") %>% 
ggplot() +
geom_line(aes(x = Value, y = mean)) +
facet_wrap(~ variable, scales = "free")+
theme_bw()
p1

ggplot() +
  gg(mesh1,
    mask = as(NWA,'Spatial'),
    col = exp(out.inla$summary.random$marker_field$mean)
  )

# xx <- here("results","bsh_ISDM_spatial_sst_bathy_normal.rds") %>% readRDS()
# summary(xx)

# p1 <- bind_rows(xx$summary.random, .id = "variable") %>% filter(variable %in% c("sst", "bathy")) %>% 
# rename("Value" = "ID") %>% 
# ggplot() +
# geom_line(aes(x = Value, y = mean)) +
# facet_wrap(~ variable, scales = "free_x")+
# theme_bw()
# p1

# yy <- here("results","bsh_ISDM_spatial_sst_bathy_copy.rds") %>% readRDS()
# summary(yy)

# p2 <- bind_rows(yy$summary.random, .id = "variable") %>% #filter(variable %in% c("sst", "bathy")) %>% 
# rename("Value" = "ID") %>% 
# ggplot() +
# geom_line(aes(x = Value, y = mean)) +
# facet_wrap(~ variable, scales = "free")+
# theme_bw()
# p2

# zz <- here("results","bsh_ISDM_spatial_sst_bathy_norm2.rds") %>% readRDS()
# summary(zz)

# p3 <- bind_rows(zz$summary.random, .id = "variable") %>% filter(variable %in% c("sst", "bathy")) %>% 
# rename("Value" = "ID",
#        "Effect" = "mean") %>% 
# ggplot() +
# geom_line(aes(x = Value, y = Effect)) +
# facet_wrap(~ variable, scales = "free")+
# theme_bw()
# p3

out.inla2 <- here("results","bsh_ISDM_spatial.rds") %>% readRDS()
preds_bru <- predict(out.inla, data = test %>% dplyr::select(sst,bathy), formula = ~ plogis(sst + bathy + etag_field + marker_field + observer_field), 
                           n.samples = 100, num.threads = 12)
print(preds_bru$mean %>% mean())
print(preds_bru$sd %>% mean())
