# INLA spatial only ISDM
library(tidyverse, lib.loc = "~/bsh_ISDM/R_packages")
library(raster, lib.loc = "~/bsh_ISDM/R_packages")
library(zoo, lib.loc = "~/bsh_ISDM/R_packages")
library(here, lib.loc = "~/bsh_ISDM/R_packages")
library(inlabru, lib.loc = "~/bsh_ISDM/R_packages")
library(INLA, lib.loc = "~/bsh_ISDM/R_packages")
library(sf)
sf_use_s2(FALSE) # need to do this to remove spherical geometry
source(here("scripts", "functions","collinearity.r"))
print(bru_safe_inla())
#INLA:::inla.binary.install(os = "Ubuntu-22.04")

# load NWA shapefile
NWA <- here("data","shapefiles","NWA.shp") %>% 
        sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

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
inla.x = c(8,11,13:17) 
inla.y=1
fam = "binomial"
shp = NWA
k_folds = 2
repeats = 1
cores = 12
n_samples = 1000





# make data frames for metrics
Evaluations_kfold_INLA <- as.data.frame(matrix(data=0, nrow = (k_folds*4) * repeats, ncol = 16))
colnames(Evaluations_kfold_INLA) <- c("k","WAIC","DIC","CPO", "R_squared", "AUC", "TSS", "MAE","Bias","Sensitivity","Specificity","FalsePos","FalseNeg","repeat", "eval", "time") 
counter=1

################################################
# make boundary from the shapefile and make mesh
# same mesh will be applied for all iterations
################################################

bdry <- inla.sp2segment(shp %>% as('Spatial'))
bdry$loc <- inla.mesh.map(bdry$loc)

mesh1 <- inla.mesh.2d(boundary = bdry, #using the boundry agrument since we have a shapefile
                      max.edge=c(4, 6), #specifies the max triangle edge length in inner domain and outer extension ****see how c(3,6) how be like instead*****
                      offset = c(0.5,4), #used to set the extension distance
                      cutoff = 0.72  # when using a boarder value is no longer affected by the distance between points but the boundary polygon itself. Thus may be need to reduce cutoff value to achieve a higher the precision of the coastline
                      )
mesh1$crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# ### trying the barrier model ###
# # number of triangles of the mesh
# tl <- mesh1$graph$tv[,1] %>% length()

# ## Initialize a matrix containing the central coordinates of each triangle's
# posTri <- matrix(data = 0, nrow = tl, ncol = 2)

# for(t in 1:tl){
#   # Take the vertex of the triangles
#   temp <- mesh1$loc[mesh1$graph$tv[t,],] 
  
#   # Compute center of each triangle
#   posTri[t,] <- colMeans(temp)[c(1,2)]
# }

# # Convert to spatial points
# posTri <- SpatialPoints(posTri)
# crs(posTri) <- crs(NWA %>% as('Spatial')) # have to make sure they have the same crs


# # Intersection between mesh points and sea points contained in NWA
# normal <- over(NWA %>% as('Spatial'), posTri, returnList = TRUE)

# # Remove the sea triangles from all triangles to obtain the barrier ones
# normal <- unlist(normal)
# barrier.triangles <- setdiff(1:tl, normal)

# # Build a barrier, this obj contains all the polygons composing the islands
# # this will be used just for plotting
# poly.barrier <- inla.barrier.polygon(mesh1, barrier.triangles)
# plot(poly.barrier)

# mesh1$n
# ggplot()+
# geom_sf(data = shp, fill = '#4ebce0', linewidth = 1)+
# gg(mesh1) +
# labs(title = " max.edge=c(4, 6) and cuoff = 0.75")


# create SPDE object
spde <- inla.spde2.matern(mesh1, alpha=2)

i =1
set.seed(i*150) # for reproducability
dataInput$Kset <- dismo::kfold(dataInput, k_folds) # randomly allocate k groups

# separate train by dataset
train_etag <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "etag",] %>% as.data.frame()
train_marker <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "marker",] %>% as.data.frame()
train_observer <- dataInput[dataInput$Kset!=1 & dataInput$dataset == "observer",] %>% as.data.frame()

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
covar <- train_etag %>% dplyr::select(all_of(inla.x)) %>% colnames()
res <- train_etag %>% dplyr::select(all_of(inla.y)) %>% colnames()

# form <- as.formula(paste0(res, " ~ -1 + ", "Intercept_etag(1) + ", "Intercept_marker(1) + ", "Intercept_observer(1) + ",
#     paste(covar, collapse = " + "),
#     " + etag_field(geometry, model = spde)",
#     " + marker_field(geometry, copy = 'etag_field', fixed = FALSE)",
#     " + observer_field(geometry, copy = 'etag_field', fixed = FALSE)"))


form <- formula(pres_abs ~ -1 + 
        intercept_observer(1) + # observer intercept (dataset-specific)
        intercept_marker(1) + # marker intercept (dataset-specific)
        intercept_etag(1) + # etag intercept (dataset-specific)
        sst(sst, model = sst_spde) + 
        mld(mld, model = mld_spde) + 
        sst_sd(sst_sd, model = sst_sd_spde) + 
        ssh_sd(ssh_sd, model = ssh_sd_spde) + 
        sss_sd(sss_sd, model = sss_sd_spde) + 
        bathy(bathy, model = bathy_spde) + 
        rugosity(rugosity, model = rugosity_spde) + 
        etag_field(geometry, model = spde) + 
        marker_field(geometry, copy = "etag_field", fixed = FALSE) +
        observer_field(geometry, copy = "etag_field", fixed = FALSE))

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


like_etag <- like(family = fam,
                  formula = pres_abs ~ .,
                  data = train_etag,
                  #include = c("Intercept_etag","sst","mld","sst_sd","ssh_sd","sss_sd","bathy","rugosity","etag_field"),
                  exclude = c("Intercept_marker", "Intercept_observer", "marker_field", "observer_field"),
                  #Ntrials = 1
                  )

like_marker <- like(fam,
                    formula = pres_abs ~ .,
                    data = train_marker,
                    #include = c("Intercept_marker","sst","mld","sst_sd","ssh_sd","sss_sd","bathy","rugosity","marker_field"),
                    exclude = c("Intercept_etag", "Intercept_observer", "etag_field", "observer_field"),
                    #Ntrials = 1
                    )

like_observer <- like(fam,
                    formula = pres_abs ~ .,
                    data = train_observer,
                    #include = c("Intercept_observer","sst","mld","sst_sd","ssh_sd","sss_sd","bathy","rugosity","observer_field"),
                    exclude = c("Intercept_etag", "Intercept_marker","etag_field", "marker_field"),
                    #Ntrials = 1
                    )


###############
# Model Fitting
###############
#inla.setOption("num.threads", cores)

t1 <- Sys.time()
out.inla <- bru(
    components = form,
    like_etag,
    like_marker,
    like_observer,
    #data = train,
    options = 
      list(
          bru_verbose = TRUE,
          #control.family = list(link = "logit"),
          control.inla = list(int.strategy = "eb", strategy = "gaussian"),
          control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE,config = TRUE),
          num.threads = cores
      )
)
t2 <- Sys.time()
t3 <- t2-t1
#saveRDS(out.inla,here("results","INLA_spatial_rw2covariates.rds")) 
print(summary(out.inla))
print(sessionInfo())
saveRDS(out.inla,here("results","INLA_spatial_spde.rds"))


# p1 <- bind_rows(out.inla$summary.random, .id = "variable") %>% filter(variable %in% covar) %>% 
# rename("Value" = "ID") %>% 
# ggplot() +
# geom_line(aes(x = Value, y = mean)) +
# facet_wrap(~ variable, scales = "free")+
# theme_bw()

# ggsave(p1, here("results","response_plot_INLA_spatial_spde.png"))
