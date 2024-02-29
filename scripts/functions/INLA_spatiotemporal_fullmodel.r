# # Fitting ISDM spatiotemporal with full dataset

INLA_spatiotemporal_fullmodel <- function(dataInput, inla.x, inla.y, shp, cores = 1, n_samples = 1000){

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
    
    # separate by dataset
    etag <- dataInput[dataInput$dataset == "etag",] %>% as.data.frame() %>% arrange(season)
    marker <- dataInput[dataInput$dataset == "marker",] %>% as.data.frame() %>% arrange(season)
    observer <- dataInput[dataInput$dataset == "observer",] %>% as.data.frame() %>% arrange(season)
      
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
            etag_field(geometry, model = spde, group = season, control.group = list(model = 'ar1', cyclic = TRUE)) + # control.group = list(model = 'ar1', cyclic = TRUE) <- this does not have to be added to the other spatial fields because they are copied
            marker_field(geometry, copy = "etag_field", group = season, fixed = FALSE) + # fixed = F means we want to scale the field by an estimated scalar parameter 
            observer_field(geometry, copy = "etag_field", group = season, fixed = FALSE)
        
                
        
    ################################
    # Smoothing Covariates with SPDE
    ################################
    #sst
    mesh_1d_sst <- inla.mesh.1d(seq(dataInput$sst %>% min() %>% round(), dataInput$sst %>% max() %>% round(), length.out = 20), 
                                boundary = "free",
                                degree = 1) # quadratic (default)
    sst_spde <- inla.spde2.matern(mesh_1d_sst)
    #mld
    # mesh_1d_mld <- inla.mesh.1d(seq(train$mld %>% min() %>% round(), train$mld %>% max() %>% round(), length.out = 20), 
    #                             boundary = "free",
    #                             degree = 1) 
    # mld_spde <- inla.spde2.matern(mesh_1d_mld)
    #sst_sd
    mesh_1d_sst_sd <- inla.mesh.1d(seq(dataInput$sst_sd %>% min() %>% round(), dataInput$sst_sd %>% max() %>% round(), length.out = 20), 
                                boundary = "free",
                                degree = 1) 
    sst_sd_spde <- inla.spde2.matern(mesh_1d_sst_sd)
    # #ssh_sd
    # mesh_1d_ssh_sd <- inla.mesh.1d(seq(train$ssh_sd %>% min() %>% round(), train$ssh_sd %>% max() %>% round(), length.out = 20), 
    #                             boundary = "free",
    #                             degree = 1) 
    # ssh_sd_spde <- inla.spde2.matern(mesh_1d_ssh_sd)
    # #sss_sd
    # mesh_1d_sss_sd <- inla.mesh.1d(seq(train$sss_sd %>% min() %>% round(), train$sss_sd %>% max() %>% round(), length.out = 20), 
    #                             boundary = "free",
    #                             degree = 1) 
    # sss_sd_spde <- inla.spde2.matern(mesh_1d_sss_sd)
    #bathy
    mesh_1d_bathy <- inla.mesh.1d(seq(dataInput$bathy %>% min() %>% round(), dataInput$bathy %>% max() %>% round(), length.out = 20), 
                                boundary = "free",
                                degree = 1) 
    bathy_spde <- inla.spde2.matern(mesh_1d_bathy)
    # #rugosity
    # mesh_1d_rugosity <- inla.mesh.1d(seq(train$rugosity %>% min() %>% round(), train$rugosity %>% max() %>% round(), length.out = 20), 
    #                             boundary = "free",
    #                             degree = 1) 
    # rugosity_spde <- inla.spde2.matern(mesh_1d_rugosity)



    #####################
    # Set the likelihoods
    #####################
    # need to be in sf or sp format
    etag <- etag %>% sf::st_as_sf(coords = c("lon", "lat"),
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    marker <- marker %>% sf::st_as_sf(coords = c("lon", "lat"),
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    observer <- observer %>% sf::st_as_sf(coords = c("lon", "lat"),
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    

    like_etag <- like(family = "binomial",
                        formula = pres_abs ~ sst + sst_sd + bathy + etag_field,
                        data = etag,
                        samplers = NWA,
                        domain = list(geometry = mesh1),
                    #   exclude = c("Intercept_marker", "Intercept_observer", 
                    #               "marker_field", "observer_field", 
                    #               "sst")
                        )

    like_marker <- like(family = "binomial",
                        formula = pres_abs ~ sst + sst_sd + bathy + marker_field,
                        data = marker,
                        samplers = NWA,
                        domain = list(geometry = mesh1),
                        # exclude = c("Intercept_etag", "Intercept_observer", 
                        #             "etag_field", "observer_field", 
                        #             "sst")
                        )

    like_observer <- like(family = "binomial",
                        formula = pres_abs ~ sst + sst_sd + bathy + observer_field,
                        data = observer,
                        # exclude = c("Intercept_etag", "Intercept_marker", 
                        #             "etag_field", "marker_field", 
                        #             "sst_lgcp")
                        )
    like_all <- like_list(like_etag, like_marker, like_observer)
        
        
        
    ###############
    # Model Fitting
    ###############
    
    out.inla <- bru(
        components = form,
        like_all,
        options = 
            list(
                # bru_max_iter = 25,
                bru_verbose = TRUE,
                # control.family = list(link = "logit"),
                # control.inla = list(int.strategy = "eb", strategy = "gaussian"),
                control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE, config = TRUE),
                num.threads = cores
                )
            )
    
    # the marginal effects (ME) data for the covariates 
    ME <- out.inla$summary.random
    
    # projection of the spatial random field
    ppxl <- fm_pixels(mesh1, mask = NWA, dims = c(541, 1141))
    ppxl_all <- fm_cprod(ppxl, data.frame(season = seq_len(4)))
    
    GMRF <- predict(out.inla,
                    ppxl_all, # using the same dims as GLORYS
                    ~ etag_field,
                    n.samples = n_samples, 
                    num.threads = cores
                    )

    ISDM_spatial_ME_GMRF <- list(ME, GMRF)

    return(ISDM_spatial_ME_GMRF)
}