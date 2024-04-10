# # repeated k fold cross validation function for spatiotemporal ISDM

INLA_spatiotemporal_skill <- function(dataInput, inla.x, inla.y, shp, k_folds = 5, repeats = 10, cores = 1, n_samples = 1000){

    # make data frames for metrics
    Evaluations_kfold_INLA <- as.data.frame(matrix(data=0, nrow = (k_folds*4) * repeats, ncol = 17))
    colnames(Evaluations_kfold_INLA) <- c("k","WAIC","DIC","CPO", "R_squared", "AUC", "TSS", "MAE","Bias","Sensitivity","Specificity","FalsePos","FalseNeg","Boyce","repeat", "eval", "time") 
    counter=1


    ################################################
    # make boundary from the shapefile and make mesh
    # same mesh will be applied for all iterations
    ################################################

    ## Set the max length of triangles edge
    max.edge = 0.8
    ## Set the length of the boundary extension
    bound.outer = 5
    bdry <- inla.sp2segment(shp %>% as('Spatial'))
    bdry$loc <- inla.mesh.map(bdry$loc)

    mesh1 <- inla.mesh.2d(boundary = bdry, #using the boundry agrument since we have a shapefile
                        max.edge=c(5,8)*max.edge, #specifies the max triangle edge length in inner domain and outer extension ****see how c(3,6) how be like instead*****
                        offset = c(max.edge, bound.outer), #used to set the extension distance
                        cutoff = 0.4  # when using a boarder value is no longer affected by the distance between points but the boundary polygon itself. Thus may be need to reduce cutoff value to achieve a higher the precision of the coastline
                        )

    mesh1$crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    # create SPDE object
    spde <- inla.spde2.matern(mesh1, alpha=2)
    
    print(paste0("test skill"))
    for (i in 1:repeats){
        set.seed(i*150) # for reproducability
        dataInput$Kset <- dismo::kfold(dataInput, k_folds) # randomly allocate k groups
        
        for (k in 1:k_folds){
            # separate train by dataset
            train_etag <- dataInput[dataInput$Kset!=k & dataInput$dataset == "etag",] %>% as.data.frame() %>% arrange(season)
            train_marker <- dataInput[dataInput$Kset!=k & dataInput$dataset == "marker",] %>% as.data.frame() %>% arrange(season)
            train_observer <- dataInput[dataInput$Kset!=k & dataInput$dataset == "observer",] %>% as.data.frame() %>% arrange(season)

            train <- rbind(train_etag, train_marker, train_observer)
            test <- dataInput[dataInput$Kset==k,] %>% rbind(., train) %>% as.data.frame() # testing data should be all the datasets
            
            # number of groups for spatial index and projection (A) matrix (automatically made in inlabru)
            n_seasons_etag <- length(unique(train_etag[,"season"]))
            n_seasons_marker <- length(unique(train_marker[,"season"]))
            n_seasons_observer <- length(unique(train_observer[,"season"]))
            
            # sea <- train %>% dplyr::select(season) %>% colnames()
      
            ###############
            # Model Formula
            ###############
            form <-  ~ -1 + 
                    intercept_etag(1) + # etag intercept (dataset-specific)
                    intercept_marker(1) + # marker intercept (dataset-specific)
                    intercept_observer(1) + # observer intercept (dataset-specific)
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
            mesh_1d_sst <- inla.mesh.1d(seq(train$sst %>% min() %>% round(), train$sst %>% max() %>% round(), length.out = 20), 
                                        boundary = "free",
                                        degree = 1) # quadratic (default)
            sst_spde <- inla.spde2.matern(mesh_1d_sst)
            #mld
            # mesh_1d_mld <- inla.mesh.1d(seq(train$mld %>% min() %>% round(), train$mld %>% max() %>% round(), length.out = 20), 
            #                             boundary = "free",
            #                             degree = 1) 
            # mld_spde <- inla.spde2.matern(mesh_1d_mld)
            #sst_sd
            mesh_1d_sst_sd <- inla.mesh.1d(seq(train$sst_sd %>% min() %>% round(), train$sst_sd %>% max() %>% round(), length.out = 20), 
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
            mesh_1d_bathy <- inla.mesh.1d(seq(train$bathy %>% min() %>% round(), train$bathy %>% max() %>% round(), length.out = 20), 
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
            train_etag <- train_etag %>% sf::st_as_sf(coords = c("lon", "lat"),
                                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
            train_marker <- train_marker %>% sf::st_as_sf(coords = c("lon", "lat"),
                                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
            train_observer <- train_observer %>% sf::st_as_sf(coords = c("lon", "lat"),
                                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
            test <- test  %>% sf::st_as_sf(coords = c("lon", "lat"),
                                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

            like_etag <- like(family = "binomial",
                                formula = pres_abs ~ intercept_etag + sst + sst_sd + bathy + etag_field,
                                data = train_etag,
                                
                                samplers = shp,
                                domain = list(geometry = mesh1),
                            #   exclude = c("Intercept_marker", "Intercept_observer", 
                            #               "marker_field", "observer_field", 
                            #               "sst")
                                )

            like_marker <- like(family = "binomial",
                                formula = pres_abs ~ intercept_marker + sst + sst_sd + bathy + marker_field,
                                data = train_marker,
                                samplers = shp,
                                domain = list(geometry = mesh1),
                                # exclude = c("Intercept_etag", "Intercept_observer", 
                                #             "etag_field", "observer_field", 
                                #             "sst")
                                )

            like_observer <- like(family = "binomial",
                                formula = pres_abs ~ intercept_observer + sst + sst_sd + bathy + observer_field,
                                data = train_observer,
                                # exclude = c("Intercept_etag", "Intercept_marker", 
                                #             "etag_field", "marker_field", 
                                #             "sst_lgcp")
                                )
            like_all <- like_list(like_etag, like_marker, like_observer)
        
        
        
            ###############
            # Model Fitting
            ###############
            t1 <- Sys.time()
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

      
            ##################################
            # Prediction & Preformance Metrics
            ##################################
            preds_bru <- predict(out.inla, data = test %>% dplyr::select(sst,sst_sd,bathy, season), formula = ~ data.frame(season = season, lambda = plogis(intercept_etag + intercept_marker + intercept_observer + sst + sst_sd + bathy + etag_field + marker_field + observer_field)), 
                                n.samples = n_samples, num.threads = cores)
            t2 <- Sys.time()
            t3 <- difftime(t2,t1, units = c("mins")) #curious how each fold takes      
            
            # calculate and return performance metrics on test data
            # Get predicted and observed
            preds <- preds_bru$mean
            obs <- train %>% st_drop_geometry() %>% pull(inla.y)
            
            # R-sqaured
            R2 <- 1 - sum((obs - preds[which(test$Kset!=k)])^2)/sum((obs - mean(obs))^2)
            
            
            # etag evaluation
            etag_bru <- preds[which(test$dataset == "etag" & test$Kset==k)]
            
            d <- cbind(test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y), etag_bru)
            pres_y <- as.numeric(d[d[,1] == 1,2])
            abs_y <- as.numeric(d[d[,1] == 0,2])
            e <- dismo::evaluate(p = pres_y, a = abs_y)
            
            Evaluations_kfold_INLA[counter,1] <- k
            Evaluations_kfold_INLA[counter,2] <- out.inla$waic[1]
            Evaluations_kfold_INLA[counter,3] <- out.inla$dic[1]
            Evaluations_kfold_INLA[counter,4] <- out.inla$cpo$cpo %>% mean(na.rm=TRUE)
            Evaluations_kfold_INLA[counter,5] <- R2
            Evaluations_kfold_INLA[counter,6] <- e@auc
            Evaluations_kfold_INLA[counter,7] <- max(e@TPR + e@TNR-1)
            Evaluations_kfold_INLA[counter,8] <- Metrics::mae(test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y), etag_bru)
            Evaluations_kfold_INLA[counter,9] <- Metrics::bias(test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y), etag_bru)
            Evaluations_kfold_INLA[counter,10] <- caret::sensitivity(factor((test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y))),factor(round(etag_bru)))
            Evaluations_kfold_INLA[counter,11] <- caret::specificity(factor((test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y))),factor(round(etag_bru)))
            Evaluations_kfold_INLA[counter,12] <- mean(etag_bru[test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y)==0])
            Evaluations_kfold_INLA[counter,13] <- mean(etag_bru[test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y)==1])
            Evaluations_kfold_INLA[counter,14] <- ecospat::ecospat.boyce(etag_bru, etag_bru[test %>% filter(dataset == "etag" & Kset==k) %>% pull(inla.y)==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
            Evaluations_kfold_INLA[counter,15] <- i
            Evaluations_kfold_INLA[counter,16] <- "etag"
            Evaluations_kfold_INLA[counter,17] <- t3
            counter=counter+1 
            
            
            # marker evaluation
            marker_bru <- preds[which(test$dataset == "marker" & test$Kset==k)]
            
            d <- cbind(test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y), marker_bru)
            pres_y <- as.numeric(d[d[,1] == 1,2])
            abs_y <- as.numeric(d[d[,1] == 0,2])
            e <- dismo::evaluate(p = pres_y, a = abs_y)
            
            Evaluations_kfold_INLA[counter,1] <- k
            Evaluations_kfold_INLA[counter,2] <- out.inla$waic[1]
            Evaluations_kfold_INLA[counter,3] <- out.inla$dic[1]
            Evaluations_kfold_INLA[counter,4] <- out.inla$cpo$cpo %>% mean(na.rm=TRUE)
            Evaluations_kfold_INLA[counter,5] <- R2
            Evaluations_kfold_INLA[counter,6] <- e@auc
            Evaluations_kfold_INLA[counter,7] <- max(e@TPR + e@TNR-1)
            Evaluations_kfold_INLA[counter,8] <- Metrics::mae(test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y), marker_bru)
            Evaluations_kfold_INLA[counter,9] <- Metrics::bias(test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y), marker_bru)
            Evaluations_kfold_INLA[counter,10] <- caret::sensitivity(factor((test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y))),factor(round(marker_bru)))
            Evaluations_kfold_INLA[counter,11] <- caret::specificity(factor((test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y))),factor(round(marker_bru)))
            Evaluations_kfold_INLA[counter,12] <- mean(marker_bru[test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y)==0])
            Evaluations_kfold_INLA[counter,13] <- mean(marker_bru[test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y)==1])
            Evaluations_kfold_INLA[counter,14] <- ecospat::ecospat.boyce(marker_bru, marker_bru[test %>% filter(dataset == "marker" & Kset==k) %>% pull(inla.y)==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
            Evaluations_kfold_INLA[counter,15] <- i
            Evaluations_kfold_INLA[counter,16] <- "marker"
            Evaluations_kfold_INLA[counter,17] <- t3
            counter=counter+1 
            
            
            # observer evaluation
            observer_bru <- preds[which(test$dataset == "observer" & test$Kset==k)]
            
            d <- cbind(test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y), observer_bru)
            pres_y <- as.numeric(d[d[,1] == 1,2])
            abs_y <- as.numeric(d[d[,1] == 0,2])
            e <- dismo::evaluate(p = pres_y, a = abs_y)
            
            Evaluations_kfold_INLA[counter,1] <- k
            Evaluations_kfold_INLA[counter,2] <- out.inla$waic[1]
            Evaluations_kfold_INLA[counter,3] <- out.inla$dic[1]
            Evaluations_kfold_INLA[counter,4] <- out.inla$cpo$cpo %>% mean(na.rm=TRUE)
            Evaluations_kfold_INLA[counter,5] <- R2
            Evaluations_kfold_INLA[counter,6] <- e@auc
            Evaluations_kfold_INLA[counter,7] <- max(e@TPR + e@TNR-1)
            Evaluations_kfold_INLA[counter,8] <- Metrics::mae(test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y), observer_bru)
            Evaluations_kfold_INLA[counter,9] <- Metrics::bias(test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y), observer_bru)
            Evaluations_kfold_INLA[counter,10] <- caret::sensitivity(factor((test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y))),factor(round(observer_bru)))
            Evaluations_kfold_INLA[counter,11] <- caret::specificity(factor((test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y))),factor(round(observer_bru)))
            Evaluations_kfold_INLA[counter,12] <- mean(observer_bru[test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y)==0])
            Evaluations_kfold_INLA[counter,13] <- mean(observer_bru[test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y)==1])
            Evaluations_kfold_INLA[counter,14] <- ecospat::ecospat.boyce(observer_bru, observer_bru[test %>% filter(dataset == "observer" & Kset==k) %>% pull(inla.y)==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
            Evaluations_kfold_INLA[counter,15] <- i
            Evaluations_kfold_INLA[counter,16] <- "observer"
            Evaluations_kfold_INLA[counter,17] <- t3
            counter=counter+1 
            
            
            # all evaluation
            d <- cbind(test %>% filter(Kset==k) %>% pull(inla.y), preds[which(test$Kset==k)])
            pres_y <- as.numeric(d[d[,1] == 1,2])
            abs_y <- as.numeric(d[d[,1] == 0,2])
            e <- dismo::evaluate(p = pres_y, a = abs_y)
            
            Evaluations_kfold_INLA[counter,1] <- k
            Evaluations_kfold_INLA[counter,2] <- out.inla$waic[1]
            Evaluations_kfold_INLA[counter,3] <- out.inla$dic[1]
            Evaluations_kfold_INLA[counter,4] <- out.inla$cpo$cpo %>% mean(na.rm=TRUE)
            Evaluations_kfold_INLA[counter,5] <- R2
            Evaluations_kfold_INLA[counter,6] <- e@auc
            Evaluations_kfold_INLA[counter,7] <- max(e@TPR + e@TNR-1)
            Evaluations_kfold_INLA[counter,8] <- Metrics::mae(test %>% filter(Kset==k) %>% pull(inla.y), preds[which(test$Kset==k)])
            Evaluations_kfold_INLA[counter,9] <- Metrics::bias(test %>% filter(Kset==k) %>% pull(inla.y), preds[which(test$Kset==k)])
            Evaluations_kfold_INLA[counter,10] <- caret::sensitivity(factor((test %>% filter(Kset==k) %>% pull(inla.y))),factor(round(preds[which(test$Kset==k)])))
            Evaluations_kfold_INLA[counter,11] <- caret::specificity(factor((test %>% filter(Kset==k) %>% pull(inla.y))),factor(round(preds[which(test$Kset==k)])))
            Evaluations_kfold_INLA[counter,12] <- mean(preds[test %>% filter(Kset==k) %>% pull(inla.y)==0])
            Evaluations_kfold_INLA[counter,13] <- mean(preds[test %>% filter(Kset==k) %>% pull(inla.y)==1])
            Evaluations_kfold_INLA[counter,14] <- ecospat::ecospat.boyce(preds, preds[test %>% filter(Kset==k) %>% pull(inla.y)==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
            Evaluations_kfold_INLA[counter,15] <- i
            Evaluations_kfold_INLA[counter,16] <- "all"
            Evaluations_kfold_INLA[counter,17] <- t3
            counter=counter+1  
        }
    }
    
    return(Evaluations_kfold_INLA)
}