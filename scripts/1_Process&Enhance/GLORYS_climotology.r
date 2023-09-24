library(raster)
library(tidyverse)
library(foreach)
library(doParallel)
library(here)

##################################################################
# climotology for each covariate (i.e. 1 raster for each variable)
##################################################################

GLORYS_NWA_dir<-"D:/GLORYS_NWA/monthly_0.08"

NWA_env_data<-list.files(GLORYS_NWA_dir, full.names = TRUE) %>%
  grep(pattern = ".grd", value = TRUE)
  
#NWA_env_data<-NWA_env_data[1:(length(udates)-4)] # only need from 2003 - 2016

env_cov<-NWA_env_data[1] %>% stack() %>% names() 

for(i in 1:8){ # I could change this so I only pick the variables I want
    env_index <- i
    env_brick <- brick()
    for(ii in 1:length(NWA_env_data)){
        print(paste(env_cov[i],NWA_env_data[ii]))
        ras <- raster(NWA_env_data[ii], band = i)
        env_brick <- addLayer(env_brick, ras)
    }
    beginCluster(n=12)
    f1 <- function(x) calc(x, mean, na.rm=TRUE)
    env_clim <- clusterR(env_brick, f1)
    endCluster()
    names(env_clim)<- raster(NWA_env_data[ii], band = i) %>% names()
    
    if(!exists('clim_stack')){
        clim_stack <- stack()
        clim_stack <- addLayer(clim_stack, env_clim)
        } else {
        clim_stack <- addLayer(clim_stack, env_clim)
        } 
}

clim_stack <- addLayer(clim_stack, raster(NWA_env_data[1], band = 9)) # add bathy
clim_stack <- addLayer(clim_stack, raster(NWA_env_data[1], band = 10)) # add rugosity
names(clim_stack) <- env_cov #just in case
writeRaster(clim_stack, here("data","GLORYS", "GlORYS_clim.grd"), overwrite = TRUE)

here("data","GLORYS", "GlORYS_clim.grd") %>% stack() %>% plot()

# # Now create a stack of the clim hycom data
# NWA_clim_dir<-"E:/GLORYS_NWA/clim_hycom/seasonal_clim/"
# NWA_clim_env<-list.files(NWA_clim_dir, full.names = TRUE) %>%
#   grep(pattern = ".grd", value = TRUE)

# # taking the average over the entire period
# env_clim_stack <- stack()
# for (i in 1:length(NWA_clim_env)){
#   env_stack <- NWA_clim_env[i] %>% stack()
#   env_clim <- calc(env_stack,mean, na.rm = TRUE)
#   env_clim_stack <- addLayer(env_clim_stack, env_clim)
# }


# env_clim_stack <- addLayer(env_clim_stack,
#                            raster(NWA_env_data[1], band = 11), #bathy
#                            raster(NWA_env_data[1], band = 12), #rugosity
#                            raster("E:/GLORYS_NWA/dis_from_seamount_NWA.nc") #distance from seamount
#                            )

# names(env_clim_stack) <- c("eke", "ild", "n2", "ssh_sd", "ssh", "sst_sd", "sst", "bathy", "rugosity", "dis_from_seamount")

# writeRaster(env_clim_stack, paste0("E:/GLORYS_NWA/clim_hycom/NWA_clim_hycom"), overwrite = TRUE)
# # scale the data
# clim_stack_scaled<-scale(env_clim_stack)
# writeRaster(clim_stack_scaled, paste0("E:/GLORYS_NWA/clim_hycom/NWA_clim_scaled_hycom"), , overwrite = TRUE)

######################
# seasonal climatology 
######################
udates<-seq(as.Date("1993-01-01"),as.Date("2019-12-01"), by = "month")

GLORYS_NWA_dir<-"D:/GLORYS_NWA/monthly_0.08"

NWA_env_data<-list.files(GLORYS_NWA_dir, full.names = TRUE) %>%
  grep(pattern = ".grd", value = TRUE)

env_cov<-NWA_env_data[1] %>% stack() %>% names() 

season_indx <- gsubfn::strapplyc(NWA_env_data,  "\\d{4}-\\d{2}", simplify = TRUE)
season_indx <- as.yearqtr(as.yearmon(season_indx, "%Y-%m") + 1/12)
season_indx <- format(season_indx, "%q") %>% as.numeric()

for(i in unique(season_indx)){ 
    env_index <- NWA_env_data[which(season_indx == i)]
    
    env_stack <- stack(env_index)
    
    #lets try to remove bathy and rugosity. no need to take their means
    num_stacks <- nlayers(env_stack) / 10
    env_stack <- dropLayer(env_stack, rep(c(9,10), num_stacks) + (rep(10,num_stacks) * rep(0:(num_stacks-1), each = 2)))
    layer_indx <- rep(1:8, num_layers)
    
    print(paste0("start Q",i))
    beginCluster(n=12)
    env_season <- clusterR(env_stack, stackApply,
                         args=list(indices = layer_indx,
                                   fun = mean,
                                   na.rm=TRUE))
    endCluster()
    
    env_season <- addLayer(env_season, raster(NWA_env_data[1], band = 9)) # add bathy
    env_season <- addLayer(env_season, raster(NWA_env_data[1], band = 10)) # add rugosity
    names(env_season)<- paste0(env_cov,"Q", i)
    
    writeRaster(env_season, paste0(here("data","GLORYS"),"/GLORYS_seasonal_Q",i), overwrite=TRUE)
}