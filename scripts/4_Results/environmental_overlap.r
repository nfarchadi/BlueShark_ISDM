# Exploring environmental overlap among  

library(tidyverse)
library(zoo)
library(here)
library(sf)
sf_use_s2(FALSE) # need to do this to remove spherical geometry


### first making all the data pres:abs ratio 1:1 --- making the same the same as the models

##########
# bsh etag
##########
etag <- here("data","bsh_data","bsh_etag_enhanced.rds") %>% # or here("data","bsh_data","bsh_etag_enhanced_seasonal.rds")
            readRDS() %>%
            dplyr::select(-X,-Y) %>% 
            sf::st_drop_geometry() %>% 
            unique()


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
            sf::st_drop_geometry() %>% 
            unique()
                

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
            sf::st_drop_geometry() %>% 
            unique()

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

#######################################################################
# Environmental overlap for SST, SST SD, and Bathy among the data types
#######################################################################

bsh_all %>% 
filter(pres_abs == 1) %>% 
dplyr::select(year, sst, sst_sd, bathy, dataset)  %>% 
mutate(dataset = case_when(dataset == "etag" ~ "Electronic Tag",
                                dataset == "marker" ~ "Marker",
                                dataset == "observer" ~ "Observer"),
       dataset = factor(dataset, levels = c("Marker", "Observer", "Electronic Tag"))) %>% 
rename("SST" = "sst", "SST_sd" = "sst_sd", "Bathymetry" = "bathy") %>% 
gather(variable, value, -year, -dataset) %>%
ggplot() +
geom_density(aes(x = value, fill = dataset), alpha = 0.4) +
facet_wrap(~variable, scales = "free", nrow = 1) +
theme_bw() +
labs(y = "", x = "Variable Values", fill  = "Data Type") +
scale_fill_manual(values = c('#7CAE00','#00BFC4','#C77CFF')) +
theme_bw()
# theme(axis.title=element_text(size=20),
#       axis.text=element_text(size=10),
#       strip.text.x = element_text(size=15))


ggsave(here("plots","EnvironmentalOverlap.png"),
       width = 15, height = 7, units = "in", dpi = 300)
