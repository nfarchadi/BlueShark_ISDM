library(tidyverse)
library(here)
library(cmocean)

# All BRT and INLA results
BRT_ensemble <- here("results","BRT_ensemble_skill.rds") %>% readRDS()
BRT_pooling <- here("results","BRT_pooling_skill.rds") %>% readRDS()
ISDM_spatial <- here("results","bsh_ISDM_spatial.rds") %>% readRDS()
ISDM_spatiotemporal <- here("results","bsh_ISDM_spatiotemporal.rds") %>% readRDS()

# making sure that each of the dataset have the same columns
BRT_ensemble <- BRT_ensemble %>% dplyr::select(-2) %>% mutate(model = "Ensemble")
BRT_pooling <- BRT_pooling %>% dplyr::select(-2) %>% mutate(model = "Pooling")
ISDM_spatial <- ISDM_spatial %>% dplyr::select(-c(2:4)) %>% mutate(model = "iSDM Constant")
ISDM_spatiotemporal <- ISDM_spatiotemporal %>% dplyr::select(-c(2:4)) %>% mutate(model = "iSDM Seasonal")
colnames(ISDM_spatial) <- colnames(BRT_ensemble)
colnames(ISDM_spatiotemporal) <- colnames(BRT_ensemble)
all <- rbind(BRT_ensemble, BRT_pooling, ISDM_spatial, ISDM_spatiotemporal) %>% 
            mutate(eval = case_when(eval == "etag" ~ "Eletronic Tag",
                                    eval == "marker" ~ "Marker",
                                    eval == "observer" ~ "Observer",
                                    eval == "all" ~ "all"),
                   eval = factor(eval, levels = c("Marker", "Observer", "Eletronic Tag"))) %>% 
            filter(eval != "all")

# view(all)

# AUC across datasets
all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
geom_boxplot(aes(x = model, y = AUC, color = model)) +
theme_bw() +
labs(x="") +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=15),
      strip.text.x = element_text(size=15),
      legend.position = "none") +
scale_color_brewer(palette = "Spectral")

# AUC per dataset
all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
geom_boxplot(aes(x = model, y = AUC, color = model)) +
facet_wrap(~eval) +
theme_bw() +
labs(x="") +
theme(axis.title=element_text(size=20),
      axis.text.x=element_text(size=15,angle = 75, hjust = 1),
      strip.text.x = element_text(size=15),
      legend.position = "none") +
scale_color_brewer(palette = "Spectral")

# MAE across datasets
all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
geom_boxplot(aes(x = model, y = MAE, color = model)) +
theme_bw() +
labs(x="") +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=15),
      strip.text.x = element_text(size=15),
      legend.position = "none" ) +
scale_color_brewer(palette = "Spectral")

# MAE per dataset
all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
geom_boxplot(aes(x = model, y = MAE, color = model)) +
facet_wrap(~eval) +
theme_bw() +
labs(x="") +
theme(axis.title=element_text(size=20),
      axis.text.x=element_text(size=15),
      strip.text.x = element_text(size=15),
      legend.position = "none") +
scale_color_brewer(palette = "Spectral")

############
# AUC vs MAE
############
all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot() +
geom_point(aes(x = MAE, y = AUC, color = model, shape = eval), size = 3) +
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")

ggsave(here("plots","AUCvsMAE_PredictiveSkill.png"),
       width = 10, height = 7, units = "in", dpi = 300)



####################
# Ecological Realism
####################
all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
# geom_boxplot(aes(x = model, y = FalseNeg, color = model)) +
geom_point(aes(x = FalseNeg, y = FalsePos, color = model)) +
theme_bw() +
labs(x="Prediction at Presences", y = "Prediction at Pseudo-absences") +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=15),
      strip.text.x = element_text(size=15),
      legend.position = "none" ) +
scale_color_brewer(palette = "Spectral")



######################
# Computational Demand
######################
all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot(aes(x = time, y = AUC)) +
geom_point(aes(color = model, shape = eval), size = 3) +
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type", x = "Time (minutes)") +
scale_color_brewer(palette = "Spectral") + 
geom_smooth(method='lm', formula= y~x)

ggsave(here("plots","AUCvsMAE_ComputationalDemand.png"),
       width = 10, height = 7, units = "in", dpi = 300)




all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot() +
geom_point(aes(x = MAE, y = AUC, color = eval)) +
theme_bw() +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=15))+
labs(color="")
cmocean("haline")(4)

bsh_all %>%  # had to go to another script to get this but update so that it is in here
group_by(dataset)  %>% 
summarise(Records = n())  %>% 
ggplot() +
geom_bar(aes(x = dataset, y = Records, fill = dataset),
         stat = 'identity') +
theme_bw() +
scale_fill_manual(values = c('#7CAE00','#00BFC4','#C77CFF')) +
labs(x = "") +
theme(legend.position = "none",
      axis.title=element_text(size=20),
      axis.text=element_text(size=15))









####################
# exploring the data
####################
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

# # scale environmental data
# bsh_all <- bsh_all %>%
#                mutate_at(.vars = c("sst", "sss", "ssh", "mld", "log_eke", "sst_sd", "ssh_sd",
#                                    "sss_sd", "bathy", "rugosity"),
#                          .funs = scale)

# add season term
yq <- as.yearqtr(bsh_all$year_mon + 1/12)
bsh_all$season <- format(yq, "%q") %>% as.numeric()

#########################
# Total records over time
#########################

bsh_all %>% 
filter(pres_abs == 1) %>% 
group_by(year, dataset) %>% 
summarise(yearly_obs = n()) %>% 
ggplot() +
geom_line(aes(x=year, y = yearly_obs, color = dataset)) +
theme_bw() +
# facet_wrap(~dataset) +
labs(x = "Year", y = "Total Records", color = "Dataset")+
xlim(1990,2020) + 
scale_color_manual(values = c('#7CAE00','#00BFC4','#C77CFF')) +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=10),
      strip.text.x = element_text(size=15))


#######################################################################
# Environmental overlap for SST, SST SD, and Bathy among the data types
#######################################################################

bsh_all  %>% 
filter(pres_abs == 1) %>% 
dplyr::select(year, season, sst, sst_sd, mld, bathy, dataset)  %>%  
gather(variable, value, -year, -season, -dataset) %>% 
ggplot() +
geom_density(aes(x = value, fill = dataset), alpha = 0.4) +
facet_wrap(~variable, scales = "free") +
theme_bw() +
scale_fill_manual(values = c('#7CAE00','#00BFC4','#C77CFF')) +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=10),
      strip.text.x = element_text(size=15))

bsh_all  %>% 
filter(pres_abs == 1) %>% 
dplyr::select(year, season, sst, sst_sd, mld, bathy, dataset)  %>%  
gather(variable, value, -year, -season, -dataset) %>% 
filter(variable == 'bathy') %>% 
ggplot() +
geom_density(aes(x = value, fill = dataset), alpha = 0.4) +
facet_wrap(~dataset) +
theme_bw() +
labs(x = "Bathy") +
scale_fill_manual(values = c('#7CAE00','#00BFC4','#C77CFF')) +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=10),
      strip.text.x = element_text(size=15))

bsh_all  %>% 
filter(pres_abs == 1) %>% 
dplyr::select(year, season, sst, sst_sd, mld, bathy, dataset)  %>%  
gather(variable, value, -year, -season, -dataset) %>% 
filter(variable == 'mld') %>% 
ggplot() +
geom_density(aes(x = value, fill = dataset), alpha = 0.4) +
facet_wrap(~dataset) +
theme_bw() +
labs(x = "MLD") +
scale_fill_manual(values = c('#7CAE00','#00BFC4','#C77CFF')) +
theme(axis.title=element_text(size=20),
      axis.text=element_text(size=10),
      strip.text.x = element_text(size=15))
