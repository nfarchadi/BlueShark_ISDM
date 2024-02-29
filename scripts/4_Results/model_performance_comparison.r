library(tidyverse)
library(here)
library(cmocean)
library(patchwork)

###########################################
# Metrics from models ran with all the data
###########################################

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
   
            
            
### AUC vs MAE ###
PredSkill <- all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot() +
geom_point(aes(x = MAE, y = AUC, color = model, shape = eval), size = 3) +
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral") +
theme(legend.position = "none")



### Ecological Realism ###
EcoReal <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
# geom_boxplot(aes(x = model, y = FalseNeg, color = model)) +
geom_point(aes(x = FalsePos, y = FalseNeg, color = model, shape = eval), size = 2) +
theme_bw() +
labs(x="Prediction at Pseudo-absences", y = "Prediction at Presences", 
     color = "Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")



### Computational Demand ###
CompDemand <- all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot(aes(x = time, y = AUC)) +
geom_point(aes(color = model, shape = eval), size = 3) +
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type", x = "Time (minutes)") +
scale_color_brewer(palette = "Spectral") +
theme(legend.position = "none")


PredSkill / EcoReal / CompDemand + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(here("plots","ModelPreformanceAnalysis.png"),
       width = 8, height = 10, units = "in", dpi = 300)



### Ecological Realism option B & C ###
EcoReal_ratio <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
# geom_violin(aes(x = model, y = FalseNeg_FalsePos_ratio, color = model)) +
geom_jitter(aes(x = model, y = FalseNeg_FalsePos_ratio, color = model, shape = eval)) +
# geom_point(aes(x = FalsePos, y = FalseNeg, color = model, shape = eval), size = 2) +
theme_bw() +
labs(x="", y = "Ecological Realism (presence : pseduo-absence ratio)", 
     color = "Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")

ggsave(here("plots","EcologicalRealism.png"),
       width = 10, height = 6, units = "in", dpi = 300)

all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%
ggplot() +
geom_point(aes(x = FalseNeg_FalsePos_ratio, y = AUC, color = model, shape = eval), size = 2) +
theme_bw() +
labs(y="AUC", x = "Ecological Realism (presence : pseduo-absence ratio)", 
     color = "Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")



### Effects Size of Being More Dynamic ###
# How much better are we at prediction using a certain model. Since each data type is the same we account for that by grouping it by data type
effect_size <- all %>% 
group_by(eval) %>% 
mutate(AUC_mean = mean(AUC, na.rm = TRUE),
       effect_size = ((AUC - AUC_mean)/AUC_mean)*100,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ungroup() %>% 
ggplot() +
geom_jitter(aes(x = model, y = effect_size, color = model, shape = eval), size = 3) +
theme_bw() +
labs(color="Integration Model", shape = "Data Type", 
     x = "", y = "Effect Size (%)") +
scale_color_brewer(palette = "Spectral")

ggsave(here("plots","PredictiveSkill_EffectSize.png"),
       width = 10, height = 6, units = "in", dpi = 300)







#################################
# Metrics from downsampled models
#################################

# All BRT and INLA results
BRT_ensemble <- here("results","BRT_ensemble_skill_downsampled.rds") %>% readRDS()
BRT_pooling <- here("results","BRT_pooling_skill_downsampled.rds") %>% readRDS()
ISDM_spatial <- here("results","bsh_ISDM_spatial_downsampled.rds") %>% readRDS()
ISDM_spatiotemporal <- here("results","bsh_ISDM_spatiotemporal_downsampled.rds") %>% readRDS()

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
   
            
            
### AUC vs MAE ###
PredSkill_downsampled <- all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot() +
geom_point(aes(x = MAE, y = AUC, color = model, shape = eval), size = 3) +
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral") +
theme(legend.position = "none")



### Ecological Realism ###
EcoReal_downsampled <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
# geom_boxplot(aes(x = model, y = FalseNeg, color = model)) +
geom_point(aes(x = FalsePos, y = FalseNeg, color = model, shape = eval), size = 2) +
theme_bw() +
labs(x="Prediction at Pseudo-absences", y = "Prediction at Presences", 
     color = "Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")




### Computational Demand ###
CompDemand_downsampled <- all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot(aes(x = time, y = AUC)) +
geom_point(aes(color = model, shape = eval), size = 3) +
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type", x = "Time (minutes)") +
scale_color_brewer(palette = "Spectral") +
theme(legend.position = "none")
# geom_smooth(method='lm', formula= y~x)


PredSkill_downsampled / EcoReal_downsampled / CompDemand_downsampled + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(here("plots","ModelPreformanceAnalysis_downsampled.png"),
       width = 8, height = 10, units = "in", dpi = 300)



### Ecological Realism option B & C ###
EcoReal_ratio_downsampled <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
# geom_violin(aes(x = model, y = FalseNeg_FalsePos_ratio, color = model)) +
geom_jitter(aes(x = model, y = FalseNeg_FalsePos_ratio, color = model, shape = eval)) +
# geom_point(aes(x = FalsePos, y = FalseNeg, color = model, shape = eval), size = 2) +
theme_bw() +
labs(x="", y = "Ecological Realism (presence : pseduo-absence ratio)", 
     color = "Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")

ggsave(here("plots","EcologicalRealism_downsampled.png"),
       width = 10, height = 6, units = "in", dpi = 300)

all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%
ggplot() +
geom_point(aes(x = FalseNeg_FalsePos_ratio, y = AUC, color = model, shape = eval), size = 2) +
theme_bw() +
labs(y="AUC", x = "Ecological Realism (presence : pseduo-absence ratio)", 
     color = "Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")



### Effects Size of Being More Dynamic ###
# How much better are we at prediction using a certain model. Since each data type is the same we account for that by grouping it by data type
effect_size_downsampled <- all %>% 
group_by(eval) %>% 
mutate(AUC_mean = mean(AUC, na.rm = TRUE),
       effect_size = ((AUC - AUC_mean)/AUC_mean)*100,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ungroup() %>% 
ggplot() +
geom_jitter(aes(x = model, y = effect_size, color = model, shape = eval), size = 3) +
theme_bw() +
labs(color="Integration Model", shape = "Data Type", 
     x = "", y = "Effect Size (%)") +
scale_color_brewer(palette = "Spectral")


ggsave(here("plots","PredictiveSkill_EffectSize_downsampled.png"),
       width = 10, height = 6, units = "in", dpi = 300)



### AUC & Boyce (Discrimintation vs Calibration) ###

all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos) %>%
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot() +
geom_point(aes(x = Boyce, y = AUC, color = model, shape = eval), size = 3) +
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type") +
scale_color_brewer(palette = "Spectral")

all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos) %>%
group_by(model) %>% 
summarise(cor_coef = cor.test(FalseNeg_FalsePos_ratio, Boyce)$estimate, 
          p_val = cor.test(FalseNeg_FalsePos_ratio, Boyce)$p.value)


