# Comparing model performance across integration approaches

library(tidyverse)
library(here)
library(cmocean)
library(patchwork)

###########################################
# Metrics from models ran with all the data
###########################################

# All BRT and INLA results
BRT_ensemble <- here("results","BRT_ensemble_skill.rds") %>% readRDS() %>% dplyr::select(-Boyce)
BRT_pooling <- here("results","BRT_pooling_skill.rds") %>% readRDS()
ISDM_spatial <- here("results","bsh_ISDM_spatial_norm_withintercepts.rds") %>% readRDS() %>% dplyr::select(-Boyce)
ISDM_spatiotemporal <- here("results","bsh_ISDM_spatiotemporal_norm_withintercepts.rds") %>% readRDS() %>% dplyr::select(-Boyce)

# making sure that each of the dataset have the same columns
BRT_ensemble <- BRT_ensemble %>% dplyr::select(-2) %>% mutate(model = "Ensemble")
BRT_pooling <- BRT_pooling %>% dplyr::select(-2) %>% mutate(model = "Pooling")
ISDM_spatial <- ISDM_spatial %>% dplyr::select(-c(2:4)) %>% mutate(model = "iSDM Constant")
ISDM_spatiotemporal <- ISDM_spatiotemporal %>% dplyr::select(-c(2:4)) %>% mutate(model = "iSDM Seasonal")
colnames(ISDM_spatial) <- colnames(BRT_ensemble)
colnames(ISDM_spatiotemporal) <- colnames(BRT_ensemble)
colnames(BRT_pooling) <- colnames(BRT_ensemble)
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
geom_point(aes(x = MAE, y = AUC, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(fill="Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
theme(legend.position = "none")



### Ecological Realism ###
EcoReal <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
# geom_boxplot(aes(x = model, y = FalseNeg, color = model)) +
geom_point(aes(x = FalsePos, y = FalseNeg, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
labs(x="Prediction at Pseudo-absences", y = "Prediction at Presences", 
     fill = "Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
guides(fill = guide_legend(override.aes = list(shape = 21)))


### Computational Demand ###
CompDemand <- all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot(aes(x = time, y = AUC)) +
geom_point(aes(fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(color="Integration Model", shape = "Data Type", x = "Time (minutes)") +
scale_fill_brewer(palette = "Spectral") +
# facet_wrap(~eval) + 
# geom_smooth(aes(x = time, y = AUC, group = eval), color = 'black', method = lm, formula = y ~ x, se = FALSE) +
theme(legend.position = "none")

# linear regression to explore relationship between AUC and Computational Demand
summary(lm(AUC ~ time, data = all %>% filter(eval == "Marker")))
summary(lm(AUC ~ time, data = all %>% filter(eval == "Observer")))
summary(lm(AUC ~ time, data = all %>% filter(eval == "Eletronic Tag")))

PredSkill / EcoReal / CompDemand + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(here("plots","ModelPreformanceAnalysis.png"),
       width = 8, height = 10, units = "in", dpi = 300)



### Ecological Realism option B & C ###
EcoReal_ratio <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
geom_jitter(aes(x = eval, y = FalseNeg_FalsePos_ratio, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
labs(x="", y = "Ecological Realism (presence : pseduo-absence ratio)", 
     fill = "Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
theme(axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 15)) +
guides(fill = guide_legend(override.aes = list(shape = 21))) #+
# theme(legend.position = "none")

# ggsave(here("plots","EcologicalRealism.png"),
#        width = 10, height = 6, units = "in", dpi = 300)

all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%
ggplot() +
geom_point(aes(x = FalseNeg_FalsePos_ratio, y = AUC, fill = model, shape = eval), size = 2) +
scale_shape_manual(values = c(21,24,22)) +
theme_bw() +
labs(y="AUC", x = "Ecological Realism (presence : pseduo-absence ratio)", 
     fill = "Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
guides(fill = guide_legend(override.aes = list(shape = 21)))


### Effects Size of Being More Dynamic ###
# How much better are we at prediction using a certain model. Since each data type is the same we account for that by grouping it by data type
effect_size <- all %>% 
group_by(eval) %>% 
mutate(AUC_mean = mean(AUC, na.rm = TRUE),
       effect_size = ((AUC - AUC_mean)/AUC_mean)*100,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ungroup() %>% 
ggplot() +
geom_jitter(aes(x = eval, y = effect_size, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) +
theme_bw() +
labs(fill = "Integration Model", shape = "Data Type", 
     x = "", y = "Deviation in Predictive Skill (%)") +
scale_fill_brewer(palette = "Spectral") +
theme(axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 15)) +
guides(fill = guide_legend(override.aes = list(shape = 21))) +
theme(legend.position = "none")

# ggsave(here("plots","PredictiveSkillDeviations.png"),
#        width = 7, height = 5, units = "in", dpi = 300)


# anova to see how effect size differs among models
effect_size_df <- all %>% 
group_by(eval) %>% 
mutate(AUC_mean = mean(AUC, na.rm = TRUE),
       effect_size = ((AUC - AUC_mean)/AUC_mean)*100,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ungroup()

effect_size_aov <- aov(AUC ~ eval, data = effect_size_df)
summary(effect_size_aov)
TukeyHSD(effect_size_aov)

# Have the EcoReal_ratio plot and Effects size in the same figure
effect_size + EcoReal_ratio + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

ggsave(here("plots","EffectSizePredictiveSkill_EcologicalRealism.png"),
       width = 15, height = 6, units = "in", dpi = 300)







#################################
# Metrics from downsampled models
#################################

# All BRT and INLA results
BRT_ensemble <- here("results","BRT_ensemble_skill_downsampled.rds") %>% readRDS()
BRT_pooling <- here("results","BRT_pooling_skill_downsampled.rds") %>% readRDS()
ISDM_spatial <- here("results","bsh_ISDM_spatial_downsampled_norm_withintercepts.rds") %>% readRDS()
ISDM_spatiotemporal <- here("results","bsh_ISDM_spatiotemporal_downsampled_norm_withintercepts.rds") %>% readRDS()

# making sure that each of the dataset have the same columns
BRT_ensemble <- BRT_ensemble %>% dplyr::select(-2) %>% mutate(model = "Ensemble")
BRT_pooling <- BRT_pooling %>% dplyr::select(-2) %>% mutate(model = "Pooling")
ISDM_spatial <- ISDM_spatial %>% dplyr::select(-c(2:4)) %>% mutate(model = "iSDM Constant")
ISDM_spatiotemporal <- ISDM_spatiotemporal %>% dplyr::select(-c(2:4)) %>% mutate(model = "iSDM Seasonal")
colnames(ISDM_spatial) <- colnames(BRT_ensemble)
colnames(ISDM_spatiotemporal) <- colnames(BRT_ensemble)
colnames(BRT_pooling) <- colnames(BRT_ensemble)
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
geom_point(aes(x = MAE, y = AUC, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(fill="Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
theme(legend.position = "none")



### Ecological Realism ###
EcoReal_downsampled <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
# geom_boxplot(aes(x = model, y = FalseNeg, color = model)) +
geom_point(aes(x = FalsePos, y = FalseNeg, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
labs(x="Prediction at Pseudo-absences", y = "Prediction at Presences", 
     fill = "Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
guides(fill = guide_legend(override.aes = list(shape = 21)))




### Computational Demand ###
CompDemand_downsampled <- all %>% 
mutate(model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%  
ggplot(aes(x = time, y = AUC)) +
geom_point(aes(fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
# theme(axis.title=element_text(size=10),
#       axis.text=element_text(size=10))+
labs(fill="Integration Model", shape = "Data Type", x = "Time (minutes)") +
scale_fill_brewer(palette = "Spectral") +
# facet_wrap(~eval) + 
# geom_smooth(aes(x = time, y = AUC, group = eval), color = 'black', method = lm, formula = y ~ x, se = FALSE) +
theme(legend.position = "none")

# linear regression to explore relationship between AUC and Computational Demand
summary(lm(AUC ~ time, data = all %>% filter(eval == "Marker")))
summary(lm(AUC ~ time, data = all %>% filter(eval == "Observer")))
summary(lm(AUC ~ time, data = all %>% filter(eval == "Eletronic Tag")))


PredSkill_downsampled / EcoReal_downsampled / CompDemand_downsampled + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

ggsave(here("plots","ModelPreformanceAnalysis_downsampled.png"),
       width = 8, height = 10, units = "in", dpi = 300)



### Ecological Realism option B & C ###
EcoReal_ratio_downsampled <- all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ggplot() +
geom_jitter(aes(x = eval, y = FalseNeg_FalsePos_ratio, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) + 
theme_bw() +
theme(axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 15)) +
labs(x="", y = "Ecological Realism (presence : pseduo-absence ratio)", 
     fill = "Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
guides(fill = guide_legend(override.aes = list(shape = 21))) #+
# theme(legend.position = "none")

# ggsave(here("plots","EcologicalRealism_downsampled.png"),
#        width = 10, height = 6, units = "in", dpi = 300)

all %>%
mutate(FalseNeg_FalsePos_ratio = FalseNeg/FalsePos, # the higher the ratio the more realistic predictions (predictions at presences are higher and prediction at pseudoabs are lower)
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>%
ggplot() +
geom_point(aes(x = FalseNeg_FalsePos_ratio, y = AUC, fill = model, shape = eval), size = 2) +
scale_shape_manual(values = c(21,24,22)) +
theme_bw() +
labs(y="AUC", x = "Ecological Realism (presence : pseduo-absence ratio)", 
     fill = "Integration Model", shape = "Data Type") +
scale_fill_brewer(palette = "Spectral") +
guides(fill = guide_legend(override.aes = list(shape = 21)))



### Effects Size of Being More Dynamic ###
# How much better are we at prediction using a certain model. Since each data type is the same we account for that by grouping it by data type
effect_size_downsampled <- effect_size <- all %>% 
group_by(eval) %>% 
mutate(AUC_mean = mean(AUC, na.rm = TRUE),
       effect_size = ((AUC - AUC_mean)/AUC_mean)*100,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ungroup() %>% 
ggplot() +
geom_jitter(aes(x = eval, y = effect_size, fill = model, shape = eval), size = 3) +
scale_shape_manual(values = c(21,24,22)) +
theme_bw() +
theme(axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 15)) +
labs(fill = "Integration Model", shape = "Data Type", 
     x = "", y = "Deviation in Predictive Skill (%)") +
scale_fill_brewer(palette = "Spectral") +
guides(fill = guide_legend(override.aes = list(shape = 21))) +
theme(legend.position = "none")

# ggsave(here("plots","PredictiveSkillDeviations_downsampled.png"),
#        width = 7, height = 5, units = "in", dpi = 300)

# anova to see how effect size differs among models
effect_size_df <- all %>% 
group_by(eval) %>% 
mutate(AUC_mean = mean(AUC, na.rm = TRUE),
       effect_size = ((AUC - AUC_mean)/AUC_mean)*100,
       model = factor(model, levels = c("Pooling", "Ensemble", "iSDM Constant", "iSDM Seasonal"))) %>% 
ungroup()

effect_size_aov <- aov(AUC ~ eval, data = effect_size_df)
summary(effect_size_aov)
TukeyHSD(effect_size_aov)

# Have the EcoReal_ratio_downsampled plot and effect_size_downsampled in the same figure
effect_size_downsampled + EcoReal_ratio_downsampled + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10))

ggsave(here("plots","EffectSizePredictiveSkill_EcologicalRealism_downsampled.png"),
       width = 15, height = 6, units = "in", dpi = 300)



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