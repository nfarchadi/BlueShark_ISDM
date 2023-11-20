library(tidyverse)
library(here)

# All BRT and INLA results
BRT_ensemble <- here("results","BRT_ensemble_skill.rds") %>% readRDS()
BRT_pooling <- here("results","BRT_pooling_skill.rds") %>% readRDS()
ISDM_spatial <- here("results","bsh_ISDM_spatial.rds") %>% readRDS()
# ISDM_spatiotemporal <- here("results","bsh_ISDM_spatiotemporal.rds") %>% readRDS()

# making sure that each of the dataset have the same columns
BRT_ensemble <- BRT_ensemble %>% dplyr::select(-2) %>% mutate(dataset = "ensemble")
BRT_pooling <- BRT_pooling %>% dplyr::select(-2) %>% mutate(dataset = "pooling")
ISDM_spatial <- ISDM_spatial %>% dplyr::select(-c(2:4)) %>% mutate(dataset = "ISDM spatial")
colnames(ISDM_spatial) <- colnames(BRT_ensemble)
all <- rbind(BRT_ensemble, BRT_pooling, ISDM_spatial)


all %>% 
ggplot() +
geom_boxplot(aes(x = dataset, y = AUC)) +
theme_bw()

all %>% 
ggplot() +
geom_boxplot(aes(x = dataset, y = MAE)) +
theme_bw()


all %>% 
ggplot() +
geom_point(aes(x = MAE, y = AUC, color = dataset)) +
theme_bw()

all %>% 
ggplot() +
geom_point(aes(x = MAE, y = AUC, color = eval)) +
theme_bw()
