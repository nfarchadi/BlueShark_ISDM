# BRT ensemble and pooling

library(tidyverse)
library(raster)
library(zoo)
library(here)
library(dismo)
library(Metrics)
library(ecospat)
library(caret)
library(sf)
sf_use_s2(FALSE) # need to do this to remove spherical geometry


##########
# bsh data
##########
bsh_all <- here("data","bsh_data_subset.csv") %>% 
                read.csv() 


#########################################################
# 5 fold cross-validation, repeated 10 times BRT ensemble
#########################################################
source(here("functions", "BRT_ensemble_skill.r"))

bsh_ensemble <- BRT_ensemble_skill(dataInput = bsh_all, 
                            gbm.x = c(5,6,7),
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            k_folds = 5, repeats = 10,
                            n_trees = 2000)

saveRDS(bsh_ensemble, here("BRT_ensemble_skill_results.rds"))


########################################################
# 5 fold cross-validation, repeated 10 times BRT pooling
########################################################
source(here("functions", "BRT_pooling_skill.r"))

bsh_pooling <- BRT_pooling_skill(dataInput = bsh_all, 
                            gbm.x = c(5,6,7), 
                            gbm.y=1, learning.rate = 0.005, 
                            bag.fraction = 0.75, tree.complexity = 5,
                            k_folds = 5, repeats = 10,
                            n_trees = 2000)

saveRDS(bsh_pooling, here("BRT_pooling_skill_results.rds"))