# # Fitting BRT pooling with full dataset

BRT_pooling_fullmodel <- function(dataInput, gbm.x, gbm.y, learning.rate = 0.05, tree.complexity = 3, bag.fraction = 0.6, n_trees = 2500){

bsh_pooled <- dataInput %>% as.data.frame()

brt.pooling <- dismo::gbm.fixed(data = bsh_pooled, gbm.x = gbm.x,
                            gbm.y = gbm.y,
                            family="bernoulli",
                            tree.complexity = tree.complexity,
                            learning.rate = learning.rate,
                            bag.fraction = bag.fraction,
                            n.trees = n_trees)
    

return(brt.pooling)
}
