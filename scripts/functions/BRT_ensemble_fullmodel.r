# # Fitting BRT ensemble with full dataset

BRT_ensemble_fullmodel <- function(dataInput, gbm.x, gbm.y, learning.rate = 0.05, tree.complexity = 3, bag.fraction = 0.6, n_trees = 2500){
  
  #separate by dataset
  etag <- dataInput %>% filter(dataset == "etag") %>% as.data.frame()
  marker <- dataInput %>% filter(dataset == "marker") %>% as.data.frame()
  observer <- dataInput %>% filter(dataset == "observer") %>% as.data.frame()

  brt.etag <- dismo::gbm.fixed(data = etag, gbm.x = gbm.x,
                                gbm.y = gbm.y,
                                family="bernoulli",
                                tree.complexity = tree.complexity,
                                learning.rate = learning.rate,
                                bag.fraction = bag.fraction,
                                n.trees = n_trees)

  brt.marker <- dismo::gbm.fixed(data = marker, gbm.x = gbm.x,
                                gbm.y = gbm.y,
                                family="bernoulli",
                                tree.complexity = tree.complexity,
                                learning.rate = learning.rate,
                                bag.fraction = bag.fraction,
                                n.trees = n_trees)

  brt.observer <- dismo::gbm.fixed(data = observer, gbm.x = gbm.x,
                                gbm.y = gbm.y,
                                family="bernoulli",
                                tree.complexity = tree.complexity,
                                learning.rate = learning.rate,
                                bag.fraction = bag.fraction,
                                n.trees = n_trees)

  brt.ensemble <- list(brt.etag = brt.etag, brt.marker = brt.marker, brt.observer = brt.observer)

    
  return(brt.ensemble)
}
