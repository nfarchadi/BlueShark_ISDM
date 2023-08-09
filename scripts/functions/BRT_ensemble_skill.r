# # repeated k fold cross validation for BRT ensembles 

BRT_ensemble_skill <- function(dataInput, gbm.x, gbm.y, learning.rate = 0.05, k_folds = 5, repeats = 10, tree.complexity = 3, bag.fraction = 0.6, n_trees = 2500){
  
  #separate by dataset
  etag <- dataInput %>% filter(dataset == "etag")
  marker <- dataInput %>% filter(dataset == "marker")
  observer <- dataInput %>% filter(dataset == "observer")  

 
  # make dataframe for metrics
  Evaluations_kfold_BRT <- as.data.frame(matrix(data=0,nrow=(k_folds*4) * repeats,ncol=14)) 
  colnames(Evaluations_kfold_BRT) <- c("k","Deviance", "R_squred", "AUC","TSS","MAE","Bias","Sensitivity","Specificity","FalsePos","FalseNeg","repeat", "eval", "time")
  counter = 1
    
  print(paste0("testing BRT ensemble predictive skill"))
  for (i in 1:repeats){
    
    
    set.seed(i*150) # for reproducability
    etag$Kset <- dismo::kfold(etag, k_folds) # randomly allocate k groups
    marker$Kset <- dismo::kfold(marker, k_folds) # randomly allocate k groups
    observer$Kset <- dismo::kfold(observer, k_folds) # randomly allocate k groups
   
  
    for (k in 1:k_folds){
      print(paste0("k=",k,", repeat=",i))
      
      #spilt between training and testing
      etag_train <- etag[etag$Kset!=k,] %>% as.data.frame()
      etag_test <- etag[etag$Kset==k,] %>% as.data.frame()
      
      marker_train <- marker[marker$Kset!=k,] %>% as.data.frame()
      marker_test <- marker[marker$Kset==k,] %>% as.data.frame()
      
      observer_train <- observer[observer$Kset!=k,] %>% as.data.frame()
      observer_test <- observer[observer$Kset==k,] %>% as.data.frame()
      
      # fit BRT models
      t1 <- Sys.time()
      set.seed(i*124)
      brt.etag <- dismo::gbm.fixed(data = etag_train, gbm.x = gbm.x,
                                   gbm.y = gbm.y,
                                   family="bernoulli",
                                   tree.complexity = tree.complexity,
                                   learning.rate = learning.rate,
                                   bag.fraction = bag.fraction,
                                   n.trees = n_trees)
      
      brt.marker <- dismo::gbm.fixed(data = marker_train, gbm.x = gbm.x,
                                   gbm.y = gbm.y,
                                   family="bernoulli",
                                   tree.complexity = tree.complexity,
                                   learning.rate = learning.rate,
                                   bag.fraction = bag.fraction,
                                   n.trees = n_trees)
      
      brt.observer <- dismo::gbm.fixed(data = observer_train, gbm.x = gbm.x,
                                   gbm.y = gbm.y,
                                   family="bernoulli",
                                   tree.complexity = tree.complexity,
                                   learning.rate = learning.rate,
                                   bag.fraction = bag.fraction,
                                   n.trees = n_trees)
      
      
      t2 <- Sys.time()
      t3 <- t2-t1

      # predict and evaluate
      testCombine <- rbind(etag_test, marker_test, observer_test)

      preds_etag <- gbm::predict.gbm(brt.etag, testCombine,
                                n.trees=brt.etag$gbm.call$best.trees,
                                type="response")
      
      preds_marker <- gbm::predict.gbm(brt.marker, testCombine,
                                n.trees=brt.marker$gbm.call$best.trees,
                                type="response")
      
      preds_observer <- gbm::predict.gbm(brt.observer, testCombine,
                                n.trees=brt.observer$gbm.call$best.trees,
                                type="response")
      
      preds_Ensemble <- rowMeans(cbind(preds_etag,preds_etag,preds_observer), na.rm = TRUE)
      
      
      
      dev_eval3<-function(x){
        null <- x$self.statistics$null.deviance #use with gbm.fixed
        res <- x$self.statistics$resid.deviance #use with gbm.fixed
        # null <- x$self.statistics$mean.null
        # res <- x$self.statistics$mean.resid
        dev=((null - res)/null)*100
        return(dev)
      }
      dev.etag<-dev_eval3(brt.etag)
      dev.marker<-dev_eval3(brt.marker)
      dev.observer<-dev_eval3(brt.observer)
      dev <- mean(c(dev.etag,dev.marker,dev.observer))
      
      pseudoR2.BRT <- function(x){
      d2 <- 1-(x$self.statistics$resid.deviance/x$self.statistics$null.deviance)
      return(d2)
      }
      R2.etag <- pseudoR2.BRT(brt.etag)
      R2.marker <- pseudoR2.BRT(brt.marker)
      R2.observer <- pseudoR2.BRT(brt.observer)
      R2 <-mean(c(R2.etag,R2.marker,R2.observer))
      
      # etag evaluation
      etag_Ensemble <- preds_Ensemble[which(testCombine$dataset == "etag")]
      
      d <- cbind(etag_test %>% pull(gbm.y), etag_Ensemble)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(etag_test %>% pull(gbm.y), etag_Ensemble)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(etag_test %>% pull(gbm.y), etag_Ensemble)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((etag_test %>% pull(gbm.y))),factor(round(etag_Ensemble)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((etag_test %>% pull(gbm.y))),factor(round(etag_Ensemble)))
      Evaluations_kfold_BRT[counter,10] <- mean(etag_Ensemble[etag_test[,gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(etag_Ensemble[etag_test[,gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- i
      Evaluations_kfold_BRT[counter,13] <- "etag"
      Evaluations_kfold_BRT[counter,14] <- t3
      counter=counter+1  


      # marker evaluation
      marker_Ensemble <- preds_Ensemble[which(testCombine$dataset == "marker")]
      
      d <- cbind(marker_test %>% pull(gbm.y), marker_Ensemble)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(marker_test %>% pull(gbm.y), marker_Ensemble)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(marker_test %>% pull(gbm.y), marker_Ensemble)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((marker_test %>% pull(gbm.y))),factor(round(marker_Ensemble)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((marker_test %>% pull(gbm.y))),factor(round(marker_Ensemble)))
      Evaluations_kfold_BRT[counter,10] <- mean(marker_Ensemble[marker_test[,gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(marker_Ensemble[marker_test[,gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- i
      Evaluations_kfold_BRT[counter,13] <- "marker"
      Evaluations_kfold_BRT[counter,14] <- t3
      counter=counter+1  
      
      
      # observer evaluation
      observer_Ensemble <- preds_Ensemble[which(testCombine$dataset == "observer")]
      
      d <- cbind(observer_test %>% pull(gbm.y), observer_Ensemble)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(observer_test %>% pull(gbm.y), observer_Ensemble)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(observer_test %>% pull(gbm.y), observer_Ensemble)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((observer_test %>% pull(gbm.y))),factor(round(observer_Ensemble)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((observer_test %>% pull(gbm.y))),factor(round(observer_Ensemble)))
      Evaluations_kfold_BRT[counter,10] <- mean(observer_Ensemble[observer_test[,gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(observer_Ensemble[observer_test[,gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- i
      Evaluations_kfold_BRT[counter,13] <- "observer"
      Evaluations_kfold_BRT[counter,14] <- t3
      counter=counter+1  
      
      
      
      # all evaluation
      d <- cbind(testCombine %>% pull(gbm.y), preds_Ensemble)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(testCombine %>% pull(gbm.y), preds_Ensemble)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(testCombine %>% pull(gbm.y), preds_Ensemble)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((testCombine %>% pull(gbm.y))),factor(round(preds_Ensemble)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((testCombine %>% pull(gbm.y))),factor(round(preds_Ensemble)))
      Evaluations_kfold_BRT[counter,10] <- mean(preds_Ensemble[testCombine[,gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(preds_Ensemble[testCombine[,gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- i
      Evaluations_kfold_BRT[counter,13] <- "all"
      Evaluations_kfold_BRT[counter,14] <- t3
      counter=counter+1  
      
    }
  }
   
  return(Evaluations_kfold_BRT)
}
