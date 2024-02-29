# # repeated k fold cross validation for BRT pooling

BRT_pooling_skill <- function(dataInput, gbm.x, gbm.y, learning.rate = 0.05, k_folds = 5, repeats = 10, tree.complexity = 3, bag.fraction = 0.6, n_trees = 2500){

  # make dataframe for metrics
  Evaluations_kfold_BRT <- as.data.frame(matrix(data=0,nrow=(k_folds*4) * repeats,ncol=15)) 
  colnames(Evaluations_kfold_BRT) <- c("k","Deviance", "R_squared", "AUC","TSS","MAE","Bias","Sensitivity","Specificity","FalsePos","FalseNeg","Boyce","repeat","eval","time")
  counter = 1
    
  print(paste0("testing BRT pooling predictive skill"))
  for (i in 1:repeats){
    
    set.seed(i*150) # for reproducability
    dataInput$Kset <- dismo::kfold(dataInput, k_folds) # randomly allocate k groups

     
    for (k in 1:k_folds){
      print(paste0("k=",k,", repeat=",i))
      
      #spilt between training and testing
      train <- dataInput[dataInput$Kset!=k,] %>% as.data.frame()
      test <- dataInput[dataInput$Kset==k,] %>% as.data.frame()

      
      # fit BRT models
      t1 <- Sys.time()
      set.seed(i*124)
      brt.k <- dismo::gbm.fixed(data = train, gbm.x = gbm.x,
                                   gbm.y = gbm.y,
                                   family="bernoulli",
                                   tree.complexity = tree.complexity,
                                   learning.rate = learning.rate,
                                   bag.fraction = bag.fraction,
                                   n.trees = n_trees)

      # predict and evaluate
      preds_pooling <- gbm::predict.gbm(brt.k, test,
                                n.trees=brt.k$gbm.call$best.trees,
                                type="response")
      
      t2 <- Sys.time()
      t3 <- difftime(t2,t1, units = c("mins"))
      
      
      dev_eval3<-function(x){
        null <- x$self.statistics$null.deviance #use with gbm.fixed
        res <- x$self.statistics$resid.deviance #use with gbm.fixed
        # null <- x$self.statistics$mean.null
        # res <- x$self.statistics$mean.resid
        dev=((null - res)/null)*100
        return(dev)
      }
      dev<-dev_eval3(brt.k)

      
      pseudoR2.BRT <- function(x){
      d2 <- 1-(x$self.statistics$resid.deviance/x$self.statistics$null.deviance)
      return(d2)
      }
      R2 <- pseudoR2.BRT(brt.k)
      
      # etag evaluation
      etag_pooling <- preds_pooling[which(test$dataset == "etag")]
      
      d <- cbind(test %>% filter(dataset == "etag") %>% pull(gbm.y), etag_pooling)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(test %>% filter(dataset == "etag") %>% pull(gbm.y), etag_pooling)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(test %>% filter(dataset == "etag") %>% pull(gbm.y), etag_pooling)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((test %>% filter(dataset == "etag") %>% pull(gbm.y))),factor(round(etag_pooling)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((test %>% filter(dataset == "etag") %>% pull(gbm.y))),factor(round(etag_pooling)))
      Evaluations_kfold_BRT[counter,10] <- mean(etag_pooling[test[test$dataset == "etag",gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(etag_pooling[test[test$dataset == "etag",gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- ecospat::ecospat.boyce(etag_pooling, etag_pooling[test[test$dataset == "etag",gbm.y]==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
      Evaluations_kfold_BRT[counter,13] <- i
      Evaluations_kfold_BRT[counter,14] <- "etag"
      Evaluations_kfold_BRT[counter,15] <- t3
      counter=counter+1  


      # marker evaluation
      marker_pooling <- preds_pooling[which(test$dataset == "marker")]
      
      d <- cbind(test %>% filter(dataset == "marker") %>% pull(gbm.y), marker_pooling)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(test %>% filter(dataset == "marker") %>% pull(gbm.y), marker_pooling)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(test %>% filter(dataset == "marker") %>% pull(gbm.y), marker_pooling)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((test %>% filter(dataset == "marker") %>% pull(gbm.y))),factor(round(marker_pooling)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((test %>% filter(dataset == "marker") %>% pull(gbm.y))),factor(round(marker_pooling)))
      Evaluations_kfold_BRT[counter,10] <- mean(marker_pooling[test[test$dataset == "marker",gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(marker_pooling[test[test$dataset == "marker",gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- ecospat::ecospat.boyce(marker_pooling, marker_pooling[test[test$dataset == "marker",gbm.y]==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
      Evaluations_kfold_BRT[counter,13] <- i
      Evaluations_kfold_BRT[counter,14] <- "marker"
      Evaluations_kfold_BRT[counter,15] <- t3
      counter=counter+1  
      
      
      # observer evaluation
      observer_pooling <- preds_pooling[which(test$dataset == "observer")]
      
      d <- cbind(test %>% filter(dataset == "observer") %>% pull(gbm.y), observer_pooling)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(test %>% filter(dataset == "observer") %>% pull(gbm.y), observer_pooling)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(test %>% filter(dataset == "observer") %>% pull(gbm.y), observer_pooling)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((test %>% filter(dataset == "observer") %>% pull(gbm.y))),factor(round(observer_pooling)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((test %>% filter(dataset == "observer") %>% pull(gbm.y))),factor(round(observer_pooling)))
      Evaluations_kfold_BRT[counter,10] <- mean(observer_pooling[test[test$dataset == "observer",gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(observer_pooling[test[test$dataset == "observer",gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- ecospat::ecospat.boyce(observer_pooling, observer_pooling[test[test$dataset == "observer",gbm.y]==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
      Evaluations_kfold_BRT[counter,13] <- i
      Evaluations_kfold_BRT[counter,14] <- "observer"
      Evaluations_kfold_BRT[counter,15] <- t3
      counter=counter+1  
      
      
      
      # all evaluation
      d <- cbind(test %>% pull(gbm.y), preds_pooling)
      pres <- as.numeric(d[d[,1]==1,2])
      abs <- as.numeric(d[d[,1]==0,2])
      e <- dismo::evaluate(p=pres, a=abs)
      
      Evaluations_kfold_BRT[counter,1] <- k
      Evaluations_kfold_BRT[counter,2] <- dev
      Evaluations_kfold_BRT[counter,3] <- R2
      Evaluations_kfold_BRT[counter,4] <- e@auc
      Evaluations_kfold_BRT[counter,5] <- max(e@TPR + e@TNR-1)
      Evaluations_kfold_BRT[counter,6] <- Metrics::mae(test %>% pull(gbm.y), preds_pooling)
      Evaluations_kfold_BRT[counter,7] <- Metrics::bias(test %>% pull(gbm.y), preds_pooling)
      Evaluations_kfold_BRT[counter,8] <- caret::sensitivity(factor((test %>% pull(gbm.y))),factor(round(preds_pooling)))
      Evaluations_kfold_BRT[counter,9] <- caret::specificity(factor((test %>% pull(gbm.y))),factor(round(preds_pooling)))
      Evaluations_kfold_BRT[counter,10] <- mean(preds_pooling[test[,gbm.y]==0])
      Evaluations_kfold_BRT[counter,11] <- mean(preds_pooling[test[,gbm.y]==1])
      Evaluations_kfold_BRT[counter,12] <- ecospat::ecospat.boyce(preds_pooling, preds_pooling[test[,gbm.y]==1], nclass=0, window.w="default", res=100, PEplot = FALSE)$cor
      Evaluations_kfold_BRT[counter,13] <- i
      Evaluations_kfold_BRT[counter,14] <- "all"
      Evaluations_kfold_BRT[counter,15] <- t3
      counter=counter+1  
      
    }
  }
   
  return(Evaluations_kfold_BRT)
}
