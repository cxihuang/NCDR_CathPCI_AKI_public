# tuning algorithm for xgboost
# stepwise hyperparameter tuning, based on CV auc.
# Input: foldseed: seed for CV validation
#        modelseed: seed for randomness in modeling
#        y: outcome
#        x: variables
#        nfold: number of folds
#        ntrees: max number of trees used
#        mindepth, maxdepth: range of tree depth
#        minchildw,maxchildw,init_childw: range of leaf size
#        minsubsample, maxsubsample, init_subsample: range of subsamping rate
#        mincolsample, maxcolsample, init_colsample: range of column sampling rate
#        eta, init_eta: learning rate candidates
# Output: learning rate, column sampling rate, min leaf size, subsampling rate, max tree depth.


library(caret)
library(xgboost)

xgboost_autogrid<-function(foldseed,modelseed,y,x,nfold,ntrees,mindepth,maxdepth,
                           minsubsample,maxsubsample,init_subsample,
                           minchildw,maxchildw,init_childw,
                           mincolsample,maxcolsample,init_colsample,
                           eta,init_eta,
                           nthread){
  set.seed(foldseed)
  folds=createFolds(factor(y),k=nfold)
  
  max_depth=seq(mindepth,maxdepth,1)
  cvauc=rep(NA, length(max_depth))
  cvaucsd=cvauc
  cvniter=cvauc
  for (i in 1:length(max_depth)){
    print(max_depth[i])
    set.seed(modelseed)
    tmp=xgb.cv(data=x,label=as.numeric(y), nrounds=ntrees, objective="binary:logistic",
               early_stopping_rounds = 50, folds = folds, 
               metrics=c("auc"),
               eta=init_eta, subsample=init_subsample, colsample_bytree=init_colsample, max_depth=max_depth[i], 
               min_child_weight=init_childw,
               verbose=0, nthread=nthread
    )
    cvniter[i]=as.numeric(tmp$best_iteration)
    cvauc[i]=as.numeric(tmp$evaluation_log[cvniter[i],4]) # test_auc_mean
    cvaucsd[i]=as.numeric(tmp$evaluation_log[cvniter[i],5])
    print(cvauc[i])
    print(cvaucsd[i])
    print(cvniter[i])
  }
  newmax_depth=max_depth[which.max(cvauc)]
  
  subsample=seq(minsubsample,maxsubsample,0.1)
  cvauc=rep(NA, length(subsample))
  cvaucsd=cvauc
  cvniter=cvauc
  for (i in 1:length(subsample)){
    print(subsample[i])
    set.seed(modelseed)
    tmp=xgb.cv(data=x,label=as.numeric(y), nrounds=ntrees, objective="binary:logistic",
               early_stopping_rounds = 50, folds=folds,
               metrics=c("auc"),
               eta=init_eta, subsample=subsample[i], colsample_bytree=init_colsample, max_depth=newmax_depth, 
               min_child_weight=init_childw,
               nthread=nthread, verbose=0)
    cvniter[i]=as.numeric(tmp$best_iteration)
    cvauc[i]=as.numeric(tmp$evaluation_log[cvniter[i],4]) # test_auc_mean
    cvaucsd[i]=as.numeric(tmp$evaluation_log[cvniter[i],5])
    print(cvauc[i])
    print(cvaucsd[i])
    print(cvniter[i])
  }
  newsubsample=subsample[which.max(cvauc)]
  
  
  min_child_weight=seq(minchildw,maxchildw,1)
  cvauc=rep(NA, length(min_child_weight))
  cvaucsd=cvauc
  cvniter=cvauc
  for (i in 1:length(min_child_weight)){
    print(min_child_weight[i])
    set.seed(modelseed)
    tmp=xgb.cv(data=x,label=as.numeric(y), nrounds=ntrees, objective="binary:logistic",
               early_stopping_rounds = 50, folds=folds,
               metrics=c("auc"),
               eta=init_eta, subsample=newsubsample, colsample_bytree=init_colsample, max_depth=newmax_depth, 
               min_child_weight=min_child_weight[i],
               nthread=nthread, verbose=0)
    cvniter[i]=as.numeric(tmp$best_iteration)
    cvauc[i]=as.numeric(tmp$evaluation_log[cvniter[i],4]) # test_auc_mean
    cvaucsd[i]=as.numeric(tmp$evaluation_log[cvniter[i],5])
    print(cvauc[i])
    print(cvaucsd[i])
    print(cvniter[i])
  }
  newmin_child_weight=min_child_weight[which.max(cvauc)]
  
  colsample_bytree=seq(mincolsample,maxcolsample,0.1)
  cvauc=rep(NA, length(colsample_bytree))
  cvaucsd=cvauc
  cvniter=cvauc
  for (i in 1:length(colsample_bytree)){
    print(colsample_bytree[i])
    set.seed(modelseed)
    tmp=xgb.cv(data=x,label=as.numeric(y), nrounds=ntrees, objective="binary:logistic",
               early_stopping_rounds = 50, folds=folds,
               metrics=c("auc"),
               eta=init_eta, subsample=newsubsample, colsample_bytree=colsample_bytree[i], 
               max_depth=newmax_depth, 
               min_child_weight=newmin_child_weight,
               nthread=nthread, verbose=0)
    cvniter[i]=as.numeric(tmp$best_iteration)
    cvauc[i]=as.numeric(tmp$evaluation_log[cvniter[i],4]) # test_auc_mean
    cvaucsd[i]=as.numeric(tmp$evaluation_log[cvniter[i],5])
    print(cvauc[i])
    print(cvaucsd[i])
    print(cvniter[i])
  }
  newcolsample_bytree=colsample_bytree[which.max(cvauc)]
  

  cvauc=rep(NA, length(eta))
  cvaucsd=cvauc
  cvniter=cvauc
  for (i in 1:length(eta)){
    print(eta[i])
    set.seed(modelseed)
    tmp=xgb.cv(data=x,label=as.numeric(y), nrounds=ntrees, objective="binary:logistic",
               early_stopping_rounds = 50, folds=folds, 
               metrics=c("auc"),
               eta=eta[i], subsample=newsubsample, colsample_bytree=newcolsample_bytree, 
               max_depth=newmax_depth, min_child_weight=newmin_child_weight,
               verbose=0,
               nthread=nthread)
    cvniter[i]=as.numeric(tmp$best_iteration)
    cvauc[i]=as.numeric(tmp$evaluation_log[cvniter[i],4]) # test_auc_mean
    cvaucsd[i]=as.numeric(tmp$evaluation_log[cvniter[i],5])
    print(cvauc[i])
    print(cvaucsd[i])
    print(cvniter[i])
  }
  neweta=eta[which.max(cvauc)]
  return(data.frame(neweta=neweta,newcolsample=newcolsample_bytree,newchildw=newmin_child_weight,
                    newsubsample=newsubsample,newdepth=newmax_depth))
}