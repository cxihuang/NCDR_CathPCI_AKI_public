# performance comparison for all models in development cohort
# from 100 iterations

library(xgboost)
library(doMC)
library(caret)
library(onehot)
library(fifer)
library(glmnet)
source("prediction_performance.R") # performance measures

# load data.candidate, data.preproc

vars_selected=c("AKI","Age","IABPst","egfrc","Prior2weeksHF","Diabetes",
                "PriorHF","PriorCVD","cadpresent","PriorCardioShock","PriorCardiacArrest","Anemia")
data.selected=data.candidate[,colnames(data.candidate)%in%vars_selected]



niter=100 # iterations
ntrees=1000 # max trees used by xgboost

writeLines(c(""),"log.txt")
registerDoMC(10)
set.seed(1205, kind = "L'Ecuyer-CMRG")
perform.all=foreach(i=1:niter) %dopar% {
  cat(paste("Starting iteration",i,"\n"),file="log.txt",append=TRUE)
  c.split = createDataPartition(data0$AKI, p = 0.7, list = F) # split to train and test
  # Model 1 selected: logistic regression ##############################################
  data0=data.selected
  train = data0[c.split, ]
  test = data0[-c.split, ]
  model=glm(AKI~.,family = binomial(),data=train)
  model.selected=model
  prediction=predict(model,newdata = test,type="response")
  prediction.selected.logit=prediction
  pred.selected.logit=prediction.performance(prediction,test$AKI,10)
  # Model 2 selected: xgboost ###########################################################
  ind.out=1
  predictor=train[,-ind.out] # one-hot encoding
  y=train[,ind.out]
  x=predict(onehot(predictor),predictor)
  newx=predict(onehot(test[,-ind.out]),test[,-ind.out])
  set.seed(1127) # build model
  tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.1, subsample=0.8, colsample_bytree=0.6, max_depth=4,
             min_child_weight=5, verbose = 0,
             stratified = TRUE, nthread=32,  seed=1127)
  newnrounds=tmp$best_iteration # choose best #trees
  set.seed(1130)
  model=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                eta=0.1, max_depth=4, min_child_weight=5,
                subsample=0.8, colsample_bytree=0.6, eval_metric=c("auc"),
                verbose=0
  )
  prediction=predict(model,newx)
  prediction.selected.xgboost=prediction
  pred.selected.xgboost=prediction.performance(prediction,test$AKI,10)
  
  # candidate: feature ranking #############################################
  data0=data.candidate
  train= data0[c.split, ]
  test = data0[-c.split, ]
  tmp=train
  ps=univariate.tests(tmp,group="AKI")
  ind=sort(ps,decreasing=T,index.return=T) # try sub-sampling if tied
  candidate.logit.features=ind$ix
  # Model 0 candidate: logistic regression #########################################
  model=glm(AKI~.,family = binomial(),data=train)
  model.all=model
  prediction=predict(model,newdata = test,type="response")
  prediction.selectedall.logit=prediction
  pred.selectedall.logit=prediction.performance(prediction,test$AKI,10)
  
  # Model 3 candidate: lasso #######################################################
  ind.out=1
  predictor=train[,-ind.out]
  y=train[,ind.out]
  x=predict(onehot(predictor),predictor)
  newx=predict(onehot(test[,-ind.out]),test[,-ind.out])
  features=colnames(x)
  set.seed(1127)
  folds=createFolds(factor(y),k=10)
  foldid=rep(NA,length(y))
  for (j in 1:length(folds)){
    foldid[folds[[j]]]=j
  }
  cvfit=cv.glmnet(x,y,foldid=foldid,family="binomial",type.measure="auc",parallel = TRUE)
  prediction=as.vector(predict(cvfit,newx = newx,s="lambda.1se",type="response"))
  prediction.candidate.lasso=prediction
  pred.candidate.lasso=prediction.performance(prediction,test$AKI,10)
  ind=predict(cvfit,s="lambda.1se",type="nonzero")$X1
  candidate.lasso.features=ind
  # Model 4 candidate: xgboost ####################################################
  set.seed(1127)
  tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.1, subsample=0.8, colsample_bytree=0.5, max_depth=4, 
             min_child_weight=3, verbose=0,
             stratified = TRUE, nthread=32,  seed=1127)
  newnrounds=tmp$best_iteration
  set.seed(1130)
  model=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                eta=0.1, max_depth=4, min_child_weight=3, subsample=0.8,
                colsample_bytree=0.5, eval_metric=c("auc"), verbose=0
  )
  prediction=predict(model,newx)
  prediction.candidate.xgboost=prediction
  pred.candidate.xgboost=prediction.performance(prediction,test$AKI,10)
  model.importance=xgb.importance(feature_names = features, model=model)
  ind=sort(model.importance$Feature,index.return=TRUE)$ix
  candidate.xgboost.features=model.importance$Gain[ind]
  
  # Model 5 candidate: xgboostrefit ######################################################
  # using selected vars from xgboost permutation method
  sel=c("HFpresent","priorShock","transfer=3","egfrc=(60,Inf]","egfrc=[0,30]","diabetes",
        "age","anemia","cadpresent=3","priorHF","egfrc=(30,45]","PriorCardiacArrest",
        "BMI","cadpresent=1","priorPAD","egfrc=(45,60]","priorCVD")
  x=x[,features%in%sel]
  newx=newx[,features%in%sel]
  set.seed(1127)
  tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.1, subsample=0.9, colsample_bytree=0.5, max_depth=5, 
             min_child_weight=1, verbose=0,
             stratified = TRUE, nthread=32,  seed=1127)
  newnrounds=tmp$best_iteration
  set.seed(1130)
  model=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                eta=0.1, max_depth=5, min_child_weight=1, subsample=0.9,
                colsample_bytree=0.5, eval_metric=c("auc"), verbose=0
  )
  prediction=predict(model,newx)
  prediction.candidate.xgboostrefit=prediction
  pred.candidate.xgboostrefit=prediction.performance(prediction,test$AKI,10)
  
  # Model 6 preproc: lasso ###############################################################
  data0=data.preproc
  train= data0[c.split, ]
  test = data0[-c.split, ]
  ind.out=33
  predictor=train[,-ind.out]
  y=train[,ind.out]
  x=predict(onehot(predictor),predictor)
  newx=predict(onehot(test[,-ind.out]),test[,-ind.out])
  vars.ex=c("AnginalClass=1","BetaBlockers","AdmtSource=3","CADPresentationcat=5")
  x=x[,-which(colnames(x)%in%vars.ex)]
  newx=newx[,-which(colnames(newx)%in%vars.ex)]
  features=colnames(x)
  set.seed(1127)
  folds=createFolds(factor(y),k=10)
  foldid=rep(NA,length(y))
  for (j in 1:length(folds)){
    foldid[folds[[j]]]=j
  }
  cvfit=cv.glmnet(x,y,foldid=foldid,family="binomial",type.measure="auc",parallel = TRUE)
  prediction=as.vector(predict(cvfit,newx = newx,s="lambda.1se",type="response"))
  prediction.preproc.lasso=prediction
  pred.preproc.lasso=prediction.performance(prediction,test$AKI,10)
  ind=predict(cvfit,s="lambda.1se",type="nonzero")$X1
  preproc.lasso.features=ind
  # Model 7 preproc.xgboost ################################################################
  set.seed(1127)
  tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.05, subsample=0.9, colsample_bytree=0.5, max_depth=5, 
             min_child_weight=5, verbose=0,
             stratified = TRUE, nthread=32,  seed=1127)
  newnrounds=tmp$best_iteration
  set.seed(1130)
  model=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                eta=0.05, max_depth=5, min_child_weight=5, subsample=0.9,
                colsample_bytree=0.5, eval_metric=c("auc"), verbose = 0
  )
  prediction=predict(model,newx)
  prediction.preproc.xgboost=prediction
  pred.preproc.xgboost=prediction.performance(prediction,test$AKI,10)
  model.importance=xgb.importance(feature_names = features, model=model)
  ind=sort(model.importance$Feature,index.return=TRUE)$ix
  preproc.xgboost.features=model.importance$Gain[ind]
  # Model 8 preproc: xgboostrefit ##########################################################
  sel=c("egfr","PreProcHgb","Prior2weeksHFcat=1","PriorCardioShock","Age",
        "PCIStatus=1","PCIStatus=3","PrePCILVEF","Diabetescat=3","Diabetescat=1",
        "BMI","CADPresentationcat=4","Prior2weeksHFcat=5","PriorHF",
        "PriorCardiacArrest" ,"AdmtSource=1")
  x=x[,features%in%sel]
  newx=newx[,features%in%sel]
  set.seed(1127)
  tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.1, subsample=0.9, colsample_bytree=0.6, max_depth=6, 
             min_child_weight=3, verbose=0,
             stratified = TRUE, nthread=32,  seed=1127)
  newnrounds=tmp$best_iteration
  set.seed(1130)
  model=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                eta=0.1, max_depth=6, min_child_weight=3, subsample=0.9,
                colsample_bytree=0.6, eval_metric=c("auc"), verbose=0
  )
  model.xgboost=model
  prediction=predict(model,newx)
  prediction.preproc.xgboostrefit=prediction
  pred.preproc.xgboostrefit=prediction.performance(prediction,test$AKI,10)
  
  #########################################################################################
  # saving all results
  list(pred.selected.logit,prediction.selected.logit,
       pred.selected.xgboost,prediction.selected.xgboost,
       pred.candidate.lasso,prediction.candidate.lasso,candidate.lasso.features,
       candidate.logit.features, pred.selectedall.logit, prediction.selectedall.logit,
       pred.candidate.xgboost,prediction.candidate.xgboost,candidate.xgboost.features,
       pred.candidate.xgboostrefit,prediction.candidate.xgboostrefit,
       pred.preproc.lasso,prediction.preproc.lasso,preproc.lasso.features,
       pred.preproc.xgboost,prediction.preproc.xgboost,preproc.xgboost.features,
       pred.preproc.xgboostrefit,prediction.preproc.xgboostrefit,
       test$AKI)
}