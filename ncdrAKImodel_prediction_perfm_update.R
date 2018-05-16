# performance comparison for two models in updating cohort
# three different updating strategy
# Strategy 1: apply without modification
# Strategy 2: logistic re-calibration
# Strategy 3: re-train on new data
# Strategy 4: re-train on old and new data and re-calibration on new data
# from 100 iterations
library(xgboost)
library(doMC)
library(caret)
library(onehot)
source("prediction_performance.R")


# load data.selected, data.selected.new, data.preproc, data.preproc.new

# one-hot encoding for Model 8
sel=c("egfr","PreProcHgb","Prior2weeksHFcat=1","PriorCardioShock","Age",
      "PCIStatus=1","PCIStatus=3","PrePCILVEF","Diabetescat=3","Diabetescat=1",
      "BMI","CADPresentationcat=4","Prior2weeksHFcat=5","PriorHF",
      "PriorCardiacArrest" ,"AdmtSource=1")
ind.out=1
old=predict(onehot(data.preproc[,-ind.out]),data.preproc[,-ind.out])
old=old[,colnames(old)%in%sel]
x.new=predict(onehot(data.preproc.new[,-ind.out]),data.preproc.new[,-ind.out])
x.new=x.new[,colnames(x.new)%in%sel]
y.new=data.preproc.new$AKI

# build old model on old data
set.seed(0220)
c.split = createDataPartition(data.selected$AKI, p = 0.7, list = F)
# model 1
train.selected=data.selected[c.split,]
model.selected=glm(AKI~.,family = binomial(),data=train.selected)
# model 8
x=old[c.split,]
y=data.preproc$AKI[c.split]
ntrees=1000
set.seed(1127)
tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
           early_stopping_rounds = 50, metrics=c("auc"),
           eta=0.1, subsample=0.9, colsample_bytree=0.6, max_depth=6, 
           min_child_weight=3, print_every_n = 10,
           stratified = TRUE, nthread=32,  seed=1127)
newnrounds=tmp$best_iteration
set.seed(1130)
model.preproc1=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                       eta=0.1, max_depth=6, min_child_weight=3, subsample=0.9,
                       colsample_bytree=0.6, eval_metric=c("auc"), print_every_n = 10
)

# validate old model on new data
registerDoMC(10)
writeLines(c(""),"log.txt")
set.seed(20180425, kind = "L'Ecuyer-CMRG")
niter=100
ntrees=1000
perform.update=foreach(i=1:niter) %dopar% {
  cat(paste("Starting iteration",i,"\n"),file="log.txt",append=TRUE)
  c.split.new = createDataPartition(data.selected.new$AKI, p = 0.7, list = F)
  
  # Model 1
  # split new data into an updating set and a validation set
  trains=data.selected.new[c.split.new,]
  tests=data.selected.new[-c.split.new,]
  # Strategy 1###########################################################
  prediction.test=predict(model.selected,newdata = tests,type="response")
  pred1.selected=prediction.performance(prediction.test,tests$AKI,10)
  # Strategy 2###########################################################
  LinearPred.train=predict(model.selected,newdata = trains)
  trains$lp=LinearPred.train
  model.selected.recalib=glm(AKI~lp, family=binomial(),data=trains)
  prediction.recalib.train=predict(model.selected.recalib,newdata=trains,type="response")
  pred2.selected=prediction.performance(prediction.recalib.train, trains$AKI,10)
  LinearPred.test=predict(model.selected,tests)
  tests$lp=LinearPred.test
  prediction.recalib.test=predict(model.selected.recalib, tests, type="response")
  pred3.selected=prediction.performance(prediction.recalib.test, tests$AKI,10)
  # Strategy 3############################################################
  trains$lp=NULL
  model.selected.new=glm(AKI~.,family = binomial(),data=trains)
  prediction.new.test=predict(model.selected.new,newdata = tests,type="response")
  pred4.selected=prediction.performance(prediction.new.test,tests$AKI,10)
  # Strategy 4#############################################################
  comb.selected=rbind(data.selected,trains)
  model.selected.comb=glm(AKI~.,family = binomial(),data=comb.selected)
  prediction.comb.test=predict(model.selected.comb,newdata = tests, type="response")
  pred5.selected=prediction.performance(prediction.comb.test,tests$AKI,10)
  LinearPred.comb.train=predict(model.selected.comb,newdata = trains)
  trains$lp=LinearPred.comb.train
  model.selected.comb.recalib=glm(AKI~lp, family=binomial(),data=trains)
  prediction.comb.recalib.train=predict(model.selected.comb.recalib,newdata=trains,type="response")
  pred6.selected=prediction.performance(prediction.comb.recalib.train, trains$AKI,10)
  LinearPred.comb.test=predict(model.selected.comb,tests)
  tests$lp=LinearPred.comb.test
  prediction.comb.recalib.test=predict(model.selected.comb.recalib, tests, type="response")
  pred7.selected=prediction.performance(prediction.comb.recalib.test, tests$AKI,10)
  
  # Model 8: xgboost model
  # split into updating and validation sets
  trains=x.new[c.split.new,]
  tests=x.new[-c.split.new,]
  y.trains=y.new[c.split.new]
  y.tests=y.new[-c.split.new]
  # Strategy 1###################################################################
  prediction.test=predict(model.preproc1,tests)
  pred1.preproc=prediction.performance(prediction.test,y.tests,10)
  # Strategy 2###################################################################
  prediction.train=predict(model.preproc1,trains)
  LinearPred.train=log(prediction.train/(1-prediction.train))
  trains.tmp=data.frame(AKI=y.trains,lp=LinearPred.train)
  model.preproc.recalib=glm(AKI~lp, family=binomial(),data=trains.tmp)
  prediction.recalib.train=predict(model.preproc.recalib,newdata=trains.tmp,type="response")
  pred2.preproc=prediction.performance(prediction.recalib.train, y.trains,10)
  LinearPred.test=log(prediction.test/(1-prediction.test))
  tests.tmp=data.frame(AKI=y.tests,lp=LinearPred.test)
  prediction.recalib.test=predict(model.preproc.recalib, tests.tmp, type="response")
  # Strategy 3###################################################################
  pred3.preproc=prediction.performance(prediction.recalib.test, y.tests,10)
  set.seed(1127)
  tmp=xgb.cv(data=trains,label=as.numeric(y.trains), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.1, subsample=0.9, colsample_bytree=0.6, max_depth=6, 
             min_child_weight=3, print_every_n = 10,
             stratified = TRUE, nthread=32,  seed=1127)
  newnrounds=tmp$best_iteration
  set.seed(1130) 
  model.preproc2=xgboost(data=trains,label=as.numeric(y.trains),nrounds=newnrounds, objective="binary:logistic",
                         eta=0.1, max_depth=6, min_child_weight=3, subsample=0.9,
                         colsample_bytree=0.6, eval_metric=c("auc"), print_every_n = 10
  )
  prediction.new.test=predict(model.preproc2,tests)
  pred4.preproc=prediction.performance(prediction.new.test,y.tests,10)
  # Strategy 4###################################################################
  comb.x=rbind(old,trains)
  comb.y=c(data.preproc$AKI, y.trains)
  set.seed(1127)
  tmp=xgb.cv(data=comb.x,label=as.numeric(comb.y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.1, subsample=0.9, colsample_bytree=0.6, max_depth=6,
             min_child_weight=3, print_every_n = 10,
             stratified = TRUE, nthread=32,  seed=1127)
  newnrounds=tmp$best_iteration
  set.seed(1130)
  model.preproc3=xgboost(data=comb.x,label=as.numeric(comb.y),nrounds=newnrounds, objective="binary:logistic",
                         eta=0.1, max_depth=6, min_child_weight=3, subsample=0.9,
                         colsample_bytree=0.6, eval_metric=c("auc"), print_every_n = 10
  )
  prediction.comb.test=predict(model.preproc3,tests)
  pred5.preproc=prediction.performance(prediction.comb.test,y.tests,10)
  prediction.comb.train=predict(model.preproc3,trains)
  LinearPred.comb.train=log(prediction.comb.train/(1-prediction.comb.train))
  trains.tmp=data.frame(AKI=y.trains,lp=LinearPred.comb.train)
  model.preproc.comb.recalib=glm(AKI~lp, family=binomial(),data=trains.tmp)
  prediction.comb.recalib.train=predict(model.preproc.comb.recalib, trains.tmp, type="response")
  pred6.preproc=prediction.performance(prediction.comb.recalib.train,y.trains,10)
  prediction.comb.test=predict(model.preproc3,tests)
  LinearPred.comb.test=log(prediction.comb.test/(1-prediction.comb.test))
  tests.tmp=data.frame(AKI=y.tests,lp=LinearPred.comb.test)
  prediction.comb.recalib.test=predict(model.preproc.comb.recalib, tests.tmp, type="response")
  pred7.preproc=prediction.performance(prediction.comb.recalib.test, y.tests,10)
  
  
  list(pred1.selected,pred2.selected,pred3.selected,pred4.selected,pred5.selected,pred6.selected,
       pred7.selected,
       pred1.preproc,pred2.preproc,pred3.preproc,pred4.preproc,pred5.preproc,pred6.preproc,
       pred7.preproc,
       prediction.test,prediction.recalib.test,prediction.new.test,prediction.comb.recalib.test,
       y.tests)
}
registerDoSEQ()

