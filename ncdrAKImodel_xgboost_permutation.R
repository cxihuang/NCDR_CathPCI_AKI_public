# permutation selection with xgboost 

library(doMC)
library(xgboost)
library(onehot)
library(Hmisc)



###################################################################################
# from Set A variables
# load data.candidate

data0=data.candidate

ntrees=1000
# one-hot encoding
ind.out=1
predictor=data0[,-ind.out]
y=data0[,ind.out]
x0=predict(onehot(predictor),predictor)
x1=apply(x0,2,sample)
colnames(x1)=paste("Shf",colnames(x1),sep="_")
x=cbind(x0,x1)

# check correlation
corm=rcorr(x0,type="spearman")
abscor=abs(corm$r)
abscor[lower.tri(abscor,diag=T)]=0

# run for 30 different random permutations
registerDoMC(10)
writeLines(c(""),"log.txt")
set.seed(1205, kind = "L'Ecuyer-CMRG")
gain.candidate=foreach(i=1:30)%dopar%{
  cat(paste("Starting iteration",i,"\n"),file="log.txt",append=TRUE)
  x1=apply(x0,2,sample)
  colnames(x1)=paste("Shf",colnames(x1),sep="_") # shadow variable with prefix "Shf"
  x=cbind(x0,x1)
  # start build model
  set.seed(1127)
  tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.1, subsample=0.8, colsample_bytree=0.5, max_depth=4, 
             min_child_weight=3, print_every_n = 10,
             stratified = TRUE, nthread=32)
  newnrounds=tmp$best_iteration
  set.seed(1130)
  model=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                eta=0.1, max_depth=4, min_child_weight=3, subsample=0.8,
                colsample_bytree=0.5, eval_metric=c("auc"), verbose=0
  )
  # sort by variable importance
  model.importance=xgb.importance(feature_names = colnames(x), model=model)
  gain=model.importance$Gain
  feature=model.importance$Feature
  # catch error
  if (length(feature)<dim(x)[2]){
    zerofeature=setdiff(colnames(x),feature)
    gain=c(gain,rep(0,length(zerofeature)))
    feature=c(feature,zerofeature)
  }
  feature.order=sort(feature,index.return=T) # sort feature name
  gain=gain[feature.order$ix]
  names(gain)=feature[feature.order$ix]
  out=gain
}
registerDoSEQ()

# examine importance of variables, compare to shadow version
gain.mat=matrix(NA,nrow = dim(x)[2],ncol = 30)
for (i in 1:30){
  gain.mat[,i]=gain.candidate[[i]]
}
gain.names=sort(colnames(x))
gain.mean=rowMeans(gain.mat)
gain.sd=apply(gain.mat,1,sd)
ind=sort(gain.mean,decreasing = TRUE,index.return=TRUE)
gain.names[ind$ix] # select those that are higher than shadow variables

############################################################################
# from Set B variables
# load data.preproc
data0=data.preproc
ind.out=1

ind.out=33
predictor=data0[,-ind.out]
y=data0[,ind.out]
x0=predict(onehot(predictor),predictor)

# check correlation
corm=rcorr(x0,type="spearman")
corm=rcorr(x0)
abscor=abs(corm$r)
abscor[lower.tri(abscor,diag=T)]=0
threshold=0.25
c.cor=which(abscor>threshold,arr.ind=T)
ccorn=data.frame(fcol=colnames(x0)[c.cor[,1]],scol=colnames(x0)[c.cor[,2]], score=abscor[c.cor])
ccorn
# exclude correlated variables
vars.ex=c("AnginalClass=1","BetaBlockers","AdmtSource=3","CADPresentationcat=5")
x0=x0[,-which(colnames(x0)%in%vars.ex)]

x1=apply(x0,2,sample)
colnames(x1)=paste("Shf",colnames(x1),sep="_")
x=cbind(x0,x1)

# run for 30 permutations
registerDoMC(5)
writeLines(c(""),"log.txt")
set.seed(1205, kind = "L'Ecuyer-CMRG")
gain.candidate=foreach(i=1:30)%dopar%{
  cat(paste("Starting iteration",i,"\n"),file="log.txt",append=TRUE)
  x1=apply(x0,2,sample)
  colnames(x1)=paste("Shf",colnames(x1),sep="_")# shadow variables
  x=cbind(x0,x1)
  # build model
  set.seed(1127)
  tmp=xgb.cv(data=x,label=as.numeric(y), nfold=5, nrounds=ntrees, objective="binary:logistic",
             early_stopping_rounds = 50, metrics=c("auc"),
             eta=0.05, subsample=0.9, colsample_bytree=0.5, max_depth=5, 
             min_child_weight=5, print_every_n = 10,
             stratified = TRUE, nthread=32)
  newnrounds=tmp$best_iteration
  set.seed(1130)
  model=xgboost(data=x,label=as.numeric(y),nrounds=newnrounds, objective="binary:logistic",
                eta=0.05, max_depth=5, min_child_weight=5, subsample=0.9,
                colsample_bytree=0.5, eval_metric=c("auc"), print_every_n = 10
  )
  # variable importance
  model.importance=xgb.importance(feature_names = colnames(x), model=model)
  gain=model.importance$Gain
  feature=model.importance$Feature
  if (length(feature)<dim(x)[2]){
    zerofeature=setdiff(colnames(x),feature)
    gain=c(gain,rep(0,length(zerofeature)))
    feature=c(feature,zerofeature)
  }
  feature.order=sort(feature,index.return=T)
  gain=gain[feature.order$ix]
  names(gain)=feature[feature.order$ix]
  out=gain
}
registerDoSEQ()

gain.mat=matrix(NA,nrow = dim(x)[2],ncol = 30)
for (i in 1:30){
  gain.mat[,i]=gain.candidate[[i]]
}
gain.names=sort(colnames(x))
gain.mean=rowMeans(gain.mat)
gain.sd=apply(gain.mat,1,sd)
ind=sort(gain.mean,decreasing = TRUE,index.return=TRUE)
gain.names[ind$ix]