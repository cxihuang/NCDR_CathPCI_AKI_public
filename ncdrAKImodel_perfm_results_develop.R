# Results for comparing models in development cohort
library(metafor)
source("prediction_performance.R")

# extract results from experiments
# load perform.all

niter=100
selected.logit=matrix(NA,nrow = niter,ncol = 7) # performance measures
selected.xgboost=selected.logit
selected.predlogit=matrix(NA,nrow=niter,ncol=284126) # predictions
selected.predxgboost=selected.predlogit

candidate.lasso=matrix(NA,nrow = niter,ncol = 7)
candidate.predlasso=matrix(NA,nrow=niter,ncol=284126)
candidate.lassofeatures=matrix(NA,nrow=niter,ncol=27)
candidate.logitfeatures=matrix(NA,nrow=niter,ncol=20)
candidate.logit=selected.logit
candidate.predlogit=selected.predlogit
candidate.xgboost=candidate.lasso
candidate.predxgboost=candidate.predlasso
candidate.xgboostfeatures=candidate.lassofeatures
candidate.xgboostrefit=candidate.lasso
candidate.predxgboostrefit=candidate.predlasso

preproc.lasso=matrix(NA,nrow = niter,ncol = 7)
preproc.predlasso=matrix(NA,nrow=niter,ncol=284126)
preproc.lassofeatures=matrix(NA,nrow=niter,ncol=56)
preproc.xgboost=preproc.lasso
preproc.predxgboost=preproc.predlasso
preproc.xgboostfeatures=preproc.lassofeatures
preproc.xgboostrefit=preproc.lasso
preproc.predxgboostrefit=preproc.predlasso

testlabel=preproc.predlasso



for (i in 1:niter){
  tmp=perform.all[[i]]
  selected.logit[i,]=unlist(tmp[[1]])
  selected.predlogit[i,]=tmp[[2]]
  selected.xgboost[i,]=unlist(tmp[[3]])
  selected.predxgboost[i,]=tmp[[4]]
  
  candidate.lasso[i,]=unlist(tmp[[5]])
  candidate.predlasso[i,]=tmp[[6]]
  candidate.lassofeatures[i,tmp[[7]]]=1
  candidate.logitfeatures[i,]=tmp[[8]]
  candidate.logit[i,]=unlist(tmp[[9]])
  candidate.predlogit[i,]=tmp[[10]]
  
  candidate.xgboost[i,]=unlist(tmp[[11]])
  candidate.predxgboost[i,]=tmp[[12]]
  candidate.xgboostfeatures[i,]=tmp[[13]]
  candidate.xgboostrefit[i,]=unlist(tmp[[14]])
  candidate.predxgboostrefit[i,]=tmp[[15]]
  
  preproc.lasso[i,]=unlist(tmp[[16]])
  preproc.predlasso[i,]=tmp[[17]]
  preproc.lassofeatures[i,tmp[[18]]]=1
  preproc.xgboost[i,]=unlist(tmp[[19]])
  preproc.predxgboost[i,]=tmp[[20]]
  preproc.xgboostfeatures[i,]=tmp[[21]]
  preproc.xgboostrefit[i,]=unlist(tmp[[22]])
  preproc.predxgboostrefit[i,]=tmp[[23]]
  
  testlabel[i,]=tmp[[24]]
  
}


# generate forest plots for auc
selected.logit.auc=colMeans(selected.logit)[1]
selected.logit.aucsd=apply(selected.logit,2,sd)[1]
selected.xgboost.auc=colMeans(selected.xgboost)[1]
selected.xgboost.aucsd=apply(selected.xgboost,2,sd)[1]
candidate.lasso.auc=colMeans(candidate.lasso)[1]
candidate.lasso.aucsd=apply(candidate.lasso,2,sd)[1]
candidate.logit.auc=colMeans(candidate.logit)[1]
candidate.logit.aucsd=apply(candidate.logit,2,sd)[1]
candidate.xgboost.auc=colMeans(candidate.xgboost)[1]
candidate.xgboost.aucsd=apply(candidate.xgboost,2,sd)[1]
candidate.xgboostrefit.auc=colMeans(candidate.xgboostrefit)[1]
candidate.xgboostrefit.aucsd=apply(candidate.xgboostrefit,2,sd)[1]
preproc.lasso.auc=colMeans(preproc.lasso)[1]
preproc.lasso.aucsd=apply(preproc.lasso,2,sd)[1]
preproc.xgboost.auc=colMeans(preproc.xgboost)[1]
preproc.xgboost.aucsd=apply(preproc.xgboost,2,sd)[1]
preproc.xgboostrefit.auc=colMeans(preproc.xgboostrefit)[1]
preproc.xgboostrefit.aucsd=apply(preproc.xgboostrefit,2,sd)[1]
aucm=c(candidate.logit.auc,
       selected.logit.auc,selected.xgboost.auc,candidate.lasso.auc,candidate.xgboost.auc,candidate.xgboostrefit.auc,
       preproc.lasso.auc,preproc.xgboost.auc,preproc.xgboostrefit.auc)
aucsd=c(candidate.logit.aucsd,
        selected.logit.aucsd,selected.xgboost.aucsd,candidate.lasso.aucsd,candidate.xgboost.aucsd,candidate.xgboostrefit.aucsd,
        preproc.lasso.aucsd,preproc.xgboost.aucsd,preproc.xgboostrefit.aucsd)
forest(x=aucm,sei=aucsd,refline=aucm[2],at=c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0), digits = 3,xlab="", xlim=c(0.4,1.08),
       pch=15,psize=0.5,cex=.9,slab=c("  0. Logistic regression-Full",
                                      "  1. Logistic regression (baseline)","  2. XGBoost","  3. Lasso regression","  4. XGBoost-Full","  5. XGBoost",
                                      "  6. Lasso regression","  7. XGBoost-Full","  8. XGBoost"),
       rows=c(16,14,13,11,9,7,4,2,0),ylim=c(0,21)) 
text(1.08,21,"AUC (95% CI)",font=2,cex=1,pos=2)
text(0.44,21,"Method",font=2,cex=1,pos=2)
text(0.4,18,pos=4,"Strategy A",cex=.9,font=4)
text(0.4,17,pos=4," No variable selection",cex=.9,font=3)
text(0.4,15,pos=4," Backward selection",cex=.9,font=3)
text(0.4,12,pos=4," Lasso regularization",cex=.9,font=3)
text(0.4,10,pos=4," No variable selection",cex=.9,font=3)
text(0.4,8,pos=4," Permutation selection",cex=.9,font=3)
text(0.4,6,pos=4,"Strategy B",cex=.9,font=4)
text(0.4,5,pos=4," Lasso regularization",cex=.9,font=3)
text(0.4,3,pos=4," No variable selection",cex=.9,font=3)
text(0.4,1,pos=4," Permutation selection",cex=.9,font=3)

# forest plot for brier score
selected.logit.brier=colMeans(selected.logit)[4]
selected.logit.briersd=apply(selected.logit,2,sd)[4]
selected.xgboost.brier=colMeans(selected.xgboost)[4]
selected.xgboost.briersd=apply(selected.xgboost,2,sd)[4]
candidate.logit.brier=colMeans(candidate.logit)[4]
candidate.logit.briersd=apply(candidate.logit,2,sd)[4]
candidate.lasso.brier=colMeans(candidate.lasso)[4]
candidate.lasso.briersd=apply(candidate.lasso,2,sd)[4]
candidate.xgboost.brier=colMeans(candidate.xgboost)[4]
candidate.xgboost.briersd=apply(candidate.xgboost,2,sd)[4]
candidate.xgboostrefit.brier=colMeans(candidate.xgboostrefit)[4]
candidate.xgboostrefit.briersd=apply(candidate.xgboostrefit,2,sd)[4]
preproc.lasso.brier=colMeans(preproc.lasso)[4]
preproc.lasso.briersd=apply(preproc.lasso,2,sd)[4]
preproc.xgboost.brier=colMeans(preproc.xgboost)[4]
preproc.xgboost.briersd=apply(preproc.xgboost,2,sd)[4]
preproc.xgboostrefit.brier=colMeans(preproc.xgboostrefit)[4]
preproc.xgboostrefit.briersd=apply(preproc.xgboostrefit,2,sd)[4]
brierm=c(candidate.logit.brier,
         selected.logit.brier,selected.xgboost.brier,candidate.lasso.brier,candidate.xgboost.brier,candidate.xgboostrefit.brier,
         preproc.lasso.brier,preproc.xgboost.brier,preproc.xgboostrefit.brier)
briersd=c(candidate.logit.briersd,
          selected.logit.briersd,selected.xgboost.briersd,candidate.lasso.briersd,candidate.xgboost.briersd,candidate.xgboostrefit.briersd,
          preproc.lasso.briersd,preproc.xgboost.briersd,preproc.xgboostrefit.briersd)
forest(x=brierm,sei=briersd,refline=brierm[2],at=c(0.060,0.061,0.062,0.063,0.064), digits=4,
       xlab="", xlim=c(0.058,0.066),
       pch=16,psize=0.5,cex=.9,
       slab=c("  0. Logistic regression-Full",
              "  1. Logistic regression (baseline)","  2. XGBoost","  3. Lasso regression","  4. XGBoost-Full","  5. XGBoost",
              "  6. Lasso regression","  7. XGBoost-Full","  8. XGBoost"),
       rows=c(16,14,13,11,9,7,4,2,0),ylim=c(0,21)) 

text(0.066,21,"Brier score (95% CI)",font=2,cex=1,pos=2)
text(0.058,21,"Method",font=2,cex=1,pos=2)
text(0.058,18,pos=4,"Strategy A",cex=.9,font=4)
text(0.058,17,pos=4," No variable selection",cex=.9,font=3)
text(0.058,15,pos=4," Backward selection",cex=.9,font=3)
text(0.058,12,pos=4," Lasso regularization",cex=.9,font=3)
text(0.058,10,pos=4," No variable selection",cex=.9,font=3)
text(0.058,8,pos=4," Permutation selection",cex=.9,font=3)
text(0.058,6,pos=4,"Strategy B",cex=.9,font=4)
text(0.058,5,pos=4," Lasso regularization",cex=.9,font=3)
text(0.058,3,pos=4," No variable selection",cex=.9,font=3)
text(0.058,1,pos=4," Permutation selection",cex=.9,font=3)

# forest plot for resolution
selected.logit.res=colMeans(selected.logit)[6]
selected.logit.ressd=apply(selected.logit,2,sd)[6]
selected.xgboost.res=colMeans(selected.xgboost)[6]
selected.xgboost.ressd=apply(selected.xgboost,2,sd)[6]
candidate.logit.res=colMeans(candidate.logit)[6]
candidate.logit.ressd=apply(candidate.logit,2,sd)[6]
candidate.lasso.res=colMeans(candidate.lasso)[6]
candidate.lasso.ressd=apply(candidate.lasso,2,sd)[6]
candidate.xgboost.res=colMeans(candidate.xgboost)[6]
candidate.xgboost.ressd=apply(candidate.xgboost,2,sd)[6]
candidate.xgboostrefit.res=colMeans(candidate.xgboostrefit)[6]
candidate.xgboostrefit.ressd=apply(candidate.xgboostrefit,2,sd)[6]
preproc.lasso.res=colMeans(preproc.lasso)[6]
preproc.lasso.ressd=apply(preproc.lasso,2,sd)[6]
preproc.xgboost.res=colMeans(preproc.xgboost)[6]
preproc.xgboost.ressd=apply(preproc.xgboost,2,sd)[6]
preproc.xgboostrefit.res=colMeans(preproc.xgboostrefit)[6]
preproc.xgboostrefit.ressd=apply(preproc.xgboostrefit,2,sd)[6]
resm=c(candidate.logit.res,
       selected.logit.res,selected.xgboost.res,candidate.lasso.res,candidate.xgboost.res,candidate.xgboostrefit.res,
       preproc.lasso.res,preproc.xgboost.res,preproc.xgboostrefit.res)
ressd=c(candidate.logit.ressd,
        selected.logit.ressd,selected.xgboost.ressd,candidate.lasso.ressd,candidate.xgboost.ressd,candidate.xgboostrefit.ressd,
        preproc.lasso.ressd,preproc.xgboost.ressd,preproc.xgboostrefit.ressd)
forest(x=resm,sei=ressd,refline=resm[2],at=c(0.0030,0.0040,0.0050,0.0060), digits=4,
       xlab="", xlim=c(0.001,0.008),
       pch=16,psize=0.5,cex=.9,slab=c("  0. Logistic regression-Full",
                                      "  1. Logistic regression (baseline)","  2. XGBoost","  3. Lasso regression","  4. XGBoost-Full","  5. XGBoost",
                                      "  6. Lasso regression","  7. XGBoost-Full","  8. XGBoost"),
       rows=c(16,14,13,11,9,7,4,2,0),ylim=c(0,21)) 
text(0.009,21,"Resolution (95% CI)",font=2,cex=1,pos=2)
text(0.0010,21,"Method",font=2,cex=1,pos=2)
text(0.001,18,pos=4,"Strategy A",cex=.9,font=4)
text(0.001,17,pos=4," No variable selection",cex=.9,font=3)
text(0.001,15,pos=4," Backward selection",cex=.9,font=3)
text(0.001,12,pos=4," Lasso regularization",cex=.9,font=3)
text(0.001,10,pos=4," No variable selection",cex=.9,font=3)
text(0.001,8,pos=4," Permutation selection",cex=.9,font=3)
text(0.001,6,pos=4,"Strategy B",cex=.9,font=4)
text(0.001,5,pos=4," Lasso regularization",cex=.9,font=3)
text(0.001,3,pos=4," No variable selection",cex=.9,font=3)
text(0.001,1,pos=4," Permutation selection",cex=.9,font=3)

# forest plot for reliability
selected.logit.rel=colMeans(selected.logit)[5]
selected.logit.relsd=apply(selected.logit,2,sd)[5]
selected.xgboost.rel=colMeans(selected.xgboost)[5]
selected.xgboost.relsd=apply(selected.xgboost,2,sd)[5]
candidate.logit.rel=colMeans(candidate.logit)[5]
candidate.logit.relsd=apply(candidate.logit,2,sd)[5]
candidate.lasso.rel=colMeans(candidate.lasso)[5]
candidate.lasso.relsd=apply(candidate.lasso,2,sd)[5]
candidate.xgboost.rel=colMeans(candidate.xgboost)[5]
candidate.xgboost.relsd=apply(candidate.xgboost,2,sd)[5]
candidate.xgboostrefit.rel=colMeans(candidate.xgboostrefit)[5]
candidate.xgboostrefit.relsd=apply(candidate.xgboostrefit,2,sd)[5]
preproc.lasso.rel=colMeans(preproc.lasso)[5]
preproc.lasso.relsd=apply(preproc.lasso,2,sd)[5]
preproc.xgboost.rel=colMeans(preproc.xgboost)[5]
preproc.xgboost.relsd=apply(preproc.xgboost,2,sd)[5]
preproc.xgboostrefit.rel=colMeans(preproc.xgboostrefit)[5]
preproc.xgboostrefit.relsd=apply(preproc.xgboostrefit,2,sd)[5]
relm=c(candidate.logit.rel,
       selected.logit.rel,selected.xgboost.rel,candidate.lasso.rel,candidate.xgboost.rel,candidate.xgboostrefit.rel,
       preproc.lasso.rel,preproc.xgboost.rel,preproc.xgboostrefit.rel)
relsd=c(candidate.logit.relsd,
        selected.logit.relsd,selected.xgboost.relsd,candidate.lasso.relsd,candidate.xgboost.relsd,candidate.xgboostrefit.relsd,
        preproc.lasso.relsd,preproc.xgboost.relsd,preproc.xgboostrefit.relsd)
forest(x=relm*1e2,sei=relsd*1e2,refline=relm[2]*1e2,at=c(0,0.002,0.004,0.006),
       digits=4,xlab="", xlim=c(-0.006,0.011),
       pch=16,psize=0.5,cex=.9,slab=c("  0. Logistic regression-Full",
                                      "  1. Logistic regression (baseline)","  2. XGBoost","  3. Lasso regression","  4. XGBoost-Full","  5. XGBoost",
                                      "  6. Lasso regression","  7. XGBoost-Full","  8. XGBoost"),
       rows=c(16,14,13,11,9,7,4,2,0),ylim=c(0,21)) 

#text(0.025,13,expression(bold(paste("Reliability", 1%*%10^-2," (95% CI)"))),font=2,cex=1,pos=2)
text(-0.01,21,"Method",font=2,cex=1,pos=2)
text(-0.01,18,pos=4,"Strategy A",cex=.9,font=4)
text(-0.01,17,pos=4," No variable selection",cex=.9,font=3)
text(-0.01,15,pos=4," Backward selection",cex=.9,font=3)
text(-0.01,12,pos=4," Lasso regularization",cex=.9,font=3)
text(-0.01,10,pos=4," No variable selection",cex=.9,font=3)
text(-0.01,8,pos=4," Permutation selection",cex=.9,font=3)
text(-0.01,6,pos=4,"Strategy B",cex=.9,font=4)
text(-0.01,5,pos=4," Lasso regularization",cex=.9,font=3)
text(-0.01,3,pos=4," No variable selection",cex=.9,font=3)
text(-0.01,1,pos=4," Permutation selection",cex=.9,font=3)




# generate calibration plot
# for Model 1 and Model 8
nbins=10
selected.logit.cal=calibration.avg(selected.predlogit,testlabel,nbins,niter)
preproc.xgboostrefit.cal=calibration.avg(preproc.predxgboostrefit,testlabel,nbins,niter)
selected.logit.obs=selected.logit.cal[[1]]
selected.logit.pred=selected.logit.cal[[2]]
preproc.xgboostrefit.obs=preproc.xgboostrefit.cal[[1]]
preproc.xgboostrefit.pred=preproc.xgboostrefit.cal[[2]]
par(pty="s")
plot(colMeans(selected.logit.pred),colMeans(selected.logit.obs),type="b",lty=2,xlim=c(0,0.3),ylim=c(0,0.3),asp=1,xlab="Predicted risk",ylab="Observed risk")
points(colMeans(preproc.xgboostrefit.pred),colMeans(preproc.xgboostrefit.obs),type="b",pch=19)
lines(c(-1,1),c(-1,1),lty=3)
legend(0,0.30,legend=c("Model 1 (baseline)",
                       expression(paste("slope=1.036", phantom(.)%+-%phantom(.), "0.011")),
                       "Model 8 (XGBoost)",
                       expression(paste("slope=1.008", phantom(.)%+-%phantom(.), "0.010"))
),lty=c(2,0,1,0),pch=c(1,NA,19,NA),bty="n")

# shift table
iter=1 # choose a random experiment
breaks=c(0,0.05,0.10,0.25,0.5,1) # risk strata breaks
nbin=length(breaks)-1
obsr=matrix(NA,nbins,nbins)
obsn=obsr
pred1=selected.predlogit[iter,]
pred2=preproc.predxgboostrefit[iter,]
label=testlabel[iter,]
for (i in 1:nbins){
  for (j in 1:nbins){
    xmin=breaks[i]
    xmax=breaks[i+1]
    ymin=breaks[j]
    ymax=breaks[j+1]
    c=Reduce(intersect,list(which(a>=xmin),which(a<xmax),which(b>=ymin),which(b<ymax)))
    obsr[i,j]=mean(label[c])
    obsn[i,j]=length(c)
  }
}
t(obsr) # event rate
t(obsn) # n_patients
