# calculate performance measures


library(ROCR) # for calculating AUC


# auc, brier score, reliability, resolution and uncertainty
# reliability and resolution are calculated in nbins
# Input: prediction: predicted risk
#        label: ground truth binary outcome
#        nbins: number of bins for calibration, reliability and resolution
# Output: a list of auc, calibration intercept, calibration slope, brier score, 
#                   reliability, resolution and uncertainty
prediction.performance=function(prediction,label,nbins){
  pred=prediction(prediction,label)
  perf=performance(pred,"auc")
  auc=perf@y.values 
  
  predgrp=cut(prediction,breaks=quantile(prediction,probs = seq(0,1,1/nbins)),include.lowest = T)
  obs=rep(0,nbins)
  pred=rep(0,nbins)
  for (k in 1:nbins){
    grp=which(predgrp==levels(predgrp)[k])
    obs[k]=length(which(label[grp]==1))/length(grp)
    pred[k]=mean(prediction[grp])
  }
  data.AKIcalib=data.frame(obs=obs,pred=pred)
  calib=lm(obs~pred, data=data.AKIcalib)
  
  brier=sum((prediction-as.numeric(label))**2)/length(prediction)
  
  
  rel=(1/nbins)*sum((pred-obs)**2)
  res=(1/nbins)*sum((obs-mean(label))**2)
  unc=mean(label)*(1-mean(label))
  
  out=c(auc,calib$coefficients,brier,rel,res,unc)
  
  return(out)
}

# calibration plot
# Input: predictions: n_iter x n_patients
#        testlabel: n_iter x n_patients
#        nbins: number of bins
#        niter: number of experiments
# Output: a list, obs: observed risk, pred: mean predicted risk
calibration.avg=function(predictions,testlabel,nbins,niter){
  tmp=as.vector(t(predictions))
  breaks=quantile(tmp,probs = seq(0,1,1/nbins))
  obs=matrix(0,nrow = niter,ncol = nbins)
  pred=obs
  for (i in 1:niter){
    prediction=predictions[i,]
    label=testlabel[i,]
    predgrp=cut(prediction,breaks=breaks,include.lowest = T)
    for (k in 1:nbins){
      grp=which(predgrp==levels(predgrp)[k])
      obs[i,k]=length(which(label[grp]==1))/length(grp)
      pred[i,k]=mean(prediction[grp])
    }
  }
  out=list(obs,pred)
  return(out)
}




