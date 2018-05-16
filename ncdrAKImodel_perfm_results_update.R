# results for temporal validation

# summarize results
# load perform.update

niter=100

pred1.selected=matrix(NA,niter,7)
pred2.selected=pred1.selected
pred3.selected=pred1.selected
pred4.selected=pred1.selected
pred5.selected=pred1.selected
pred6.selected=pred1.selected
pred7.selected=pred1.selected

pred1.preproc=pred1.selected
pred2.preproc=pred1.selected
pred3.preproc=pred1.selected
pred4.preproc=pred1.selected
pred5.preproc=pred1.selected
pred6.preproc=pred1.selected
pred7.preproc=pred1.selected

prediction.test=matrix(NA,niter,291260)
prediction.recalib=prediction.test
prediction.new=prediction.test
prediction.comb=prediction.test
testlabel=prediction.test

for (i in 1:niter){
  tmp=perform.update[[i]]
  pred1.selected[i,]=unlist(tmp[[1]])
  pred2.selected[i,]=unlist(tmp[[2]])
  pred3.selected[i,]=unlist(tmp[[3]])
  pred4.selected[i,]=unlist(tmp[[4]])
  pred5.selected[i,]=unlist(tmp[[5]])
  pred6.selected[i,]=unlist(tmp[[6]])
  pred7.selected[i,]=unlist(tmp[[7]])
  
  pred1.preproc[i,]=unlist(tmp[[8]])
  pred2.preproc[i,]=unlist(tmp[[9]])
  pred3.preproc[i,]=unlist(tmp[[10]])
  pred4.preproc[i,]=unlist(tmp[[11]])
  pred5.preproc[i,]=unlist(tmp[[12]])
  pred6.preproc[i,]=unlist(tmp[[13]])
  pred7.preproc[i,]=unlist(tmp[[14]])
  
  prediction.test[i,]=tmp[[15]]
  prediction.recalib[i,]=tmp[[16]]
  prediction.new[i,]=tmp[[17]]
  prediction.comb[i,]=tmp[[18]]
  
  testlabel[i,]=tmp[[19]]
}

# generating plots#############################################
# generate plot for comparison in auc
pred1=pred1.selected
pred3=pred3.selected
pred4=pred4.selected
pred7=pred7.selected
pred1.auc=colMeans(pred1)[1]
pred1.aucsd=apply(pred1,2,sd)[1]
pred3.auc=colMeans(pred3)[1]
pred3.aucsd=apply(pred3,2,sd)[1]
pred4.auc=colMeans(pred4)[1]
pred4.aucsd=apply(pred4,2,sd)[1]
pred7.auc=colMeans(pred7)[1]
pred7.aucsd=apply(pred7,2,sd)[1]
aucm=c(pred1.auc,pred3.auc,pred4.auc,pred7.auc)
aucsd=c(pred1.aucsd,pred3.aucsd,pred4.aucsd,pred7.aucsd)
par(las=2)
errbar(c(1,2,3,4),aucm,aucm+1.96*aucsd,aucm-1.96*aucsd,lty=1,xlab='',ylab='',ylim=c(0.5,1),cex=0.3, xaxt="n")
lines(c(1,2,3,4),aucm,type = "l",lty=3)
pred1=pred1.preproc
pred3=pred3.preproc
pred4=pred4.preproc
pred7=pred7.preproc
pred1.auc=colMeans(pred1)[1]
pred1.aucsd=apply(pred1,2,sd)[1]
pred3.auc=colMeans(pred3)[1]
pred3.aucsd=apply(pred3,2,sd)[1]
pred4.auc=colMeans(pred4)[1]
pred4.aucsd=apply(pred4,2,sd)[1]
pred7.auc=colMeans(pred7)[1]
pred7.aucsd=apply(pred7,2,sd)[1]
aucm=c(pred1.auc,pred3.auc,pred4.auc,pred7.auc)
aucsd=c(pred1.aucsd,pred3.aucsd,pred4.aucsd,pred7.aucsd)
par(new=T)
errbar(c(1,2,3,4),aucm,aucm+1.96*aucsd,aucm-1.96*aucsd,lty=1,xlab='',ylab='',ylim=c(0.5,1),cex=0.3, xaxt="n")
lines(c(1,2,3,4),aucm,type = "l",lty=1)
legend(1,1,legend=c("Model 1 (Baseline)","Model 8 (XGBoost)"),lty=c(3,1),bty="n")
axis(1, at=1:4, labels=c("Strategy 1","Strategy 2","Strategy 3","Strategy 4"))

# for brier score
pred1=pred1.selected
pred3=pred3.selected
pred4=pred4.selected
pred7=pred7.selected
pred1.brier=colMeans(pred1)[4]
pred1.briersd=apply(pred1,2,sd)[4]
pred3.brier=colMeans(pred3)[4]
pred3.briersd=apply(pred3,2,sd)[4]
pred4.brier=colMeans(pred4)[4]
pred4.briersd=apply(pred4,2,sd)[4]
pred7.brier=colMeans(pred7)[4]
pred7.briersd=apply(pred7,2,sd)[4]
brierm=c(pred1.brier,pred3.brier,pred4.brier,pred7.brier)
briersd=c(pred1.briersd,pred3.briersd,pred4.briersd,pred7.briersd)
par(las=2)
errbar(c(1,2,3,4),brierm,brierm+1.96*briersd,brierm-1.96*briersd,lty=1,xlab='',ylab='',ylim=c(0.06,0.065),cex=0.3,xaxt="n")
lines(c(1,2,3,4),brierm,type = "l",lty=3)
pred1=pred1.preproc
pred3=pred3.preproc
pred4=pred4.preproc
pred7=pred7.preproc
pred1.brier=colMeans(pred1)[4]
pred1.briersd=apply(pred1,2,sd)[4]
pred3.brier=colMeans(pred3)[4]
pred3.briersd=apply(pred3,2,sd)[4]
pred4.brier=colMeans(pred4)[4]
pred4.briersd=apply(pred4,2,sd)[4]
pred7.brier=colMeans(pred7)[4]
pred7.briersd=apply(pred7,2,sd)[4]
brierm=c(pred1.brier,pred3.brier,pred4.brier,pred7.brier)
briersd=c(pred1.briersd,pred3.briersd,pred4.briersd,pred7.briersd)
par(new=T)
errbar(c(1,2,3,4),brierm,brierm+1.96*briersd,brierm-1.96*briersd,lty=1,xlab='',ylab='',ylim=c(0.06,0.065),cex=0.3,xaxt="n")
lines(c(1,2,3,4),brierm,type = "l",lty=1)
legend(1,1,legend=c("Model 1 (Baseline)","Model 8 (XGBoost)"),lty=c(3,1),bty="n")
axis(1, at=1:4, labels=c("Strategy 1","Strategy 2","Strategy 3","Strategy 4"))

# for reliability
pred1=pred1.selected
pred3=pred3.selected
pred4=pred4.selected
pred7=pred7.selected
pred1.rel=colMeans(pred1)[5]
pred1.relsd=apply(pred1,2,sd)[5]
pred3.rel=colMeans(pred3)[5]
pred3.relsd=apply(pred3,2,sd)[5]
pred4.rel=colMeans(pred4)[5]
pred4.relsd=apply(pred4,2,sd)[5]
pred7.rel=colMeans(pred7)[5]
pred7.relsd=apply(pred7,2,sd)[5]
relm=c(pred1.rel,pred3.rel,pred4.rel,pred7.rel)*1e2
relsd=c(pred1.relsd,pred3.relsd,pred4.relsd,pred7.relsd)*1e2
par(las=2)
errbar(c(1,2,3,4),relm,relm+1.96*relsd,relm-1.96*relsd,lty=1,xlab='',ylab='',ylim=c(0,0.02e-2)*1e2,cex=0.3,xaxt="n")
lines(c(1,2,3,4),relm,type = "l",lty=3)
pred1=pred1.preproc
pred3=pred3.preproc
pred4=pred4.preproc
pred7=pred7.preproc
pred1.rel=colMeans(pred1)[5]
pred1.relsd=apply(pred1,2,sd)[5]
pred3.rel=colMeans(pred3)[5]
pred3.relsd=apply(pred3,2,sd)[5]
pred4.rel=colMeans(pred4)[5]
pred4.relsd=apply(pred4,2,sd)[5]
pred7.rel=colMeans(pred7)[5]
pred7.relsd=apply(pred7,2,sd)[5]
relm=c(pred1.rel,pred3.rel,pred4.rel,pred7.rel)*1e2
relsd=c(pred1.relsd,pred3.relsd,pred4.relsd,pred7.relsd)*1e2
par(new=T)
errbar(c(1,2,3,4),relm,relm+1.96*relsd,relm-1.96*relsd,lty=1,xlab='',ylab='',ylim=c(0,0.02e-2)*1e2,cex=0.3,xaxt="n")
lines(c(1,2,3,4),relm,type = "l",lty=1)
legend(1,1,legend=c("Model 1 (Baseline)","Model 8 (XGBoost)"),lty=c(3,1),bty="n")
axis(1, at=1:4, labels=c("Strategy 1","Strategy 2","Strategy 3","Strategy 4"))

# for resolution
pred1=pred1.selected
pred3=pred3.selected
pred4=pred4.selected
pred7=pred7.selected
pred1.res=colMeans(pred1)[6]
pred1.ressd=apply(pred1,2,sd)[6]
pred3.res=colMeans(pred3)[6]
pred3.ressd=apply(pred3,2,sd)[6]
pred4.res=colMeans(pred4)[6]
pred4.ressd=apply(pred4,2,sd)[6]
pred7.res=colMeans(pred7)[6]
pred7.ressd=apply(pred7,2,sd)[6]
resm=c(pred1.res,pred3.res,pred4.res,pred7.res)
ressd=c(pred1.ressd,pred3.ressd,pred4.ressd,pred7.ressd)
par(las=2)
errbar(c(1,2,3,4),resm,resm+1.96*ressd,resm-1.96*ressd,lty=1,xlab='',ylab='',ylim=c(0.005,0.008),cex=0.3,xaxt="n")
lines(c(1,2,3,4),resm,type = "l",lty=3)
pred1=pred1.preproc
pred3=pred3.preproc
pred4=pred4.preproc
pred7=pred7.preproc
pred1.res=colMeans(pred1)[6]
pred1.ressd=apply(pred1,2,sd)[6]
pred3.res=colMeans(pred3)[6]
pred3.ressd=apply(pred3,2,sd)[6]
pred4.res=colMeans(pred4)[6]
pred4.ressd=apply(pred4,2,sd)[6]
pred7.res=colMeans(pred7)[6]
pred7.ressd=apply(pred7,2,sd)[6]
resm=c(pred1.res,pred3.res,pred4.res,pred7.res)
ressd=c(pred1.ressd,pred3.ressd,pred4.ressd,pred7.ressd)
par(new=T)
errbar(c(1,2,3,4),resm,resm+1.96*ressd,resm-1.96*ressd,lty=1,xlab='',ylab='',ylim=c(0.005,0.008),cex=0.3,xaxt="n")
lines(c(1,2,3,4),resm,type = "l",lty=1)
legend(1,1,legend=c("Model 1 (Baseline)","Model 8 (XGBoost)"),lty=c(3,1),bty="n")
axis(1, at=1:4, labels=c("Strategy 1","Strategy 2","Strategy 3","Strategy 4"))

