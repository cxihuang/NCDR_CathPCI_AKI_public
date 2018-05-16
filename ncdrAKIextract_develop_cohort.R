# Data pre-processing
# for development cohort: June 1, 2009-June 30, 2011
library(lubridate)


# read from csv files
folder='/data/Projects/ACC_NCDR/NCDR/CATHPCI/CPV4/CPV4_10052011/csv/'
file_list=list.files(path=folder,pattern = "*.csv")
visit=read.csv(paste(folder,file_list[13],sep=''),header = T,sep = ',')
pepsd=read.csv(paste(folder,file_list[8],sep=''),header = T,sep = ',')
full.data=merge(visit,pepsd,by='EpisodeKey')
psubm=read.csv(paste(folder,file_list[11],sep=''),header = T,sep = ',')
full.data=merge(full.data,psubm,by='SubmissionPatientKey')

# select PCI time frame
DCDate=full.data$DCDate
PCI=full.data$PCI 
DCDate.conv=as.Date(DCDate,origin='1960-01-01')
DCDate.year=year(DCDate.conv)
DCDate.month=month(DCDate.conv)
sel=rep(1,length(DCDate.conv))# select based on date of receiving PCI: 2009/06/01-2011/06/30
sel[which(DCDate.year<2009)]=0
sel[which(DCDate.year>2011)]=0
sel[which((DCDate.year==2009) & (DCDate.month<6))]=0
sel[which((DCDate.year==2011) & (DCDate.month>6))]=0

# receiving PCI between dates
in.date=sel&PCI
full.data$in.date=in.date
sub.data=subset(full.data,in.date==1)
# exclusions
# discharged on the day of procedure
in.sdDC=rep(1,dim(sub.data)[1])
in.sdDC[sub.data$DCDate==sub.data$ProcedureDate]=0 
sub.data$in.sdDC=in.sdDC# 
# missing pre- and post- procedure creatinine
in.creat=rep(1,dim(sub.data)[1])
in.creat[(is.na(sub.data$PreProcCreat)|is.na(sub.data$PostProcCreat))&(sub.data$PostDialysis==0|is.na(sub.data$PostDialysis))]=0
sub.data$in.creat=in.creat# 
# currently on dialysis
in.curdialy=rep(1,dim(sub.data)[1])
in.curdialy[which(sub.data$CurrentDialysis==1)]=0
sub.data$in.curdialy=in.curdialy
# final cohort
final.data=subset(sub.data, in.sdDC&in.creat&in.curdialy&in.dupEpKey)

# outcomes
# AKI
change.creat=round(final.data$PostProcCreat-final.data$PreProcCreat,digits=1)
final.data$AKI=(change.creat>=0.3)|(final.data$PostProcCreat>=1.5*final.data$PreProcCreat)|(final.data$PostDialysis==1)

# feature extraction and engineering
final.data$Female=final.data$Sex==2
final.data$AdmtSource=factor(final.data$AdmtSource)
final.data$Transferin=final.data$AdmtSource==2
final.data$Smoker=final.data$Smoker==1
final.data$Hypertension=final.data$Hypertension==1
final.data$Dyslipidemia=final.data$Dyslipidemia==1
final.data$FamilyHxCAD=final.data$FamilyHxCAD==1
final.data$PriorMI=final.data$PriorMI==1
final.data$PriorHF=final.data$PriorHF==1
final.data$ValveSurgery=final.data$ValveSurgery==1
# pior PCI: NO/within last year/longer
PriorPCI=rep(NA,length(final.data$PriorPCI))
PriorPCI[final.data$PriorPCI==0]=1
elapse=final.data$ArrivalDate-final.data$PriorPCIDate
PriorPCI[final.data$PriorPCI==1 & elapse<=365]=2
PriorPCI[final.data$PriorPCI==1 & elapse>365]=3
final.data$PriorPCIcat=factor(PriorPCI)
final.data$PriorPCI=final.data$PriorPCI==1
# prior CABG: No/within last year/1-5 years/longer than 5 years
final.data$prior
PriorCABG=rep(NA,length(final.data$PriorCABG))
PriorCABG[final.data$PriorCABG==0]=1
elapse=final.data$ArrivalDate-final.data$PriorCABGDate
PriorCABG[final.data$PriorCABG==1 & elapse<=365]=2
PriorCABG[final.data$PriorCABG==1 & elapse>365 & elapse<=5*365]=3
PriorCABG[final.data$PriorCABG==1 & elapse>5*365]=4
final.data$PriorCABGcat=factor(PriorCABG)
final.data$PriorCABG=final.data$PriorCABG==1
final.data$PriorCVD=final.data$PriorCVD==1
final.data$PriorPAD=final.data$PriorPAD==1
final.data$ChronicLungDisease=final.data$ChronicLungDisease==1
# diabetes: No/oral/insulin/diet or other or none
Diabetes=rep(NA,length(final.data$Diabetes))
Diabetes[final.data$Diabetes==0]=1
Diabetes[final.data$Diabetes==1 & final.data$DiabetesControl==3]=2
Diabetes[final.data$Diabetes==1 & final.data$DiabetesControl==4]=3
Diabetes[final.data$Diabetes==1 & (final.data$DiabetesControl %in% c(1,2,5))]=4
final.data$Diabetescat=factor(Diabetes)
final.data$Diabetes=final.data$Diabetes==1
# CAD presentation: no sxs or no angina or sx unlikely to be ischemic/stable angina/unstable angina/
#non-stemi/ stemi and thrombolytics/ stemi and non-thrombolytics
CADPresentation=rep(NA,length(final.data$CADPresentation))
CADPresentation[final.data$CADPresentation %in% c(1,2)]=1
CADPresentation[final.data$CADPresentation==3]=2
CADPresentation[final.data$CADPresentation==4]=3
CADPresentation[final.data$CADPresentation==5]=4
CADPresentation[final.data$CADPresentation==6 & final.data$ThromTherapy==0]=5
CADPresentation[final.data$CADPresentation==6 & final.data$ThromTherapy==1]=6
final.data$CADPresentationcat=factor(CADPresentation)
final.data$AnginalClass=factor(final.data$AnginalClass)
# anti-anginal meds: No/1 medication/?1 medications
AntiAnginalMed=rep(NA,length(final.data$AntiAnginalMed))
AntiAnginalMed[final.data$AntiAnginalMed==0]=1
nmeds=final.data$AA_BetaBlockers+final.data$AA_CaChannel+final.data$AA_LongActingNitrates+final.data$AA_Ranolazine+
  final.data$AA_OtherAgent
AntiAnginalMed[final.data$AntiAnginalMed==1 & nmeds==1]=2
AntiAnginalMed[final.data$AntiAnginalMed==1 & nmeds>1]=3
final.data$AntiAnginalMedcat=factor(AntiAnginalMed)
final.data$BetaBlockers=final.data$AA_BetaBlockers==1
final.data$BetaBlockers[final.data$AntiAnginalMed==0]=FALSE
# heart failure w/in2 weeks: NO/class1/classII/classIII/classIV
Prior2weeksHF=rep(NA,length(final.data$Prior2weeksHF))
Prior2weeksHF[final.data$Prior2weeksHF==0]=1
Prior2weeksHF[final.data$Prior2weeksHF==1 & final.data$Prior2weekNYHA==1]=2
Prior2weeksHF[final.data$Prior2weeksHF==1 & final.data$Prior2weekNYHA==2]=3
Prior2weeksHF[final.data$Prior2weeksHF==1 & final.data$Prior2weekNYHA==3]=4
Prior2weeksHF[final.data$Prior2weeksHF==1 & final.data$Prior2weekNYHA==4]=5
final.data$Prior2weeksHFcat=factor(Prior2weeksHF)
final.data$Prior2weeksHF=final.data$Prior2weeksHF==1
final.data$CardioLVSD=final.data$CardioLVSD==1
final.data$PriorCardioShock=final.data$PriorCardioShock==1
final.data$PriorCardiacArrest=final.data$PriorCardiacArrest==1
final.data$StressImaging=final.data$StressImaging==1
# IABP at the start of procedure
final.data$IABPst=final.data$IABP==1 & final.data$IABPTiming==1
final.data$MVSupportst=final.data$MVSupport==1 & final.data$MVSupportTiming==1
final.data$PCIStatus=factor(final.data$PCIStatus) 

# eGFR
age=final.data$Age
creatinine=final.data$PreProcCreat
female=final.data$Sex==2
black=final.data$RaceBlack==1
fmfactor= ifelse(female, 0.742,1)
blkfactor= ifelse(black, 1.210,1) 
egfr=186.3*((creatinine)^(-1.154))*(age^(-0.203))*fmfactor*blkfactor
egfrc=cut(egfr,breaks = c(0,30,45,60,Inf),include.lowest = T)# 4 levels
final.data$egfrc=factor(egfrc,levels(egfrc)[c(4,3,2,1)])
final.data$egfr=egfr
# BMI
final.data$BMI=10000*final.data$Weight/(final.data$Height^2)
# Anemia
final.data$Anemia=final.data$PreProcHgb<10
# cadpresent as in original paper
cadpresent=final.data$CADPresentation
cadpresent.dup=cadpresent
cadpresent[which(cadpresent.dup==1|cadpresent.dup==2|cadpresent.dup==3)]=1 # normal CAD presentation
cadpresent[which(cadpresent.dup==4|cadpresent.dup==5)]=2 # NSTEMI/Unstable angina
cadpresent[which(cadpresent.dup==6)]=3 # STEMI
cadpresent[is.na(cadpresent)]=NA
final.data$cadpresent=factor(cadpresent)

# Set A variables
vars_seta=c("AKI","Age","Female","BMI","IABPst","egfrc","Prior2weeksHF","Diabetes","Hypertension",
            "PriorMI","PriorHF","PriorPCI","PriorCABG","PriorCVD","PriorPAD","ChronicLungDisease",
            "cadpresent","PriorCardioShock","PriorCardiacArrest","Anemia","Transferin")

# Set B variables
vars_setb=c("AKI","Age","Female","AdmtSource","Smoker","Hypertension","Dyslipidemia", "FamilyHxCAD","PriorMI",
            "PriorHF", "ValveSurgery", "PriorPCIcat","PriorCABGcat","PriorCVD","PriorPAD",
            "ChronicLungDisease","Diabetescat","CADPresentationcat","AnginalClass","AntiAnginalMedcat",
            "BetaBlockers","Prior2weeksHFcat","CardioLVSD","PriorCardiacArrest","StressImaging",
            "IABPst","MVSupportst","PCIStatus","PrePCILVEF","PriorCardioShock","PreProcHgb",
            "egfr","BMI")

final.data=final.data[,colnames(final.data)%in%union(vars_seta,vars_setb)]

# imputation
final.imp=final.data
# categorical: mode imputation
calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
tmp=final.imp
type=sapply(tmp,class)
tmp=tmp[,type=="factor"|type=="logical"]
tmpnames=names(tmp)
for (i in 1:dim(tmp)[2]){
  curtmp=tmp[,i]
  curtmp[is.na(curtmp)]=calculate_mode(curtmp)
  final.imp[,tmpnames[i]]=curtmp
}
# continuous: median imputation
tmp=final.imp
type=sapply(tmp,class)
tmp=tmp[,type=="integer"|type=="numeric"]
tmpnames=names(tmp)
for (i in 1:dim(tmp)[2]){
  curtmp=tmp[,i]
  curtmp[is.na(curtmp)]=median(curtmp,na.rm=T)
  final.imp[,tmpnames[i]]=curtmp
}
# sanity check
sapply(final.imp, function(x) 100*length(which(is.na(x)))/length(x))

# Set A variables
data.candidate=final.imp[,colnames(final.imp)%in%vars_seta]
# Set B variables
data.preproc=final.imp[,colnames(final.imp)%in%vars_setb]


