library(readxl)
library(tidyverse)
library(lubridate)
library(factoextra)
library(DataExplorer)
library(missForest)
library(cluster)
library(ggalluvial)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggsci)
library(CBCgrps)
library(Publish)
library(tidyLPA)
library(data.table)
library(openxlsx)



#####批量导入数据
path <-"~/Desktop/albumin_sep/"
files<-list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

#2.数据清洗
#2.1定义sepsis人群
#年龄>=18,第一次住院的第一次入ICU,入ICU时间和入院时间>=1
attach(cirrhosis)
summary(cirrhosis)

patient_new<-cirrhosis%>%
  filter(icustay_seq==1)%>%
  filter(los_hospital>=1)%>%
  filter(los_icu>=1)%>%
  filter(los_icu<=100)%>%
  filter(admission_age>=18)

#标准化抗生素、培养、器官功能的时间
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(lubridate)
summary(patient_new)

###1.动态数据整理
maxICUday=28
#1.1 WBC
colnames(patient_new)
wbc<-complete_blood_count%>%
  select(subject_id,charttime,wbc)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(wbcdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(wbcdays=as.numeric(as.numeric(gsub(pattern = " hours",replacement = "",wbcdays))))%>%
  mutate(wbcdays=cut(wbcdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(wbcdays))%>%
  data.table()
wbc<-wbc[,.(wbc.min=min(wbc,na.rm=T),wbc.max=max(wbc,na.rm=T)),by=.(stay_id,wbcdays)]
wbc<-wbc[is.finite(wbc.max),]

#1.2 RDW
colnames(patient_new)
rdw<-complete_blood_count%>%
  select(subject_id,charttime,rdw)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(rdwdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(rdwdays=as.numeric(as.numeric(gsub(pattern = " hours",replacement = "",rdwdays))))%>%
  mutate(rdwdays=cut(rdwdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(rdwdays))%>%
  data.table()
rdw<-rdw[,.(rdw.min=min(rdw,na.rm=T),rdw.max=max(rdw,na.rm=T)),by=.(stay_id,rdwdays)]
rdw<-rdw[is.finite(rdw.max),]

#1.3 lym
colnames(blood_diff)
lym<-blood_diff%>%
  select(subject_id,charttime,lymphocytes_abs)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(lymdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(lymdays=as.numeric(as.numeric(gsub(pattern = " hours",replacement = "",lymdays))))%>%
  mutate(lymdays=cut(lymdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(lymdays))%>%
  data.table()
lym<-lym[,.(lym.min=min(lymphocytes_abs,na.rm=T),lym.max=max(lymphocytes_abs,na.rm=T)),by=.(stay_id,lymdays)]
lym<-lym[is.finite(lym.max),]

#1.4 NLR
colnames(blood_diff)
nlr<-blood_diff%>%
  mutate(nlr=round(as.numeric(blood_diff$neutrophils/blood_diff$lymphocytes),2))%>%
  select(subject_id,charttime,nlr)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(nlrdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(nlrdays=as.numeric(as.numeric(gsub(pattern = " hours",replacement = "",nlrdays))))%>%
  mutate(nlrdays=cut(nlrdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(nlrdays))%>%
  data.table()
nlr<-nlr[,.(nlr.min=min(nlr,na.rm=T),nlr.max=max(nlr,na.rm=T)),by=.(stay_id,nlrdays)]
nlr<-nlr[is.finite(nlr.max),]

#1.5 hct
colnames(complete_blood_count)
hct<-complete_blood_count%>%
  select(subject_id,charttime,hematocrit)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(hctdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(hctdays=as.numeric(as.numeric(gsub(pattern = " hours",replacement = "",hctdays))))%>%
  mutate(hctdays=cut(hctdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(hctdays))%>%
  data.table()
hct<-hct[,.(hct.min=min(hematocrit,na.rm=T),hct.max=max(hematocrit,na.rm=T)),by=.(stay_id,hctdays)]
hct<-hct[is.finite(hct.max),]

#1.6 temp
colnames(vital)
temp<-vital%>%
  select(subject_id,charttime,temperature)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(tempdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(tempdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",tempdays))))%>%
  mutate(tempdays=cut(tempdays,breaks = seq(0,1*24*maxICUday,1*24),
                      labels = seq(1,maxICUday)))%>%
  filter(!is.na(tempdays))%>%
  data.table()
temp<-temp[,.(temp.min=min(temperature,na.rm=T),temp.max=max(temperature,na.rm=T)),by=.(stay_id,tempdays)]
temp<-temp[is.finite(temp.max),]

#1.7 glu
colnames(chemistry)
glu<-chemistry%>%
  select(subject_id,charttime,glucose)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(gludays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(gludays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",gludays))))%>%
  mutate(gludays=cut(gludays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(gludays))%>%
  data.table()
glu<-glu[,.(glu.min=min(glucose,na.rm=T),glu.max=max(glucose,na.rm=T)),by=.(stay_id,gludays)]
glu<-glu[is.finite(glu.max),]


#1.8 chloride
cl<-chemistry%>%
  select(subject_id,charttime,chloride)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(cldays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(cldays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",cldays))))%>%
  mutate(cldays=cut(cldays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(cldays))%>%
  data.table()
cl<-cl[,.(cl.min=min(chloride,na.rm=T),cl.max=max(chloride,na.rm=T)),by=.(stay_id,cldays)]
cl<-cl[is.finite(cl.max),]

#1.9 sodium
sodium<-chemistry%>%
  select(subject_id,charttime,sodium)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(sodiumdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(sodiumdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",sodiumdays))))%>%
  mutate(sodiumdays=cut(sodiumdays,breaks = seq(0,1*24*maxICUday,1*24),
                        labels = seq(1,maxICUday)))%>%
  filter(!is.na(sodiumdays))%>%
  data.table()
sodium<-sodium[,.(sodium.min=min(sodium,na.rm=T),sodium.max=max(sodium,na.rm=T)),by=.(stay_id,sodiumdays)]
sodium<-sodium[is.finite(sodium.max),]

#1.10 hemoglobin
hemoglobin<-complete_blood_count%>%
  select(subject_id,charttime,hemoglobin)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(hbdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(hbdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",hbdays))))%>%
  mutate(hbdays=cut(hbdays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(hbdays))%>%
  data.table()
hemoglobin<-hemoglobin[,.(hemoglobin.min=min(hemoglobin,na.rm=T),hemoglobin.max=max(hemoglobin,na.rm=T)),by=.(stay_id,hbdays)]
hemoglobin<-hemoglobin[is.finite(hemoglobin.max),]


#1.11 alt
colnames(enzyme)
alt<-enzyme%>%
  select(subject_id,charttime,alt)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(altdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(altdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",altdays))))%>%
  mutate(altdays=cut(altdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(altdays))%>%
  data.table()
alt<-alt[,.(alt.min=min(alt,na.rm=T),alt.max=max(alt,na.rm=T)),by=.(stay_id,altdays)]
alt<-alt[is.finite(alt.max),]

#1.12 ast
ast<-enzyme%>%
  select(subject_id,charttime,ast)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(astdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(astdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",astdays))))%>%
  mutate(astdays=cut(astdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(astdays))%>%
  data.table()
ast<-ast[,.(ast.min=min(ast,na.rm=T),ast.max=max(ast,na.rm=T)),by=.(stay_id,astdays)]
ast<-ast[is.finite(ast.max),]


#1.13 albu
albu<-chemistry%>%
  select(subject_id,charttime,albumin)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(albudays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(albudays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",albudays))))%>%
  mutate(albudays=cut(albudays,breaks = seq(0,1*24*maxICUday,1*24),
                         labels = seq(1,maxICUday)))%>%
  filter(!is.na(albudays))%>%
  data.table()
albu<-albu[,.(albu.min=min(albumin,na.rm=T),albu.max=max(albumin,na.rm=T)),by=.(stay_id,albudays)]
albu<-albu[is.finite(albu.max),]


#1.14 bilirubin_total
bilirubin_total<-enzyme%>%
  select(subject_id,charttime,bilirubin_total)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(bilirubin_totaldays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(bilirubin_totaldays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",bilirubin_totaldays))))%>%
  mutate(bilirubin_totaldays=cut(bilirubin_totaldays,breaks = seq(0,1*24*maxICUday,1*24),
                                 labels = seq(1,maxICUday)))%>%
  filter(!is.na(bilirubin_totaldays))%>%
  data.table()
bilirubin_total<-bilirubin_total[,.(bilirubin_total.min=min(bilirubin_total,na.rm=T),bilirubin_total.max=max(bilirubin_total,na.rm=T)),by=.(stay_id,bilirubin_totaldays)]
bilirubin_total<-bilirubin_total[is.finite(bilirubin_total.max),]

#1.15 gcs_1
gcs_1<-gcs%>%
  select(subject_id,charttime,gcs)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(gcsdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(gcsdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",gcsdays))))%>%
  mutate(gcsdays=cut(gcsdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(gcsdays))%>%
  data.table()
gcs_1<-gcs_1[,.(gcs.min=min(gcs,na.rm=T),gcs.max=max(gcs,na.rm=T)),by=.(stay_id,gcsdays)]
gcs_1<-gcs_1[is.finite(gcs.max),]


#1.16 plt
plt<-complete_blood_count%>%
  select(subject_id,charttime,platelet)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(pltdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(pltdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",pltdays))))%>%
  mutate(pltdays=cut(pltdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(pltdays))%>%
  data.table()
plt<-plt[,.(platelet.min=min(platelet,na.rm=T),platelet.max=max(platelet,na.rm=T)),by=.(stay_id,pltdays)]
plt<-plt[is.finite(platelet.max),]

#1.17 inr
colnames(coagulation)
inr<-coagulation%>%
  select(subject_id,charttime,inr)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(inrdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(inrdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",inrdays))))%>%
  mutate(inrdays=cut(inrdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(inrdays))%>%
  data.table()
inr<-inr[,.(inr.min=min(inr,na.rm=T),inr.max=max(inr,na.rm=T)),by=.(stay_id,inrdays)]
inr<-inr[is.finite(inr.max),]

#1.18 aptt
colnames(coagulation)
aptt<-coagulation%>%
  select(subject_id,charttime,ptt)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(apttdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(apttdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",apttdays))))%>%
  mutate(apttdays=cut(apttdays,breaks = seq(0,1*24*maxICUday,1*24),
                      labels = seq(1,maxICUday)))%>%
  filter(!is.na(apttdays))%>%
  data.table()
aptt<-aptt[,.(aptt.min=min(ptt,na.rm=T),aptt.max=max(ptt,na.rm=T)),by=.(stay_id,apttdays)]
aptt<-aptt[is.finite(aptt.max),]

#1.19 creatinine
colnames(chemistry)
creatinine<-chemistry%>%
  select(subject_id,charttime,creatinine)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(crdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(crdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",crdays))))%>%
  mutate(crdays=cut(crdays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(crdays))%>%
  data.table()
creatinine<-creatinine[,.(creatinine.min=min(creatinine,na.rm=T),creatinine.max=max(creatinine,na.rm=T)),by=.(stay_id,crdays)]
creatinine<-creatinine[is.finite(creatinine.max),]

#1.20 bun
bun<-chemistry%>%
  select(subject_id,charttime,bun)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(bundays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(bundays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",bundays))))%>%
  mutate(bundays=cut(bundays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(bundays))%>%
  data.table()
bun<-bun[,.(bun.min=min(bun,na.rm=T),bun.max=max(bun,na.rm=T)),by=.(stay_id,bundays)]
bun<-bun[is.finite(bun.max),]


#1.21 heartrate
colnames(vital)
heartrate<-vital%>%
  select(subject_id,charttime,heart_rate)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(hrdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(hrdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",hrdays))))%>%
  mutate(hrdays=cut(hrdays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(hrdays))%>%
  data.table()
heartrate<-heartrate[,.(heartrate.min=min(heart_rate,na.rm=T),heartrate.max=max(heart_rate,na.rm=T)),by=.(stay_id,hrdays)]
heartrate<-heartrate[is.finite(heartrate.max),]

#1.22 sbp
sbp<-vital%>%
  select(subject_id,charttime,sbp)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(sbpdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(sbpdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",sbpdays))))%>%
  mutate(sbpdays=cut(sbpdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(sbpdays))%>%
  data.table()
sbp<-sbp[,.(sbp.min=min(sbp,na.rm=T),sbp.max=max(sbp,na.rm=T)),by=.(stay_id,sbpdays)]
sbp<-sbp[is.finite(sbp.max),]

#1.23 mbp
mbp<-vital%>%
  select(subject_id,charttime,mbp)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(mbpdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(mbpdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",mbpdays))))%>%
  mutate(mbpdays=cut(mbpdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(mbpdays))%>%
  data.table()
mbp<-mbp[,.(mbp.min=min(mbp,na.rm=T),mbp.max=max(mbp,na.rm=T)),by=.(stay_id,mbpdays)]
mbp<-mbp[is.finite(mbp.max),]

#1.24 dbp
dbp<-vital%>%
  select(subject_id,charttime,dbp)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(dbpdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(dbpdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",dbpdays))))%>%
  mutate(dbpdays=cut(dbpdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(dbpdays))%>%
  data.table()
dbp<-dbp[,.(dbp.min=min(dbp,na.rm=T),dbp.max=max(dbp,na.rm=T)),by=.(stay_id,dbpdays)]
dbp<-dbp[is.finite(dbp.max),]

#1.25 be
colnames(bg)
be<-bg%>%
  select(subject_id,charttime,baseexcess)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(bedays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(bedays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",bedays))))%>%
  mutate(bedays=cut(bedays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(bedays))%>%
  data.table()
be<-be[,.(be.min=min(baseexcess,na.rm=T),be.max=max(baseexcess,na.rm=T)),by=.(stay_id,bedays)]
be<-be[is.finite(be.max),]

#1.26 bicarbonate
bicarbonate<-chemistry%>%
  select(subject_id,charttime,bicarbonate)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(bicarbonatedays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(bicarbonatedays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",bicarbonatedays))))%>%
  mutate(bicarbonatedays=cut(bicarbonatedays,breaks = seq(0,1*24*maxICUday,1*24),
                             labels = seq(1,maxICUday)))%>%
  filter(!is.na(bicarbonatedays))%>%
  data.table()
bicarbonate<-bicarbonate[,.(bicarbonate.min=min(bicarbonate,na.rm=T),bicarbonate.max=max(bicarbonate,na.rm=T)),by=.(stay_id,bicarbonatedays)]
bicarbonate<-bicarbonate[is.finite(bicarbonate.max),]

#1.27 lactate
lactate<-bg%>%
  select(subject_id,charttime,lactate)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(lacdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(lacdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",lacdays))))%>%
  mutate(lacdays=cut(lacdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(lacdays))%>%
  data.table()
lactate<-lactate[,.(lactate.min=min(lactate,na.rm=T),lactate.max=max(lactate,na.rm=T)),by=.(stay_id,lacdays)]
lactate<-lactate[is.finite(lactate.max),]

#1.28 spo2
spo2<-vital%>%
  select(subject_id,charttime,spo2)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(spo2days=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(spo2days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",spo2days))))%>%
  mutate(spo2days=cut(spo2days,breaks = seq(0,1*24*maxICUday,1*24),
                      labels = seq(1,maxICUday)))%>%
  filter(!is.na(spo2days))%>%
  data.table()
spo2<-spo2[,.(spo2.min=min(spo2,na.rm=T),spo2.max=max(spo2,na.rm=T)),by=.(stay_id,spo2days)]
spo2<-spo2[is.finite(spo2.max),]

#1.29 pao2
pao2<-bg%>%
  select(subject_id,charttime,po2)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(pao2days=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(pao2days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",pao2days))))%>%
  mutate(pao2days=cut(pao2days,breaks = seq(0,1*24*maxICUday,1*24),
                      labels = seq(1,maxICUday)))%>%
  filter(!is.na(pao2days))%>%
  data.table()
pao2<-pao2[,.(pao2.min=min(po2,na.rm=T),pao2.max=max(po2,na.rm=T)),by=.(stay_id,pao2days)]
pao2<-pao2[is.finite(pao2.max),]

#1.30 paco2
colnames(bg)
paco2<-bg%>%
  select(subject_id,charttime,pco2)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(paco2days=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(paco2days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",paco2days))))%>%
  mutate(paco2days=cut(paco2days,breaks = seq(0,1*24*maxICUday,1*24),
                       labels = seq(1,maxICUday)))%>%
  filter(!is.na(paco2days))%>%
  data.table()
paco2<-paco2[,.(paco2.min=min(pco2,na.rm=T),paco2.max=max(pco2,na.rm=T)),by=.(stay_id,paco2days)]
paco2<-paco2[is.finite(paco2.max),]

#1.31 pf
pf<-bg%>%
  select(subject_id,charttime,pao2fio2ratio)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(pfdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(pfdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",pfdays))))%>%
  mutate(pfdays=cut(pfdays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(pfdays))%>%
  data.table()
pf<-pf[,.(pf.min=min(pao2fio2ratio,na.rm=T),pf.max=max(pao2fio2ratio,na.rm=T)),by=.(stay_id,pfdays)]
pf<-pf[is.finite(pf.max),]

#1.32 ph
ph<-bg%>%
  select(subject_id,charttime,ph)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(phdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(phdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",phdays))))%>%
  mutate(phdays=cut(phdays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(phdays))%>%
  data.table()
ph<-ph[,.(ph.min=min(ph,na.rm=T),ph.max=max(ph,na.rm=T)),by=.(stay_id,phdays)]
ph<-ph[is.finite(ph.max),]

#1.33 resp_rate
resp_rate<-vital%>%
  select(subject_id,charttime,resp_rate)%>%
  left_join(patient_new[,c(1,2,15,26)],by="subject_id")%>%
  mutate(rrdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(rrdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",rrdays))))%>%
  mutate(rrdays=cut(rrdays,breaks = seq(0,1*24*maxICUday,1*24),
                    labels = seq(1,maxICUday)))%>%
  filter(!is.na(rrdays))%>%
  data.table()
resp_rate<-resp_rate[,.(resp_rate.min=min(resp_rate,na.rm=T),resp_rate.max=max(resp_rate,na.rm=T)),by=.(stay_id,rrdays)]
resp_rate<-resp_rate[is.finite(resp_rate.max),]

#1.34 urine
uri<-urine%>%
  select(stay_id,charttime,urineoutput)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(uridays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(uridays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",uridays))))%>%
  mutate(uridays=cut(uridays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(uridays))%>%
  filter(urineoutput>=0)%>%
  data.table()
uri<-uri[,.(uri_total=sum(urineoutput,na.rm=T)),by=.(stay_id,uridays)]
uri<-uri[is.finite(uri_total),]

#1.35 input1 错的
input1<-inputs%>%
  select(stay_id,storetime,amount)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(inputdays=difftime(storetime,icu_intime,units="hours"))%>%
  mutate(inputdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",inputdays))))%>%
  mutate(inputdays=cut(inputdays,breaks = seq(0,1*24*maxICUday,1*24),
                       labels = seq(1,maxICUday)))%>%
  filter(!is.na(inputdays))%>%
  filter(amount>=0)%>%
  data.table()
input1<-input1[,.(input_total=sum(amount,na.rm=T)),by=.(stay_id,inputdays)]
input1<-input1[is.finite(input_total),]
####new input
colnames(inputs)
sepsis_inputs<-inputs%>%
  select(stay_id,starttime,endtime,amount,amountuom,rate,rateuom,ordercategoryname,ordercategorydescription)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(start=difftime(starttime,icu_intime,units="hours"))%>%
  mutate(start=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",start))))%>%
  mutate(end=difftime(endtime,icu_intime,units="hours"))%>%
  mutate(end=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",end))))%>%
  mutate(inputdays=difftime(starttime,icu_intime,units="hours"))%>%
  mutate(inputdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",inputdays))))%>%
  mutate(inputdays=cut(inputdays,breaks = seq(0,1*24*maxICUday,1*24),
                       labels = seq(1,maxICUday)))%>%
  filter(!is.na(inputdays))%>%
  as.data.table()

sepsis_inputs_2<-sqldf("select * from sepsis_inputs
                       where rateuom in ('mL/min','mL/hour')")

sepsis_inputs_3<-sepsis_inputs_2%>%
  mutate(amount=ifelse(amountuom=="L",amount*1000,amount))%>%
  mutate(rate=ifelse(rateuom=="mL/min",rate*60,rate))%>%
  mutate(amount=ifelse(ordercategorydescription=="Continuous Med",rate*(end-start),
                       ifelse(ordercategorydescription=="Continuous IV",rate*(end-start),amount)))%>%
  as.data.table()

sepsis_inputs_3<-sepsis_inputs_3[,.(input_total=sum(amount,na.rm=T)),by=.(stay_id,inputdays)]
sepsis_inputs_3<-sepsis_inputs_3[is.finite(input_total),]
######
library(lubridate)




######
#1.36 output
output_1<-outputs%>%
  select(stay_id,storetime,value)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(outputdays=difftime(storetime,icu_intime,units="hours"))%>%
  mutate(outputdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",outputdays))))%>%
  mutate(outputdays=cut(outputdays,breaks = seq(0,1*24*maxICUday,1*24),
                        labels = seq(1,maxICUday)))%>%
  filter(!is.na(outputdays))%>%
  filter(value>=0)%>%
  data.table()
output_1<-output_1[,.(output_total=sum(value,na.rm=T)),by=.(stay_id,outputdays)]
output_1<-output_1[is.finite(output_total),]

#1.37 血管活性药物（去甲肾上腺素等效剂量（ug/kg/min））max
vasopressor_ne<-norepinephrine_equivalent_dose%>%
  select(stay_id,endtime,norepinephrine_equivalent_dose)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(vasodays=difftime(endtime,icu_intime,units="hours"))%>%
  mutate(vasodays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",vasodays))))%>%
  mutate(vasodays=cut(vasodays,breaks = seq(0,1*24*maxICUday,1*24),
                      labels = seq(1,maxICUday)))%>%
  filter(!is.na(vasodays))%>%
  data.table()
vasopressor_ne<-vasopressor_ne[,.(vaso_ne=max(norepinephrine_equivalent_dose,na.rm=T)),by=.(stay_id,vasodays)]
vasopressor_ne<-vasopressor_ne[is.finite(vaso_ne),]

##1.38 CRRT
rrt$dialysis_type<-as.factor(rrt$dialysis_type)
summary(rrt$dialysis_type)
sepsis_rrt<-rrt%>%
  filter(dialysis_type%in%(c('CVVH','CVVHD','CVVHDF','SCUF','IHD','CRRT')))%>%
  select(stay_id,charttime,dialysis_present,dialysis_active)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(rrtdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(rrtdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",rrtdays))))%>%
  mutate(rrtdays=cut(rrtdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(rrtdays))%>%
  data.table()
sepsis_rrt<-sepsis_rrt[,.(rrt=max(dialysis_present,na.rm=T)),by=.(stay_id,rrtdays)]
sepsis_rrt<-sepsis_rrt[is.finite(rrt),]
#####vent

ventilation$ventilation_status<-as.factor(ventilation$ventilation_status)
summary(ventilation$ventilation_status)
summary(ventilator_setting)
ventilator_setting$vent<-1
colnames(ventilator_setting)
sepsis_vent<-ventilator_setting%>%
  select(stay_id,charttime,vent)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(ventdays=difftime(charttime,icu_intime,units="hours"))%>%
  mutate(ventdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",ventdays))))%>%
  mutate(ventdays=cut(ventdays,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(ventdays))%>%
  data.table()
sepsis_vent<-sepsis_vent[,.(vent=max(vent,na.rm=T)),by=.(stay_id,ventdays)]
sepsis_vent<-sepsis_vent[is.finite(vent),]


####1.39 sofa
colnames(patient_new)
sepsis_sofa<-sofa%>%
  select(stay_id,hr,respiration_24hours,coagulation_24hours,liver_24hours,
         cardiovascular_24hours,cns_24hours,renal_24hours,sofa_24hours)%>%
  left_join(patient_new[,c(1,2,15,26)],by="stay_id")%>%
  mutate(hr_sofa=cut(hr,breaks = seq(0,1*24*maxICUday,1*24),
                     labels = seq(1,maxICUday)))%>%
  filter(!is.na(hr_sofa))%>%
  data.table()
sepsis_sofa<-sepsis_sofa[,.(sofa_day=max(sofa_24hours,na.rm=T),respiration_24hours=max(respiration_24hours,na.rm=T),
                            coagulation_24hours=max(coagulation_24hours,na.rm=T),
                            liver_24hours=max(liver_24hours,na.rm=T),
                            cardiovascular_24hours=max(cardiovascular_24hours,na.rm=T),
                            cns_24hours=max(cns_24hours,na.rm=T),
                            renal_24hours=max(renal_24hours,na.rm=T)),by=.(stay_id,hr_sofa)]
sepsis_sofa<-sepsis_sofa[is.finite(sofa_day),]



#####2数据合并
######2.1将ICU天数进行归一化
patient_new<-as.data.table(patient_new)
colnames(patient_new)
days<-patient_new[,.(stay_id,los_icu)]
days[,days:=ceiling(los_icu)]
days<-days[,.(days=1:days),by=c("stay_id")]
days<-days[days<=maxICUday&days>=0]
days[,days:=as.factor(days)]
















vars <- c("gender","admission_age","weight_admit","height","ethni","admissiontype",
            "myocardial_infarct","congestive_heart_failure","peripheral_vascular_disease",
            "cerebrovascular_disease","dementia","chronic_pulmonary_disease","rheumatic_disease",
            "peptic_ulcer_disease","mild_liver_disease","diabetes_without_cc","diabetes_with_cc","paraplegia",
            "renal_disease","malignant_cancer","severe_liver_disease",
            "metastatic_solid_tumor","aids","charlson_comorbidity_index",
            "apsiii", "oasis","sirs","sofa_score","respiration","coagulation","liver","cardiovascular",
            "cns","renal","wbc.max","rdw.min","nlr.max","hct.min","temp.max","glu.max",
            "cl.max","sodium.max","hemoglobin.min","alt.max","ast.max","albu.min","bilirubin_total.max","gcs.min",
            "platelet.min","inr.max","aptt.max","creatinine.max","bun.max","heartrate.max","sbp.min",
            "mbp.min","dbp.min","be.min","bicarbonate.min","lactate.max","spo2.min","pao2.min",
            "paco2.max","pf.min","ph.min","resp_rate.max","uri_total","fb",
            "vaso_ne","vaso_use","rrt","vent","septicshock","positive_culture",
            "time","event","hosp_mor","los_hospital","los_icu","albumin_tr")


rawsum<-miss_var_summary()



######感染部位数据
colnames(site)
infection_site<-site%>%
  mutate(infectionsite=ifelse(lung_infection==1,1,
                              ifelse(Peritoneal_infection==1,2,
                                     ifelse(Gastrointestinal_infection==1,2,
                                            ifelse(Genitourinary_infection==1,3,4)))))%>%
  mutate(infectionsite=factor(infectionsite,levels = c(1,2,3,4)))
summary(infection_site$infectionsite)
infection_site$infectionsite[which(is.na(infection_site$infectionsite))]<-4


vars <- c("gender","admission_age","weight_admit","height","ethni","admissiontype","infectionsite",
          "myocardial_infarct","congestive_heart_failure","peripheral_vascular_disease",
          "cerebrovascular_disease","dementia","chronic_pulmonary_disease","rheumatic_disease",
          "peptic_ulcer_disease","mild_liver_disease","diabetes_without_cc","diabetes_with_cc","paraplegia",
          "renal_disease","malignant_cancer","severe_liver_disease",
          "metastatic_solid_tumor","aids","charlson_comorbidity_index",
          "apsiii", "oasis","sirs","sofa_score","respiration","coagulation","liver","cardiovascular",
          "cns","renal","wbc.max","rdw.min","nlr.max","hct.min","temp.max","glu.max",
          "cl.max","sodium.max","hemoglobin.min","alt.max","ast.max","albu.min","bilirubin_total.max","gcs.min",
          "platelet.min","inr.max","aptt.max","creatinine.max","bun.max","heartrate.max","sbp.min",
          "mbp.min","dbp.min","be.min","bicarbonate.min","lactate.max","spo2.min","pao2.min",
          "paco2.max","pf.min","ph.min","resp_rate.max","uri_total","fb",
          "vaso_ne","vaso_use","rrt","vent","septicshock","positive_culture",
          "time","event","hosp_mor","los_hospital","los_icu","albumin_tr")



#######meld initial 
######MELD Score without sodium change
####meld score
#####MELD Score (2016) = MELD*10 + 1.32*(137-Na) – [0.033*MELD*10*(137-Na)]





