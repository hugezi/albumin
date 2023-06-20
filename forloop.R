long_dose_1<-long_dose%>%
  filter(lactate.max<=2.1)
temp1sub <- ipwtm(
  exposure = albutr,
  family = "survival",
  numerator = ~scale(admission_age,scale = 10)+gender+admissiontype
  +ethni+infectionsite+first_albu,
  denominator = ~scale(admission_age,scale = 10)+gender+admissiontype
  +sofa_day+ethni+infectionsite+first_albu
  +lactate.max+scale(pf.min,scale = 50)+vaso_ne+scale(uri_total,scale=1000)+mbp.min,
  id = stay_id,
  tstart = tstart,
  timevar = tstop,
  type = "first",
  data = long_dose_1)
temp2sub <- ipwtm(
  exposure = dropout,
  family = "survival",
  numerator = ~scale(admission_age,scale = 10)+gender+admissiontype
  +ethni+infectionsite+first_albu,
  denominator = ~scale(admission_age,scale = 10)+gender+admissiontype
  +sofa_day+ethni+infectionsite+first_albu
  +lactate.max+scale(pf.min,scale = 50)+vaso_ne+scale(uri_total,scale=1000)+mbp.min,
  id = stay_id,
  tstart = tstart,
  timevar = tstop,
  type = "first",
  data = long_dose_1)
model_x<-coxph(Surv(tstart,tstop,event)~albutr+rdw.min+bilirubin_total.max+gcs.min+
                 scale(platelet.min,scale = 20)+scale(aptt.max,scale = 10)+mbp.min+be.min+bicarbonate.min+lactate.max+
                 scale(pao2.min,scale = 10)+scale(fb,scale = 1000)+respiration_24hours+gender+admission_age+cluster(stay_id),
               data =long_dose_1, weights = temp1sub$ipw.weights*temp2sub$ipw.weights)
summary(model_x)



##############MBP

N<-c(40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80)
cutoff_effect=list()
library(dplyr)

library(ipw)
library(survival)
library(survminer)
for (i in 1:length(N)) {
  #i = 1
  long_dose_1<-long_dose%>%
    filter(mbp.min<=N[[i]])
  temp1sub <- ipwtm(
    exposure = albutr,
    family = "survival",
    numerator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +ethni+infectionsite+first_albu,
    denominator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +sofa_day+ethni+infectionsite+first_albu
    +lactate.max+scale(pf.min,scale = 50)+vaso_ne+scale(uri_total,scale=1000)+mbp.min,
    id = stay_id,
    tstart = tstart,
    timevar = tstop,
    type = "first",
    data = long_dose_1)
  temp2sub <- ipwtm(
    exposure = dropout,
    family = "survival",
    numerator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +ethni+infectionsite+first_albu,
    denominator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +sofa_day+ethni+infectionsite+first_albu
    +lactate.max+scale(pf.min,scale = 50)+vaso_ne+scale(uri_total,scale=1000)+mbp.min,
    id = stay_id,
    tstart = tstart,
    timevar = tstop,
    type = "first",
    data = long_dose_1)
  model_x<-coxph(Surv(tstart,tstop,event)~albutr+rdw.min+bilirubin_total.max+gcs.min+
                   scale(platelet.min,scale = 20)+scale(aptt.max,scale = 10)+mbp.min+be.min+bicarbonate.min+lactate.max+
                   scale(pao2.min,scale = 10)+scale(fb,scale = 1000)+respiration_24hours+gender+admission_age+cluster(stay_id),
                 data =long_dose_1, weights = temp1sub$ipw.weights*temp2sub$ipw.weights)
  model_x_1<-summary(model_x)
  total_ci=cbind(N[[i]],model_x_1$conf.int)
  cutoff_effect[[i]]=total_ci
}
cutoff_effect = do.call(rbind,cutoff_effect)
cutoff_effect<-as.data.frame(cutoff_effect)
cutoff_effect
write.csv(cutoff_effect, "~/Desktop/cutoff_effect_mbp.csv", row.names=FALSE)
write.csv(long_dose, "~/Desktop/long_dose.csv", row.names=FALSE)



##############lac
N<-c(2.0,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.1,3.2,
     3.3,3.4,3.5,3.6,3.7,3.8,3.9,4.0,4.1,4.2)

cutoff_effect=list()
library(dplyr)
library(ipw)
library(survival)
library(survminer)
for (i in 1:length(N)) {
  #i = 1
  new_long_1<-new_long%>%
    filter(albu.min<=N[[i]])
  temp1sub <- ipwtm(
    exposure = albutr,
    family = "survival",
    numerator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +ethni+infectionsite+first_albu,
    denominator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +sofa_day+ethni+infectionsite+first_albu
    +lactate.max+scale(pf.min,scale = 50)+vaso_ne+scale(uri_total,scale=1000)+mbp.min,
    id = stay_id,
    tstart = tstart,
    timevar = tstop,
    type = "first",
    data = new_long_1)
  temp2sub <- ipwtm(
    exposure = dropout,
    family = "survival",
    numerator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +ethni+infectionsite+first_albu,
    denominator = ~scale(admission_age,scale = 10)+gender+admissiontype
    +sofa_day+ethni+infectionsite+first_albu
    +lactate.max+scale(pf.min,scale = 50)+vaso_ne+scale(uri_total,scale=1000)+mbp.min,
    id = stay_id,
    tstart = tstart,
    timevar = tstop,
    type = "first",
    data = new_long_1)
  model_x<-coxph(Surv(tstart,tstop,event)~albutr+rdw.min+bilirubin_total.max+gcs.min+
                   scale(platelet.min,scale = 20)+scale(aptt.max,scale = 10)+mbp.min+be.min+bicarbonate.min+lactate.max+
                   scale(pao2.min,scale = 10)+scale(fb,scale = 1000)+respiration_24hours+gender+admission_age+cluster(stay_id),
                 data =new_long_1, weights = temp1sub$ipw.weights*temp2sub$ipw.weights)
  model_x_1<-summary(model_x)
  total_ci=cbind(N[[i]],model_x_1$conf.int)
  cutoff_effect[[i]]=total_ci
}
cutoff_effect = do.call(rbind,cutoff_effect)
cutoff_effect<-as.data.frame(cutoff_effect)
cutoff_effect
