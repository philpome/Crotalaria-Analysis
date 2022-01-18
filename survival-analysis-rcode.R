library(asaur)
library(survival)
crotalaria<-read.csv("C:/Users/Megan/Desktop/Crotalaria/crotalaria-logreg-data.csv",header=TRUE)
attach(crotalaria)
#create a survival data structure
Surv(time,delta)
plot(time~wksbanked)
result<-coxph(Surv(time,delta)~wksbanked,data=crotalaria)
result
library(forestplot)
#obtain estimates of the Kaplan-Meier estimator
result.km<-survfit(Surv(time,delta)~1,conf.type="log-log")
result.km
summary(result.km)
plot(result.km, xlab="Time (weeks)",ylab="Survival probability")
#try the Nelson-Altschuler estimate:
result.fh<-survfit(Surv(time,delta)~1,conf.type="log-log",type="fh")
summary(result.fh)
plot(result.fh, xlab="Time (weeks)",ylab="Survival probability")
#estimate and plot nonparametric hazard functions
library(muhaz)
result.simple<-muhaz(time,delta,max.time=12,bw.grid=2.25,bw.method="global",b.cor="none")
plot(result.simple)
#illustrate the estimation of the hazard function
results.pe5<-pehaz(time,delta,width=2,max.time=12)
plot(results.pe5)
#smooth it
result.smooth<-muhaz(time,delta,bw.smooth=20,b.cor="left",max.time=12)
lines(result.smooth)
#extract the hazard estimate
haz<-result.smooth$haz.est
times<-result.smooth$est.grid
surv<-exp(-cumsum(haz[1:(length(haz)-1)]*diff(times)))
#compare smoothed survival estimate to the Kaplan-Meier estimate
result.km<-survfit(Surv(time,delta)~1,conf.type="none")
plot(result.km,conf.int=T,mark="|",xlab="Time (weeks)",xlim=c(0,15),ylab="Survival probability")
lines(surv~times[1:(length(times)-1)])
#Cox proportional hazards model of the effects of weeks banked
coxph(Surv(time,delta)~wksbanked)
#now stratify by genotype
coxph(Surv(time,delta)~wksbanked+strata(Genotype))
#explicitly estimate the genetic effect
coxph(Surv(time,delta)~wksbanked+Genotype)
#if coef is negative, the treatment (wksbanked) is effective (?)
#if coef is positive, the treatment is more associated with death
library(survminer)
fit<-survfit(Surv(time,delta)~Genotype)
ggsurvplot(fit,risk.table=TRUE,pval=TRUE,conf.int=TRUE,xlim=c(0,15),break.time.by=2,ggtheme=theme_dark(),risk.table.y.text.col=T,risk.table.y.text=FALSE)
nr.of.survivals<-aggregate(Location~surv.binary,FUN=length)
zoib.model<-zoib(avg.surv~wksbanked+Proc+Operator)
model1<-zoib(avg.surv ~ wksbanked + Proc + Operator |1|1,
            data = cryodata, random = 1, EUID = cryodata$Genotype,
            zero.inflation = TRUE, one.inflation = FALSE, joint = FALSE,
            n.iter = 5, n.thin = 2, n.burn = 10)
