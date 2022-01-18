setwd("C:/Users/Megan/Desktop/Crotalaria/USE THIS DATA")
cro<-read.csv("crotalaria-dv-2019.csv",header=TRUE)
attach(cro)

hist(orig_LN[Cryo.Procedure=="EV"])
hist(orig_LN[Cryo.Procedure=="EVS"])
hist(orig_LN[Cryo.Procedure=="DV"])

library(dplyr)
group_by(cro_all, Proc) %>%
  summarise(
    count = n(),
    mean = mean(FourW.Surv, na.rm = TRUE),
    sd = sd(FourW.Surv, na.rm = TRUE),
    median = median(FourW.Surv, na.rm = TRUE),
    IQR = IQR(FourW.Surv, na.rm = TRUE)
  )

group_by(cro_all, Proc) %>%
  summarise(
    count = n(),
    mean = mean(Age.Prec, na.rm = TRUE),
    min = min(Age.Prec, na.rm = TRUE),
    max = max(Age.Prec, na.rm = TRUE)
  )

group_by(cro, Cryo.Procedure) %>%
  summarise(
    count = n(),
    mean = mean(orig_LN, na.rm = TRUE),
    SE = sd(orig_LN, na.rm=TRUE) /  
      sqrt(length(orig_LN[!is.na(orig_LN)])) ,
    min = min(orig_LN, na.rm = TRUE),
    max = max(orig_LN, na.rm = TRUE)
  )

genotable <- group_by(cro_all, Genotype) %>%
  summarise(
    count = n(),
    mean = mean(FourW.Surv, na.rm = TRUE),
    sd = sd(FourW.Surv, na.rm = TRUE),
    median = median(FourW.Surv, na.rm = TRUE),
    IQR = IQR(FourW.Surv, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(cro, x = "Cryo.Procedure", y = "orig_LN", 
          color = "Cryo.Procedure", palette = c("#4DBBD5FF", "#E64B35FF"),
          ylab = "Initial Survival", xlab = "Cryo Procedure")

wilcox.test(orig_LN ~ Cryo.Procedure, data=cro, alternative = "two.sided")

p1<-ggboxplot(cro_all, x = "Proc", y = "FourW.Surv", 
          color = "Proc", palette = c("#E64B35FF", "#00A087FF","#3C5488FF"),
          ylab = "Long-term Survival", xlab = "Cryo Procedure")
p1 + theme(legend.position = "none")

p2<-ggboxplot(cro_all, x = "Proc", y = "Years", 
          color = "Proc", palette = c("#E64B35FF", "#00A087FF","#3C5488FF"),
          ylab = "Time Banked (years)", xlab = "Cryo Procedure")
p2 + theme(legend.position = "none")

PVS2<-(cro_all$PVS2.Control/100)
p3<-ggboxplot(cro_all, x = "Proc", y = "PVS2", 
          color = "Proc", palette = c("#E64B35FF", "#00A087FF","#3C5488FF"),
          ylab = "PVS2 Control Survival", xlab = "Cryo Procedure")
p3 + theme(legend.position = "none")

p4<-ggboxplot(cro_all, x = "Proc", y = "Init.Surv", 
          color = "Proc", palette = c("#E64B35FF", "#00A087FF","#3C5488FF"),
          ylab = "Initial LN Survival", xlab = "Cryo Procedure")
p4 + theme(legend.position = "none")

Contam <- (cro_all$TwoW.Contam/100)
p5<-ggboxplot(cro_all, x = "Proc", y = "Contam", 
          color = "Proc", palette = c("#E64B35FF", "#00A087FF","#3C5488FF"),
          ylab = "Two-week Contamination", xlab = "Cryo Procedure")
p5 + theme(legend.position = "none")

p6<-ggboxplot(cro_all, x = "Proc", y = "Age.Prec", 
          color = "Proc", palette = c("#E64B35FF", "#00A087FF","#3C5488FF"),
          ylab = "Average Age of Tissue before Banking (days)", xlab = "Cryo Procedure")
p6 + theme(legend.position = "none")

