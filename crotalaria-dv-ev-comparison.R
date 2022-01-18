cro_dv <- read.csv("crotalaria-dv-2019-evdvonly.csv",header=TRUE)
library(dplyr)
library(ggpubr)
ggdensity(cro_dv$Original.LN.Survival, 
          main = "Density plot of LN survival",
          xlab = "Original LN Survival")
ggqqplot(cro_dv$Original.LN.Survival)
shapiro.test(cro_dv$Original.LN.Survival)
attach(cro_dv)
m1<-glm(orig_LN~Cryo.Procedure,family=binomial(link="logit"))
summary(m1)
m2<-glm(orig_LN~Cryo.Procedure+Genotype,family=binomial(link="logit"))
summary(m2)
m3<-glm(orig_LN~Cryo.Procedure+Cryo.Procedure*Genotype,family=binomial(link="logit"))
summary(m3)
m4<-glmer(orig_LN~Cryo.Procedure+(1|Genotype),family=binomial(link="logit"))
summary(m4)
print(m4,corr=FALSE)
wilcox.test(orig_LN~Cryo.Procedure,data=cro_dv)
group_by(cro_dv, Cryo.Procedure) %>%
  summarise(
    count = n(),
    median = median(orig_LN, na.rm = TRUE),
    IQR = IQR(orig_LN, na.rm = TRUE)
  )
p7<-ggboxplot(cro, x = "Cryo.Procedure", y = "orig_LN", 
          color = "Cryo.Procedure", palette = c("#4DBBD5FF", "#E64B35FF"),
          ylab = "Original LN Survival", xlab = "Cryo Procedure")
p7 + theme(legend.position = "none")

ggplot(cro_vial, aes(x=Years.bin, y=FourW.alive)) + 
  geom_bar(stat="identity", width=.5, fill="#4DBBD5FF") + 
  labs(title="Years banked vs. Survival",
       x="Years in LN storage",
       y="Percent of tips alive at 4 weeks") + 
  theme_bw()

ggplot(cro_vial, aes(x=Init.Surv, y=FourW.alive)) + 
  geom_bar(stat="identity", fill="#E64B35FF") +
  labs(title="Initial survival vs. long-term survival",
       x="Initial survival (percent)",
       y="Percent of tips alive at 4 weeks") + 
  theme_bw()

ggplot(cro_vial, aes(x=PVS2.Control, y=FourW.alive)) + 
  geom_bar(stat="identity", fill="#E64B35FF") +
  ylim(c(0, 50)) +
  labs(title="PVS2 survival vs. long-term survival",
       x="PVS2 survival (percent)",
       y="Percent of tips alive at 4 weeks") + 
  theme_bw()

ggplot(cro_vial, aes(x=TwoW.Contam, y=FourW.alive)) + 
  geom_bar(stat="identity", fill="#4DBBD5FF") +
  labs(title="Two-week contamination vs. long-term survival",
       x="Percent of tips contaminated at 2 weeks",
       y="Percent of tips alive at 4 weeks") + 
  theme_bw()

ggplot(cro_vial, aes(Years, FourW.alive, color=Proc)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=Proc)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Years banked") + ylab("Pr (survived)") + 
  theme_bw()

ggplot(cro_vial, aes(Init.Surv, FourW.alive, color=Proc)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=Proc)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Initial survival") + ylab("Pr (survived)") + 
  theme_bw()

ggplot(cro_vial, aes(PVS2.Control, FourW.alive, color=Proc)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=Proc)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("PVS2 survival") + ylab("Pr (survived)") + 
  theme_bw()

ggplot(cro_vial, aes(TwoW.Contam, FourW.alive, color=Proc)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=Proc)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Percent of tips with contamination at 2 weeks") + ylab("Pr (survived)") + 
  theme_bw()


ggplot(cro_vial, aes(Age.Prec, FourW.alive, color=Proc)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=Proc)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Tissue age") + ylab("Pr (survived)") + 
  theme_bw()

IS_agg <- aggregate(Init.Surv ~ Proc, cro_vial, mean)
DV_agg <- aggregate(Original.LN.Survival ~ Cryo.Procedure, cro_dv, mean)
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
SE <- aggregate(Init.Surv ~ Proc, cro_vial, st.err)
IS_agg <- cbind(IS_agg, SE[,2])
FS_agg <- aggregate(FourW.Surv ~ Proc, cro_vial, mean)
SE2 <- aggregate(FourW.Surv ~ Proc, cro_vial, st.err)
FS_agg <- cbind(FS_agg, SE2[,2])
all_avg <- cbind(IS_agg, FS_agg[,2])
all_avg <- cbind(all_avg, SE2[,2])

ggplot(all_avg, aes(x=, y=FourW.alive)) + 
  geom_bar(stat="identity", width=.5, fill="#4DBBD5FF") + 
  labs(title="Years banked vs. Survival",
       x="Years in LN storage",
       y="Percent of tips alive at 4 weeks") + 
  theme_bw()