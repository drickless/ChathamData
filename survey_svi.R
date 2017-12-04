
library(dplyer)
library(tibble)

#test for relationship between SVI score and selected survey vars
setwd("~/Documents/GitHub/ChathamData")
surveySVI <- read_csv("surveySVI.csv", na = "")
surveydf <- as_data_frame(surveySVI)

surv <- select(surveydf, Evacuate,KnowEstab,SLRPers,SSDPers,ARelocate,Aprotect,ANoRecover,PCAbleMov,PCNoRecov,Age_Cat,Hincome,Education,Ethnicity,HomeLang,FldRisk,Gender,SVI)

fit1 <- lm(SVI ~ as.factor(PCNoRecov), data=surv)
fit2 <- lm(SVI ~ as.factor(ANoRecover),data=surv)
fit3 <- lm(SVI ~ as.factor(PCAbleMov),data=surv)
fit4 <- lm(SVI ~ as.factor(Aprotect),data=surv)
fit5 <- lm(SVI ~ as.factor(ARelocate),data=surv)
fit6 <- lm(SVI ~ as.factor(Hincome),data=surv)
fit7 <- lm(SVI ~ as.factor(Education),data=surv)
fit8 <- lm(SVI ~ as.factor(Ethnicity),data=surv)
fit9 <- lm(SVI ~ ANoRecover, data=surv)

summary(fit1)

fit1 <- lm(SVI ~ PCNoRecov, data=surv)
fit2 <- lm(SVI ~ ANoRecover,data=surv)
fit3 <- lm(SVI ~ PCAbleMov,data=surv)
fit4 <- lm(SVI ~ Aprotect,data=surv)
fit5 <- lm(SVI ~ ARelocate,data=surv)
fit6 <- lm(SVI ~ Hincome,data=surv)
fit7 <- lm(SVI ~ as.factor(Education),data=surv)
fit8 <- lm(SVI ~ as.factor(Ethnicity),data=surv)

#visualize

library(ggplot2)
library(reshape2)

sp <- ggplot(surveydf, aes(x=ANoRecover,y=SVI))+
  geom_smooth(method = 'lm') + geom_jitter()
sp + facet_grid(.~Ethnicity)

sp_prot <- ggplot(surveydf, aes(x=Aprotect,y=SVI))+
  geom_smooth(method = 'lm') + geom_jitter()
sp_prot + facet_grid(.~Ethnicity)

sp_reloc <- ggplot(surveydf, aes(x=ARelocate,y=SVI))+
  geom_smooth(method = 'lm') + geom_jitter()
sp_reloc + facet_grid(.~Ethnicity)

sp_inc <- ggplot(surveydf, aes(x=Hincome,y=SVI))+
  geom_smooth(method = 'lm') + geom_jitter()
sp_inc + facet_grid(.~Ethnicity)

sp_kn <- ggplot(surveydf, aes(x=ARelocate,y=SVI))+
  geom_smooth(method = 'lm') + geom_jitter()
sp_kn + facet_grid(.~KnowEstab)

