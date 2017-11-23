library(dplyer)
library(tibble)

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
