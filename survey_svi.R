library(dplyer)
library(tibble)

setwd("~/Documents/GitHub/ChathamData")
surveySVI <- read_csv("surveySVI.csv", na = "")
surveydf <- as_data_frame(surveySVI)

surv <- select(surveydf, Evacuate,KnowEstab,SLRPers,SSDPers,ARelocate,Aprotect,ANoRecover,PCAbleMov,PCNoRecov,Age_Cat,Hincome,Education,Ethnicity,HomeLang,FldRisk,Gender,svi_outp_3)



fit1 <- lm(svi_outp_3 ~ as.factor(PCNoRecov), data=surv)
fit2 <- lm(svi_outp_3 ~ as.factor(ANoRecover),data=surv)
fit3 <- lm(svi_outp_3 ~ as.factor(PCAbleMov),data=surv)
fit4 <- lm(svi_outp_3 ~ as.factor(Aprotect),data=surv)
fit5 <- lm(svi_outp_3 ~ as.factor(ARelocate),data=surv)
fit6 <- lm(svi_outp_3 ~ Hincome,data=surv)
fit7 <- lm(svi_outp_3 ~ Education,data=surv)
fit8 <- lm(svi_outp_3 ~ as.factor(Ethnicity),data=surv)
          
summary(fit3)

summary(fit2)
