library(tidyverse)

setwd("~/Documents/GitHub/ChathamData")

#----construct a weighted social vulnerability index from census data----
census <- read_csv("censusClean.csv", na="")
census <- as_data_frame(census)
#replace missing values with median
sapply(census,function(x) sum(is.na(x)))
census$PerCapInc[is.na(census$PerCapInc)] <- median(census$PerCapInc,na.rm = T)
census$MedValue[is.na(census$MedValue)] <- median(census$MedValue,na.rm = T)

#reverse direction of vars so higher values for all = higher vuln
census_trans <- transform(census,
                          PCollegePlus=-PCollegePlus,
                          MedValue = -MedValue,
                          PerCapInc = -PerCapInc)

all <- census_trans[,5:18]
#apply pca
censusPCA <- prcomp(all,
                    center = TRUE,
                    scale. = TRUE)

#look at results in different ways
print(censusPCA)
plot(censusPCA, type = "lines")
summary(censusPCA)
biplot(censusPCA, scale=0, cex=c(1,0.7))

#compute st. dev. and variance
stddev <- censusPCA$sdev
pr_var <- stddev^2
pr_var[1:10]

#scree plot
propvarex <- pr_var/sum(pr_var) #proportion of variance explained by each component
propvarex
plot(propvarex, xlab = "Principal Comp", ylab = "Prop of Var Expl", type = "b")
plot(cumsum(propvarex), xlab = "Principal Comp", ylab = "Cum Prop of Var Expl", type = "b")

#Kaiser criterion
plot(censusPCA, type="line")
abline(h=1,col="red")
censusPCA$sdev^2
#4 components meet the criterion 

#varimax rotation
PCArot <- censusPCA$rotation[,1:4]
vmx <- varimax(PCArot,normalize=FALSE)

#scores
scores<-top4<-censusPCA$x[,1:4]
scoresTrans <- transform(as_data_frame(scores),PC1=-PC1) #change cardinality of PC1

#apply varimax rotation to scores
scores_rotate <- as.matrix(scoresTrans) %*% vmx$rotmat

#use proportion of variance explained as weighting scheme
var_perc<-data.matrix(propvarex[1:4])

#calculate weighted sum and apply z standardization
sumscores<-scale(data.matrix(scores_rotate)  %*% var_perc)
Svi <- cbind(census_trans[,(1:4)],sumscores)

#write to csv
write.csv(Svi,file = "svi_output.csv",row.names = FALSE)


#----use logit model to check for relationships in survey data----

survey <- read_csv("survey.csv", na="")
survey <- as_data_frame(survey)
#logit model for demog vars
model <- glm(as.factor(PCNoRecovB) ~ Age_Cat+Hincome+
               as.factor(Education)+as.factor(Ethnicity)+as.factor(HomeLang)+as.factor(Gender), 
             family = binomial(link='logit'), data = survey)

summary(model)
#logit model for perception vars
model3 <- glm(as.factor(PCNoRecovB) ~ PCAbleMov+ARelocate+
                KnowEstab+SLRPers+SSDPers+Aprotect+PCUnexp, 
              family = binomial(link='logit'), data = survey)
summary(model3)

model4 <- glm(as.factor(ANoRecovB) ~ PCAbleMov+ARelocate+
                KnowEstab+SLRPers+SSDPers+Aprotect+PCUnexp, 
              family = binomial(link='logit'), data = survey)
summary(model4)

#----use linear regression to test whether any census vars are related to SVI scores
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
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
summary(fit7)
summary(fit8)
summary(fit9)









