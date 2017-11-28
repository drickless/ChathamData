library(tidyverse)
install.packages(dummy)
library(dummy)
library(nnet)

setwd("~/Documents/GitHub/ChathamData")
survey <- read_csv("survey.csv", na="")
survey <- as_data_frame(survey)

sapply(survey,function(x) sum(is.na(x)))

#train <- survey[1:200,]
#test <- survey[201:359,]

#logit model for demog vars
model <- glm(as.factor(PCNoRecovB) ~ Age_Cat+Hincome+
               as.factor(Education)+as.factor(Ethnicity)+as.factor(HomeLang)+as.factor(Gender), 
             family = binomial(link='logit'), data = survey)

summary(model)

model2 <- glm(as.factor(ANoRecovB) ~ Age_Cat+Hincome+
               as.factor(Education)+as.factor(Ethnicity)+as.factor(HomeLang)+as.factor(Gender), 
             family = binomial(link='logit'), data = survey)
summary(model2)


#logit model for perception vars
model3 <- glm(as.factor(PCNoRecovB) ~ PCAbleMov+ARelocate+
               KnowEstab+SLRPers+SSDPers+Aprotect+PCUnexp, 
             family = binomial(link='logit'), data = survey)
summary(model3)

model4 <- glm(as.factor(ANoRecovB) ~ PCAbleMov+ARelocate+
                          KnowEstab+SLRPers+SSDPers+Aprotect+PCUnexp, 
                        family = binomial(link='logit'), data = survey)
summary(model4)



#break out demographic groups
Afam <- filter(survey,Ethnicity==3)
Hisp <- filter(survey,Ethnicity==2)
LowInc <- filter(survey,Hincome<3)
HighInc <- filter(survey,Hincome>4)
LowEd <- filter(survey,Education<3)
HighEd <- filter(survey,Education>2)


summary(survey$PCNoRecov)
summary(survey$ANoRecover)

summary(Afam$PCNoRecov)
summary(Afam$ANoRecover)
summary(LowInc$PCNoRecov)
summary(LowInc$ANoRecover)
summary(LowEd$ANoRecover)
summary(HighEd$ANoRecover)


