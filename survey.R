library(tidyverse)
install.packages(dummy)
library(dummy)

setwd("~/Documents/GitHub/ChathamData")
survey <- read_csv("survey.csv", na="")
survey <- as_data_frame(survey)

sapply(survey,function(x) sum(is.na(x)))

train <- survey[1:200,]
test <- survey[201:359,]

model <- glm(as.factor(PCNoRecov) ~ Age_Cat+Hincome+
               as.factor(Education)+as.factor(Ethnicity)+as.factor(HomeLang)+as.factor(Gender), 
             family = binomial(link='logit'), data = train)

summary(model)
