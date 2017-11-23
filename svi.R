library(tidyverse)

setwd("~/Documents/GitHub/ChathamData")
census <- read_csv("censusClean.csv", na="")
census <- as_data_frame(census)

colnames(census)

#replace missing values with median
sapply(census,function(x) sum(is.na(x)))
census$PerCapInc[is.na(census$PerCapInc)] <- median(census$PerCapInc,na.rm = T)
census$MedValue[is.na(census$MedValue)] <- median(census$MedValue,na.rm = T)

#reverse direction of vars so higher values for all = higher vuln
census_trans <- transform(census,
                          PCollegePlus=-PCollegePlus,
                          MedValue = -MedValue,
                          PerCapInc = -PerCapInc)
#log transform
#log.census <- log(census_trans[5:18])



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

#Kaiser eigenvalue-greater-than-one rule
plot(censusPCA, type="line")
abline(h=1,col="red")
censusPCA$sdev^2
#4 components meet the criterion 

PCArot <- censusPCA$rotation[,1:4]
#PCArot_trans <- as.matrix(transform(PCArot,PC1=-PC1))

#varimax rotation
#vmx <- varimax(PCArot_trans,normalize=FALSE)
vmx <- varimax(PCArot,normalize=FALSE)

#scores
scores<-top4<-censusPCA$x[,1:4]
scoresTrans <- transform(as_data_frame(scores),PC1=-PC1) #change cardinality of PC1

#apply varimax rotation to scores
#scores_rotate<-scores %*% vmx$rotmat
scores_rotate <- as.matrix(scoresTrans) %*% vmx$rotmat

#use proportion of variance explained as weighting scheme
var_perc<-data.matrix(propvarex[1:4])
#data.matrix(scores_rotate)

#calculate weighted sum and apply z standardization
sumscores<-scale(data.matrix(scores_rotate)  %*% var_perc)
Svi <- cbind(census_trans[,(1:4)],sumscores)

#write to csv
write.csv(Svi,file = "svi_output.csv",row.names = FALSE)




