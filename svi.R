#script for constructing a weighted social vulnerability index from acs data

library(tidyverse)

setwd("~/Documents/GitHub/ChathamData")
#census <- read_csv("acsdfClean.csv", na="")
#cemsus <- as_data_frame(acsdf)

acs <- read_csv("SVIdata2.csv", na="")
acsdf <- as_data_frame(acs)

colnames(acsdf)

#replace missing values with median
sapply(acsdf,function(x) sum(is.na(x)))
acsdf$MedRent[is.na(acsdf$MedRent)] <- median(acsdf$MedRent,na.rm = T)
acsdf$MedVal[is.na(acsdf$MedVal)] <- median(acsdf$MedVal,na.rm = T)

#reverse direction of vars so higher values for all = higher vuln
acsdf_trans <- transform(acsdf,
                          MedRent=-MedRent,
                          MedVal = -MedVal,
                          PercapInc = -PercapInc,
                          PercHighInc = -PercHighInc)

all <- acsdf_trans[,7:32]
#apply pca
acsdfPCA <- prcomp(all,
                    center = TRUE,
                    scale. = TRUE)

#look at results in different ways
print(acsdfPCA)
plot(acsdfPCA, type = "lines")
summary(acsdfPCA)
biplot(acsdfPCA, scale=0, cex=c(1,0.7))

#compute st. dev. and variance
stddev <- acsdfPCA$sdev
pr_var <- stddev^2
pr_var[1:10]

#scree plot
propvarex <- pr_var/sum(pr_var) #proportion of variance explained by each component
propvarex
plot(propvarex, xlab = "Principal Comp", ylab = "Prop of Var Expl", type = "b")
plot(cumsum(propvarex), xlab = "Principal Comp", ylab = "Cum Prop of Var Expl", type = "b")

#Kaiser criterion
plot(acsdfPCA, type="line")
abline(h=1,col="red")
acsdfPCA$sdev^2 #eigenvalues
#7 components meet the criterion 



#varimax rotation
PCArot <- acsdfPCA$rotation[,1:7] #subset selected components ("rotations" = loadings)
vmx <- varimax(PCArot,normalize=FALSE)


#scores
scores<-top7<-acsdfPCA$x[,1:7]
scoresTrans <- transform(as_data_frame(scores),
                         PC1=-PC2,
                         PC3=-PC3,
                         PC6=abs(PC6)) #change cardinality of components that decrease vuln

#apply varimax rotation to scores
scores_rotate <- as.matrix(scores) %*% vmx$rotmat

#use proportion of variance explained as weighting scheme
var_perc<-data.matrix(propvarex[1:7])

#calculate weighted sum and apply z standardization
sumscores<-scale(data.matrix(scores_rotate)  %*% var_perc)
Svi <- cbind(acsdf_trans[,(1:4)],sumscores) #append to identifying info

#write to csv
write.csv(Svi,file = "svi_output2.csv",row.names = FALSE)




