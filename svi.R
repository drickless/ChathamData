install.packages("stats")

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


#apply pca
censusPCA <- prcomp(census_trans[5:18],
                    center = TRUE,
                    scale. = TRUE)
#look at results
print(censusPCA)
plot(censusPCA, type = "lines")
summary(censusPCA)
