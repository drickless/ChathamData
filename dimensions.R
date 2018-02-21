
library(tidyverse)
setwd("/Volumes/easystore/CoastSurvey")

fullsurvey <- read_csv("sgeoTable.csv", na="")
fullsurvey <- as_data_frame(fullsurvey)

#subset questions by dimension
comm <- select(fullsurvey, KnowEstab,KnowLdrs,KnowResid,SatisComm,FeelWelc)
att <- select(fullsurvey, ARelocate,Aprotect,ANoRecover,ALocalMove)
norms <- select(fullsurvey, SNProtect,SNForce,SNStay,SNNoSLR,SNWorryLv)
control <- select(fullsurvey, PCPersCho,PCLater,PCUnexp,PCAbleMov,PCNo__,PCNoRecov,PCNewMov)
outcome <- select(fullsurvey, BOMov5,BOStay,BOMovSafe,BOMovIns,BOStay__,BOProtect)

#reverse cardinality so higher values always = more vulnerability 
commTrans <- transform(comm,
                         KnowEstab=-KnowEstab,
                         KnowLdrs = -KnowLdrs,
                         KnowResid = -KnowResid,
                         SatisComm = -SatisComm,
                         FeelWelc = -FeelWelc)
attTrans <- transform(att,
                       Aprotect=-Aprotect)
normsTrans <- transform(norms,
                       SNProtect=-SNProtect,
                       SNStay = -SNStay,
                       SNNoSLR = -SNNoSLR)
controlTrans <- transform(control,
                        PCPersCho=-PCPersCho,
                        PCLater = -PCLater,
                        PCAbleMov = -PCAbleMov,
                        PCNoRecov = -PCNoRecov,
                        PCNewMov = -PCNewMov)
outcomeTrans <- transform(outcome,
                        BOStay=-BOStay,
                        BOStay__ = -BOStay__,
                        BOProtect = -BOProtect)

#Score = sum of response values / number of responses per person
commTrans$CoScore <- rowSums(commTrans)/nocl(commTrans)
attTrans$AScore <- rowSums(attTrans)/ncol(attTrans)
controlTrans$PCScore <- rowSums(controlTrans)/ncol(controlTrans)
outcomeTrans$BOScore <- rowSums(outcomeTrans)/ncol(outcomeTrans)
normsTrans$SNScore <- rowSums(normsTrans)/ncol(normsTrans)
Svi <- cbind(acsdf_trans[,(1:6)],sumscores)
output <- cbind(fullsurvey[,1],fullsurvey[,(122:127)],commTrans,attTrans,controlTrans,outcomeTrans,normsTrans)

write.csv(output,file = "dimensions.csv",row.names = FALSE)
