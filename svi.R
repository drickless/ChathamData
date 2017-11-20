install.packages("prcomp")

library(tidyverse)
library(prcomp)

setwd("~/Documents/GitHub/ChathamData")
census <- read_csv("censusClean.csv", na="")
census <- as_data_frame(census)

