#Quick linear models to check partial dependence findings
rm(list=ls())
setwd("~/Google Drive/** Dissertation **/VARIANCE EXPLAINED")
library(readr)
library(tidyverse)

data <- read_csv("randomforestdata_3.18.19_withURM.csv")
lm.gender <- lm(data$SS18_Persistence ~ data$FEMALE)
summary(lm.gender)

lm.urm <- lm(data$SS18_Persistence ~ data$URM)
summary(lm.urm)

lm.firstgen <- lm(data$SS18_Persistence ~ data$FIRSTGEN)
summary(lm.firstgen)

lm.mega <- lm(data$SS18_Persistence ~ data$FEMALE + data$URM + data$FIRSTGEN)
summary(lm.mega)



#Ref about linear modeling in R
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html