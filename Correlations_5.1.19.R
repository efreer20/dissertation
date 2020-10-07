#Correlation Table 5.1.19

library(haven)
library(apaTables)
library(tidyverse)
library(tidylog)
setwd("/Volumes/Linnen Lab/MSU Engineering/PROJECTS/Emily Dissertation")

data <- read_spss("Descriptives_Correlations_ImputedDataset.sav")

#select the right people
data <- data %>% dplyr::filter(complete_data == 1)
  
  #test to make sure that worked
  #data %>% count(complete_data)

#select the right variables
table_1 <- data %>% select(c(
  V1_ESE, V2_ESE, V3_ESE, V4_ESE, V1_Val, V2_Val, V3_Val,
  V4_Val, adv_yr2, adv_yr3, center_event_yr2, center_event_yr3, center_adv_yr2, center_adv_yr3, 
  mock_yr2, mock_yr3, mlc_yr1, mlc_yr2, mlc_yr3
))

table_2 <- data %>% select(c(
  V1_ESE, V2_ESE, V3_ESE, V4_ESE, V1_Val, V2_Val, V3_Val,
  V4_Val, adv_yr2, adv_yr3, center_event_yr2, center_event_yr3, center_adv_yr2, center_adv_yr3, 
  mock_yr2, mock_yr3, mlc_yr1, mlc_yr2, mlc_yr3, SouthcomplexBinary_2015, FEMALE, Max_ACT_Composite, FIRSTGEN, SS18_Persistence)
)

#make a correlation table
apa.cor.table(table_1, filename = "Correlation_Table_1.doc", table.number = NA,
              show.conf.interval = FALSE, landscape = TRUE)

apa.cor.table(table_2, filename = "Correlation_Table_2.doc", table.number = NA,
              show.conf.interval = FALSE, landscape = TRUE)
