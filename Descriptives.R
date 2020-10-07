library(tidyverse)

setwd("/Volumes/Linnen Lab/MSU Engineering/PROJECTS/Emily Dissertation")

data <- read_csv("randomforestdata_3.18.19.csv")

#-------Delete listwise
#Remove nas from the dataset
DATA <- na.omit(data)  

#Frequencies WITHIN the 1044 sample
  #that is, the 1044 actually used in the main analyses
#########
#GENDER
#########
#Gender frequencies
DATA%>%
  count(FEMALE)

# A tibble: 2 x 2
# FEMALE     n
# <dbl> <int>
# 1      0   759
# 2      1   285


#########
#RACE
#########
DATA %>% 
  count(RACE_ETH)

# RACE_ETH     n
# <dbl> <int>
# 1        1     2
# 2        2     1
# 3        3   126
# 4        4    76
# 5        5   765
# 6        7    10
# 7        8    12
# 8        9    52

#########
#FIRST GEN
#########

DATA %>% 
  count(FIRSTGEN)

# # A tibble: 2 x 2
# FIRSTGEN     n
# <dbl> <int>
# 1        0   852
# 2        1   192

#########
#Response rates
#########

DATA %>% 
  count(V1Completion)
# # A tibble: 2 x 2
# V1Completion     n
# <dbl> <int>
# 1            0   395
# 2            1   649

DATA %>%
  count(V2Completion)
# # A tibble: 2 x 2
# V2Completion     n
# <dbl> <int>
# 1            0   353
# 2            1   691

DATA %>% 
  count(V3Completion)

# A tibble: 2 x 2
# V3Completion     n
# <dbl> <int>
# 1            0   476
# 2            1   568

DATA %>% 
  count(V4Completion)

# # A tibble: 2 x 2
# V4Completion     n
# <dbl> <int>
# 1            0   413
# 2            1   631
