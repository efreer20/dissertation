#Merge File!
  #in which I draw together all my disparate datasets into one big thing to draw cool conclusions.

library(tidyverse)
library(haven)
library(tidylog)
library(questionr)
library(skimr)
setwd("/Volumes/Linnen Lab/MSU Engineering/PROJECTS/Emily Dissertation")

##-------------------
# Survey Dataset
##-------------------
#Read in SPSS dataset - Imputed
  #bring it in from SPSS because I have the scales computed
  #it's the imputed dataset
  # and I created the scales in it 
survey <- read_spss("imp2_withScales.sav")

  #1287 observations
  #88 variables

#Drop the individual items so that there won't be problems with collinearity in the random forest

#colnames(survey)

survey <- survey %>% 
  select(-(V1ESE1:V4Util5))

#colnames(survey)

##-------------------
# Advising Dataset
##-------------------

#Read in Advising dataset - Processed and done
advising <-read_csv("advising_processed.csv")
  #808 observations
  #7 variables

#glimpse(advising)
#skim(advising)

#Recode so NA = 0

advising <- advising %>% 
        replace(., is.na(.), 0)

#Ran this to make sure the replacement worked; it did
#questionr::freq(advising$adv_U17)
#questionr::freq(test$adv_U17)

##-------------------
# MLC Dataset
##-------------------

#Read in MLC dataset - Processed and done
mlc <- read_csv("mlc_final.csv")
  #798 observations
  #9 variables

#Recode so NA = 0

mlc <- mlc %>% 
  replace(., is.na(.), 0)

##-------------------
# Center Dataset
##-------------------

#Read in Center dataset - Processed and done
center <- read_csv("center_processed.csv")

#Recode so NA = 0

center <- center %>% 
  replace(., is.na(.), 0)

##---------------------------------------------------------
#Merging to get the co-curricular data all in one place
#and then making sure there are no NAs in co-curricular data.
##---------------------------------------------------------
#specify the complete (all participants) dataset as the first argument in the first join
  #that way, all future joins will have all the right people in them
  #and columns will be added to represent new variables being merged, but no new people will be added.

#I do this by 3 joins and then selecting only the co-curricular activities 
  #(because I don't want to add any 0s to the survey data)

####j1 survey + advising
join_1 <- left_join(survey, advising)


####j2 survey/advising + mlc
join_2 <- left_join(join_1, mlc)


####j3 survey/advising/mlc + center
join_3 <- left_join(join_2, center)
      #that is 64 variables

####select only the co-curricular data
co_curric_complete <- join_3 %>%
                        select(study_id, adv_F16:jobapp_U17)
    #this is 45 variables

#Next replace all NAs with 0s in the co-curricular data 
co_curric_complete <- co_curric_complete %>% 
  replace(., is.na(.), 0)

##---------------------------------------------------------
#Merge survey data with the co-curricular complete data
##---------------------------------------------------------

rfdata <- left_join(survey, co_curric_complete)

  ##then check the resulting file to make sure that there are no missing on anything

##---------------------------------------------------------
#Save this dataset for use in random forest
##---------------------------------------------------------

write_csv(rfdata, "TEMPrandomforestdata.csv")

#--------------------------------------------------------
#Further Reading
  ##NA replace
# https://rickpackblog.wordpress.com/2017/11/20/replace-na-in-all-columns-with-dplyr-rstats/

  #joins
# https://stat545.com/bit001_dplyr-cheatsheet.html



## side notes

#this is the way I checked to make sure my is.na syntax was working
  #i did this on mlc and advising. 
  #didn't check explicitly each variable for center or the merged co-curricular, but I know it worked because of a visual scan
    #and because I know how the syntax behaves based on variable-level checking for mlc and advising

#colnames(test)
# 
# questionr::freq(mlc$mlc_visits_F15)
# questionr::freq(test$mlc_visits_F15)
# 
# questionr::freq(mlc$mlc_visits_FS16)
# questionr::freq(test$mlc_visits_FS16)
# 
# questionr::freq(mlc$mlc_visits_FS17)
# questionr::freq(test$mlc_visits_FS17)
# 
# questionr::freq(mlc$mlc_visits_S16)
# questionr::freq(test$mlc_visits_S16)
# 
# questionr::freq(mlc$mlc_visits_SS17)
# questionr::freq(test$mlc_visits_SS17)
# 
# questionr::freq(mlc$mlc_visits_SS18)
# questionr::freq(test$mlc_visits_SS18)
# 
# questionr::freq(mlc$mlc_visits_US16)
# questionr::freq(test$mlc_visits_US16)
# 
# questionr::freq(mlc$mlc_visits_US17)
# questionr::freq(test$mlc_visits_US17)
