# Process Survey Data

library(tidyverse)
library(haven)
library(tidylog)
library(questionr)
#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------
# Loading data ------------------------------------
#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------

setwd("/Volumes/Linnen Lab/MSU Engineering/PROJECTS/Emily Dissertation")
DATA <- read_spss("Emily_Dissertation_2.28.19.sav")

#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------
#Institutional Data Variables----------------------
#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------
#### This variable creation syntax to transform variables from the
  # 2018 institutional data file is also in 
  #"Processing EGR Institutional Data_8.2.18"

###########STEP 1

#Check the current variables to see if "Gender" and "Gen_Inst" are the same.
  #if they are, then I can be confident that the Gen_Inst variable was created 
    #from the 2018 institutional data, and I can use Gen_Inst in my analyses.

DATA %>% 
  count(Gender)

DATA %>% 
  count(Gen_Inst)

# The result of that is that the existing variable called Gen_Inst is missing for 63 people
  #by contrast, the institutional data Gender received in July 2018 ("Gender") is only missing 2 people

#CREATING GENDER (Gen_Inst_18) VARIABLE

#Use Gender to create Gen_Inst.
DATA <- mutate(DATA,
               Gen_Inst_18 = ifelse(Gender == "Female", 1,
                                 ifelse(Gender == "Male", 0, NA)))

#compare frequencies to make sure they are right
DATA%>%
  count(Gender)

DATA%>%
  count(Gen_Inst_18)


###########STEP 2

#First check whether the existing Race_I variable includes Spring 2018 data

DATA %>%          #from 2018 inst. data
  count(Ethnicity)

DATA %>%          #existing processed inst. data variable, but we don't know from what time point.
  count(Race_I)

  #Result of those two frequency checks: seems like my Ethnicity variable from 2018 captures more data.
  #It needs to be recoded accordingly.

#CREATING new Race_I_18 variable
DATA<-mutate(DATA,
             Race_I_18 = ifelse(Ethnicity == "American Indian/Alaskan Native (non-Hispanic)", 1,
                             ifelse(Ethnicity == "Asian (non-Hispanic)", 3,
                                    ifelse(Ethnicity == "Black or African American  (non-Hispanic)", 4,
                                           ifelse(Ethnicity == "Hawaiian / Pacific Islander (non-Hispanic)", 2,
                                                  ifelse(Ethnicity == "Hispanic Ethnicity", 9,
                                                         ifelse(Ethnicity =="White (non-Hispanic)", 5, NA)))))))
#Institutional data codes that are now "missing" - from July 2018 data
  # Code 7: Two or more races (non-Hispanic) - because we don't know what two races they are, so can't code as multiracial URM or non-URM
  # Code 5: Not Reported (n = 11)
  # Code 6: Not Requested (n = 194)
  # "" : Data that were already missing (n = 2)
    
#compare frequencies to make sure they are right
DATA%>%
  count(Ethnicity)

#COMPARE my newly created one to the one that already exists in the data
DATA%>%
  count(Race_I_18)

DATA %>% 
  count(Race_I)

###########STEP 3

#CREATING FirstGen_I VARIABLE
DATA<-mutate(DATA,
             FirstGen_I_18 = ifelse(Adms_First_Gen == "Y", 1, 
                                 ifelse(Adms_First_Gen == "N", 0, NA)))

#compare frequencies to make sure they are right
DATA%>%
  count(Adms_First_Gen)

DATA%>%
  count(FirstGen_I_18)

###########STEP 4
#Save an abbreviated dataset and export it to SPSS

#Delete old institutional data (not the data used to create my new variables, which came in July 2018,
  #but instead the institutional data that already existed in the dataset)

DATA <- DATA %>%
  select(-(Gen_Inst:Major_US16))

write_sav(DATA, "Emily_Dissertation_3.1.19.sav")

#From here, refer to the dataset in SPSS in order to collapse the demographic variables

#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------

# Preliminary processing ---------------------------
#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------
# 


# N   O   T   E

#Everything from here down should NOT be run in this code file. 


It is reproduced in the file Impute!_3.6.19.Rmd and should be run there.








# #First, load new dataset from SPSS
# 
# survey <- read_spss("Emily_Dissertation_3.1.19_EndofDay.sav")
# 
# #Keep only the variables I want
# 
# #Make a list of variables to keep
#   ## Eventually, I'll be doing this with the imputed data - but I do still need to keep the other variables for the imputation
# 
# keep <- c("StudyID", #unique identifier
#           "V1Completion", #survey completion
#           "V2Completion",
#           "V3Completion",
#           "V4Completion",
#           "Total_Surveys_Completed", #variable calculated to count surveys comp.
#           "RACE_ETH", # demographics
#           "FIRSTGEN",
#            "FEMALE",
#           "SouthcomplexBinary_2015", #did student live in South complex in 2015?
#           "Max_ACT_Composite", #Prior Achievement
#           "SS18_Persistence", #Outcome
#           "V1ESE1", "V1ESE2", "V1ESE3", "V1ESE4", "V1ESE5", #motivation
#           "V2ESE1", "V2ESE2", "V2ESE3", "V2ESE4", "V2ESE5",
#           "V3ESE1", "V3ESE2", "V3ESE3", "V3ESE4", "V3ESE5",
#           "V4ESE1", "V4ESE2", "V4ESE3", "V4ESE4", "V4ESE5",
#           "V1Int1", "V1Int2", "V1Int3", "V1Int4", "V1Int5",
#           "V2Int1", "V2Int2", "V2Int3", "V2Int4", "V2Int5",
#           "V3Int1", "V3Int2", "V3Int3", "V3Int4", "V3Int5",
#           "V4Int1", "V4Int2", "V4Int3", "V4Int4", "V4Int5",
#           "V1Att2", "V1Att3", "V1Att4", "V1Att5",
#           "V2Att2", "V2Att3", "V2Att4", "V2Att5",
#           "V3Att2", "V3Att3", "V3Att4", "V3Att5",
#           "V4Att2", "V4Att3", "V4Att4", "V4Att5",
#           "V1Util1", "V1Util2", "V1Util5",
#           "V2Util1","V2Util2", "V2Util5",
#           "V3Util1", "V3Util2", "V3Util5",
#           "V4Util1", "V4Util2", "V4Util5")
#           
# survey_final <- survey  %>% 
#   select(one_of(keep))


############################################################################################################
##CSTAT NOTES 2.18.2019
############################################################################################################

#How can I defend what I'm claiming - about the missingness mechanism - ways to provide evidence

# Basic Consulting
#   Tutoring/consulting - learn how to implement it myself, she answers questions I may have or troubleshoot problems I encounter 
  
#Enhanced - Faculty and research staff
  # Can pay CSTAT to do the consulting
  #TO switch to enhanced, have to get

### Who is my population - because that would affect the way I explain missingness.
  # if my population is MSU - I could use MSU to estimate what variables I'm missing
  # if my population is not MSU - I 

#THe fact that data is missing does not mean that imputatino is next 
 # Data have to meet certain conditions
  
#Missing data have different mechanisms
    #CONDITION 1: MCAR The mechanism for missing is random - no predicted pattern in missingness
      # if data are missing completely at random - it doesn't matter how you deal with missing data
      # imputation and listwise deletion will yield the same results
  
    #CONDITION 2: MAR Missing because of a variable we have measured - missing because of something we have measured in the data (like heavy courseload)
  
    #CONDITION 3: MNAR When data is missing on avariable because of that variable - I didn't fill it out because I'm demotivated or highly motivated
      #No solution for this one - maybe even delete listwise
      #One way to tell: motivation score ranges between this and this - if all of a sudden at one time point we don't have a good spread, maybe that suggests that motivation is a mechanism

#Testing for missing mechanism  
  # There is no test for Missing at random or missing not at random  
#STEP 1: Run test for missing completely at random  
#  Little's Test for Missing Completely at Random

#STEP 2: Do imputation with the raw data because with the composite, some info is lost
  #the relationships used to impute values won't be as fine as they should be if you use an average
    #detailed info is lost in the aggregation

#STEP 3: Test for MAR
    # IF there's a difference between males and females in missingness - then missingness is because of these variables
    # and then you can control for those variables in the model
  
    #If you want to use imputation - use all the variables that you've measured that you think would be sensible
    # use
  
#STEP 4 : check for FIML in the random forest function
  
#STEP 5: Look into imputing longitudinal data with mice (measurements at time 1 will be related to time 2, time 3, time 4)
    #Google it
  
#STEP 6: For each variable at each time point, what percentage is missing
  #For all motivation variables
  #how many people are missing the outcome variable?