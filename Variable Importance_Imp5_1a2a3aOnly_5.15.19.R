#May 15, 2019
#The purpose is to check robustness for Models 1a, 2a, and 3a again in the new specification Lisa requested

#This code file is an aggregation of the following files from "Models 1a 2a 3a"
  # - Variable Importance-RQ1_1aOnly.R (starts on line 30)
  # - Variable Importance-RQ2_2aOnly.R (starts on line 130)
  # - Variable Importance-RQ3_3aOnly.R

########################
# Set up workspace
########################

#Working Directory
setwd("/Volumes/Linnen Lab/MSU Engineering/PROJECTS/Emily Dissertation/*May 2019 Party*")
#Libraries
library(MultivariateRandomForest)
library(tidyverse)
library(doParallel)
library(foreach)

#--------------------
#Load Data
#--------------------
data <- read_csv("randomforestdata_imp5_withURM.csv")
data <- na.omit(data)  
#leaves us with 1044 observations

#Save a unique dataset for each of the models

################
#For 1a
################

data_1a <- data %>% 
  select(c(
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    "SouthcomplexBinary_2015",
    "V1_ESE",
    "V1_Val",
    "V4_ESE",
    "V4_Val"
  ))

#put(outcome/s) at the front of the dataset
data_1a <- data_1a %>% 
  select(c(V4_ESE, V4_Val), everything())

################################################################################################
################################################################################################
################################################################################################
################################################################################################

########################
# 1a 
########################
# these are tuning parameters that went into build_forest_predict
set.seed(82091)
n_tree=5
m_feature=5
min_leaf=5

###################Save matrices for predictors and outcomes
#make sure covariate data has column names
#Creating the x input matrix (predictors)
#Make a matrix of predictor variables, for training X, exclude outcome
trainX <- data.matrix (data_1a [, -(1:2)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_1a [, (1:2)])


# useless theta function, idk why it exists
theta <- function(trainX){trainX}

# 
results <- bootstrap::bootstrap(1:nrow(trainX),n_tree,theta)
b=results$thetastar

# some params related to the number of dependent variables
Variable_number=ncol(trainY)
if (Variable_number>1){
  Command=2
}else if(Variable_number==1){
  Command=1
}
NumVariable=ncol(trainX)

# set up matrix to store result from var importance analysis
NumRepeatation=matrix(rep(0,n_tree*NumVariable),
                      nrow=n_tree,
                      dimnames = list(NULL, colnames(trainX)))

#Set up parallel processing system
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)

# run trees to make forest
for (i in 1:n_tree){
  
  # reorder rows by bootstrap thing
  X=trainX[ b[ ,i], ]
  Y=matrix(trainY[ b[ ,i], ],ncol=Variable_number)
  
  # get out inverse covariance matrix for y
  Inv_Cov_Y = solve(cov(Y)) # calculate the V inverse
  if (Command==1){
    Inv_Cov_Y=matrix(rep(0,4),ncol=2)
  }
  
  # run tree
  Single_Model=build_single_tree(X, Y, m_feature, min_leaf,Inv_Cov_Y,Command)
  
  # extract variable importance
  NumRepeatation[i,]=variable_importance_measure(Single_Model,NumVariable)
}

stopCluster(cl)
registerDoSEQ()

# calcualte average number of times a variable showed up in 
# splitting a tree intro a branch
varimp_1a <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_1a


#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
#Start 2a Here

#############################################
################
#For 2a
################
data_2a <- data %>% 
  select(c(
    #DEMOGRAPHIC PREDICTORS
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    #ON-CAMPUS LIVING
    "SouthcomplexBinary_2015",
    #MOTIVATION PREDICTORS
    "V1_ESE",
    "V1_Val",
    #OUTCOMES
    "adv_yr3",    
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "mlc_yr3")) 

#put(outcome/s) at the front of the dataset
data_2a <- data_2a %>% 
  select(c("adv_yr3":"mlc_yr3"), everything())


################################################################################################
################################################################################################
################################################################################################
################################################################################################

########################
# 2a 
########################
# these are tuning parameters that went into build_forest_predict
set.seed(82091)
n_tree=5
m_feature=5
min_leaf=5

###################Save matrices for predictors and outcomes
#make sure covariate data has column names
#Creating the x input matrix (predictors)
#Make a matrix of predictor variables, for training X, exclude outcome
trainX <- data.matrix (data_2a [, -(1:5)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_2a [, (1:5)])


# useless theta function, idk why it exists
theta <- function(trainX){trainX}

# 
results <- bootstrap::bootstrap(1:nrow(trainX),n_tree,theta)
b=results$thetastar

# some params related to the number of dependent variables
Variable_number=ncol(trainY)
if (Variable_number>1){
  Command=2
}else if(Variable_number==1){
  Command=1
}
NumVariable=ncol(trainX)

# set up matrix to store result from var importance analysis
NumRepeatation=matrix(rep(0,n_tree*NumVariable),
                      nrow=n_tree,
                      dimnames = list(NULL, colnames(trainX)))

#Set up parallel processing system
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)

# run trees to make forest
for (i in 1:n_tree){
  
  # reorder rows by bootstrap thing
  X=trainX[ b[ ,i], ]
  Y=matrix(trainY[ b[ ,i], ],ncol=Variable_number)
  
  # get out inverse covariance matrix for y
  Inv_Cov_Y = solve(cov(Y)) # calculate the V inverse
  if (Command==1){
    Inv_Cov_Y=matrix(rep(0,4),ncol=2)
  }
  
  # run tree
  Single_Model=build_single_tree(X, Y, m_feature, min_leaf,Inv_Cov_Y,Command)
  
  # extract variable importance
  NumRepeatation[i,]=variable_importance_measure(Single_Model,NumVariable)
}

stopCluster(cl)
registerDoSEQ()

# calcualte average number of times a variable showed up in 
# splitting a tree intro a branch
varimp_2a <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_2a
beep()

#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
#Start 3a Here

#############################################


################
#For 3a
################

#selecting only the variables that are needed for this specific analysis
data_3a <- data %>% 
  select(c(
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    "SouthcomplexBinary_2015",
    "V1_ESE",
    "V1_Val",
    "SS18_Persistence"
  ))


#put(outcome/s) at the front of the dataset
data_3a <- data_3a %>% 
  select(SS18_Persistence, everything())

################################################################################################
################################################################################################
################################################################################################
################################################################################################

########################
# 3a 
########################
# these are tuning parameters that went into build_forest_predict
set.seed(82091)
n_tree=5
m_feature=5
min_leaf=5

###################Save matrices for predictors and outcomes
#make sure covariate data has column names
#Creating the x input matrix (predictors)
#Make a matrix of predictor variables, for training X, exclude outcome
trainX <- data.matrix (data_3a [, -(1)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_3a [, (1)])

# useless theta function, idk why it exists
theta <- function(trainX){trainX}

# 
results <- bootstrap::bootstrap(1:nrow(trainX),n_tree,theta)
b=results$thetastar

# some params related to the number of dependent variables
Variable_number=ncol(trainY)
if (Variable_number>1){
  Command=2
}else if(Variable_number==1){
  Command=1
}
NumVariable=ncol(trainX)

# set up matrix to store result from var importance analysis
NumRepeatation=matrix(rep(0,n_tree*NumVariable),
                      nrow=n_tree,
                      dimnames = list(NULL, colnames(trainX)))

#Set up parallel processing system
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)

# run trees to make forest
for (i in 1:n_tree){
  
  # reorder rows by bootstrap thing
  X=trainX[ b[ ,i], ]
  Y=matrix(trainY[ b[ ,i], ],ncol=Variable_number)
  
  # get out inverse covariance matrix for y
  Inv_Cov_Y = solve(cov(Y)) # calculate the V inverse
  if (Command==1){
    Inv_Cov_Y=matrix(rep(0,4),ncol=2)
  }
  
  # run tree
  Single_Model=build_single_tree(X, Y, m_feature, min_leaf,Inv_Cov_Y,Command)
  
  # extract variable importance
  NumRepeatation[i,]=variable_importance_measure(Single_Model,NumVariable)
}

stopCluster(cl)
registerDoSEQ()

# calcualte average number of times a variable showed up in 
# splitting a tree intro a branch
varimp_3a <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_3a
beep()