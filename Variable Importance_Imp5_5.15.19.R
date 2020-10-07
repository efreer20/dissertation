#Variable Importance for RQ1

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
library(beepr)

#--------------------
#Load Data
#--------------------
#Load all data
data <- read_csv("randomforestdata_imp5_withURM.csv")
data <- na.omit(data)  
#leaves us with 1044 observations

#Save a unique dataset for each of the models

################
#For 1b
################
data_1b <- data %>% 
  select(c(
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",
    "mlc_yr1",
    "V2_ESE",
    "V2_Val",
    "V4_ESE",
    "V4_Val"
    
  ))


#put(outcome/s) at the front of the dataset
data_1b <- data_1b %>% 
  select(c(V4_ESE, V4_Val), everything())

################
#For 1c
################

data_1c <- data %>% 
  select(c(
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    "center_event_yr2", 
    "center_adv_yr2", 
    "mock_yr2",
    "mlc_yr2",
    "adv_yr2",    
    "V3_ESE",
    "V3_Val",
    "V4_ESE",
    "V4_Val"
  ))

#put(outcome/s) at the front of the dataset
data_1c <- data_1c %>% 
  select(c(V4_ESE, V4_Val), everything())

################
#For 1d
################
#selecting only the variables that are needed for this specific analysis
data_1d <- data %>% 
  select(c(
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "mlc_yr3",
    "adv_yr3",    
    "V3_ESE",
    "V3_Val",
    "V4_ESE",
    "V4_Val"
  ))


#put(outcome/s) at the front of the dataset
data_1d <- data_1d %>% 
  select(c(V4_ESE, V4_Val), everything())


################
#For 1e
################

data_1e <- data %>% 
  select(c(
    #demographics
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    "SouthcomplexBinary_2015",
    #math learning
    "mlc_yr1",
    "mlc_yr2",
    "mlc_yr3", 
    #Center
    "center_event_yr2", 
    "center_adv_yr2", 
    "mock_yr2",
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "adv_yr2",    
    "adv_yr3",    
    #Motivation, all 4 time points
    "V1_ESE",
    "V1_Val",
    "V2_ESE",
    "V2_Val",
    "V3_ESE",
    "V3_Val",
    "V4_ESE",
    "V4_Val"))

#put(outcome/s) at the front of the dataset
data_1e <- data_1e %>% 
  select(c(V4_ESE, V4_Val), everything())

################################################################################################
################################################################################################
################################################################################################
################################################################################################

########################
# 1b 
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
trainX <- data.matrix (data_1b [, -(1:2)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_1b [, (1:2)])


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
varimp_1b <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_1b

########################
# 1c
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
trainX <- data.matrix (data_1c [, -(1:2)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_1c [, (1:2)])


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
varimp_1c <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_1c

########################
# 1d
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
trainX <- data.matrix (data_1d [, -(1:2)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_1d [, (1:2)])


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
varimp_1d <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_1d
########################
# 1e
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
trainX <- data.matrix (data_1e [, -(1:2)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_1e [, (1:2)])


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
varimp_1e <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_1e
beep(sound=3)
########################
########################
########################
########################
########################
########################
########################
#Start RQ2
########################
########################
########################
########################
########################
########################
########################

#Variable Importance for RQ2

########################
# Set up workspace
########################

#Working Directory

#Libraries
library(MultivariateRandomForest)
library(tidyverse)
library(doParallel)
library(foreach)

#--------------------
#Load Data
#--------------------
#Load all data
data <- read_csv("randomforestdata_imp5_withURM.csv")
data <- na.omit(data)  
#leaves us with 1044 observations

#Save a unique dataset for each of the models

################
#For 2b
################

data_2b <- data%>% 
  select(c(
    #DEMOGRAPHIC PREDICTORS
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    #CO-CURRICULAR PREDICTORS
    "mlc_yr1",
    #MOTIVATION PREDICTORS
    "V2_ESE",
    "V2_Val",
    #OUTCOMES
    "adv_yr3",    
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "mlc_yr3")) 

#put(outcome/s) at the front of the dataset
data_2b <- data_2b %>% 
  select(c("adv_yr3":"mlc_yr3"), everything())
################
#For 2c
################
#selecting only the variables that are needed for this specific analysis
data_2c <- data %>% 
  select(c(
    #DEMOGRAPHIC PREDICTORS
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    #CO-CURRICULAR PREDICTORS
    "center_event_yr2", 
    "center_adv_yr2", 
    "mock_yr2",
    "mlc_yr2",
    "adv_yr2",    
    #MOTIVATION PREDICTORS
    "V3_ESE",
    "V3_Val",
    #OUTCOMES
    "adv_yr3",    
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "mlc_yr3")) 

#put(outcome/s) at the front of the dataset
data_2c <- data_2c %>% 
  select(c("adv_yr3":"mlc_yr3"), everything())


################
#For 1d
################
data_2d <- data %>% 
  select(c(
    #DEMOGRAPHIC PREDICTORS
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    #ON-CAMPUS LIVING
    "SouthcomplexBinary_2015",
    #CO-CURRICULAR PREDICTORS
    "mlc_yr1",
    "mlc_yr2",
    "adv_yr2",
    "center_event_yr2", 
    "center_adv_yr2", 
    "mock_yr2",
    #MOTIVATION PREDICTORS
    "V1_ESE",
    "V1_Val",
    "V2_ESE",
    "V2_Val",
    "V3_ESE",
    "V3_Val",
    #OUTCOMES
    "adv_yr3",    
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "mlc_yr3")) 


#put(outcome/s) at the front of the dataset
#put(outcome/s) at the front of the dataset
data_2d <- data_2d %>% 
  select(c("adv_yr3":"mlc_yr3"), everything())


################################################################################################
################################################################################################
################################################################################################
################################################################################################

########################
# 2b 
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
trainX <- data.matrix (data_2b [, -(1:5)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_2b [, (1:5)])


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
varimp_2b <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_2b

########################
# 2c
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
trainX <- data.matrix (data_2c [, -(1:5)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_2c [, (1:5)])


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
varimp_2c <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_2c

########################
# 2d
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
trainX <- data.matrix (data_2d [, -(1:5)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_2d [, (1:5)])


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
varimp_2d <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_2d
beep(sound=3)

########################
########################
########################
########################
########################
########################
########################
#Start RQ3
########################
########################
########################
########################
########################
########################
########################


#Variable Importance for RQ3

########################
# Set up workspace
########################

#Working Directory

#Libraries
library(MultivariateRandomForest)
library(tidyverse)

#--------------------
#Load Data
#--------------------
#Load all data
data <- read_csv("randomforestdata_imp5_withURM.csv")
data <- na.omit(data)  
#leaves us with 1044 observations

#Save a unique dataset for each of the models

################
#For 3b
################
data_3b <- data %>% 
  select(c("FEMALE",
           "URM",
           "FIRSTGEN",
           "Max_ACT_Composite",   
           "mlc_yr1",
           "V2_ESE",
           "V2_Val",
           "SS18_Persistence"
  ))


#put(outcome/s) at the front of the dataset
data_3b <- data_3b %>% 
  select(SS18_Persistence, everything())
################
#For 3c
################

data_3c <- data %>% 
  select(c("FEMALE",
           "URM",
           "FIRSTGEN",
           "Max_ACT_Composite",   
           "mlc_yr2",
           "adv_yr2",
           "center_event_yr2", 
           "center_adv_yr2", 
           "mock_yr2",
           "V3_ESE",
           "V3_Val",
           "SS18_Persistence"
  ))

#put(outcome/s) at the front of the dataset
data_3c <- data_3c %>% 
  select(SS18_Persistence, everything())


################
#For 3d
################

data_3d <- data %>% 
  select(c(
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "mlc_yr3",
    "adv_yr3",    
    "V4_ESE",
    "V4_Val",	
    "SS18_Persistence"
  ))

#put(outcome/s) at the front of the dataset
data_3d <- data_3d %>% 
  select(SS18_Persistence, everything())

################
#For 3e
################
data_3e <- data %>% 
  select(c(
    "FEMALE",
    "URM",
    "FIRSTGEN",
    "Max_ACT_Composite",   
    "SouthcomplexBinary_2015",
    "mlc_yr1",
    "mlc_yr2",
    "mlc_yr3",  
    "center_event_yr2", 
    "center_adv_yr2", 
    "mock_yr2",
    "center_event_yr3", 
    "center_adv_yr3", 
    "mock_yr3",
    "adv_yr2",    
    "adv_yr3",    
    "V1_ESE",
    "V1_Val",
    "V2_ESE",
    "V2_Val",
    "V3_ESE",
    "V3_Val",
    "V4_ESE",
    "V4_Val",
    "SS18_Persistence"
  ))

#put(outcome/s) at the front of the dataset
data_3e <- data_3e %>% 
  select(SS18_Persistence, everything())

################################################################################################
################################################################################################
################################################################################################
################################################################################################

########################
# 3b 
########################

# these are tuning parameters that went into build_forest_predict
set.seed(82091)
n_tree=5
m_feature=5
min_leaf=5

###################Save matrices for predictors and outcomes
#make sure covariate data has column names
#Creating the x input matrix (predictors)
trainX <- data.matrix (data_3b [, -(1)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_3b [, (1)])

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
varimp_3b <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_3b

########################
# 3c
########################

# these are tuning parameters that went into build_forest_predict
set.seed(82091)
n_tree=5
m_feature=5
min_leaf=5

###################Save matrices for predictors and outcomes
#make sure covariate data has column names
#Creating the x input matrix (predictors)
trainX <- data.matrix (data_3c [, -(1)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_3c [, (1)])

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
varimp_3c <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_3c

########################
# 3d
########################
# these are tuning parameters that went into build_forest_predict
set.seed(82091)
n_tree=5
m_feature=5
min_leaf=5

###################Save matrices for predictors and outcomes
#make sure covariate data has column names
#Creating the x input matrix (predictors)
trainX <- data.matrix (data_3d [, -(1)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_3d [, (1)])


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
varimp_3d <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_3d


########################
# 3e
########################
# these are tuning parameters that went into build_forest_predict
set.seed(82091)
n_tree=5
m_feature=5
min_leaf=5

###################Save matrices for predictors and outcomes
#make sure covariate data has column names
#Creating the x input matrix (predictors)
trainX <- data.matrix (data_3e [, -(1)]) #creates matrix

#Creating the Y input matrix (outcome)
#Make a matrix for training Y - outcome variables only
trainY = data.matrix (data_3e [, (1)])


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
varimp_3e <- sort(apply(NumRepeatation, 2, mean), decreasing=TRUE)
varimp_3e
beep(sound=3)