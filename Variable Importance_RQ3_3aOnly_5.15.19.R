#Variable Importance for RQ3

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
#Load all data (from imputation 2, like for main analyses)
data <- read_csv("randomforestdata_3.18.19_withURM.csv")
data <- na.omit(data)  
  #leaves us with 1044 observations

#Save a unique dataset for each of the models

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