##################################################
# clear workspace
rm(list=ls())
##################################################

##################################################
# some necessary pkgs
library(MultivariateRandomForest)
library(randomForest)
library(pdp)
library(tidyverse)

setwd("/Volumes/Linnen Lab/MSU Engineering/PROJECTS/Emily Dissertation/*May 2019 Party*")

##################################################

##################################################
# functions i made for you
partDepPlot_bovee =
  function (
    x, pred.data, x.var, which.class, w, plot=TRUE, add=FALSE,
    n.pt = min(length(unique(pred.data[, xname])), 51), 
    xlab=x.var, ylab="",
    main=paste("Effect on", deparse(substitute(x.var))),
    ...
  ){
    checkClass <- x$type != "regression"
    xname <- if (is.character(x.var)) x.var else {
      if (is.name(x.var)) deparse(x.var) else {
        eval(x.var)
      }
    }
    xv <- pred.data[, xname]
    n <- nrow(pred.data)
    if (missing(w)) w <- rep(1, n)
    if (checkClass) {
      if (missing(which.class)) {
        focus <- 1
      }
      else {
        focus <- charmatch(which.class, colnames(x$votes))
        if (is.na(focus))
          stop(which.class, "is not one of the class labels.")
      }
    }
    if (is.factor(xv) && !is.ordered(xv)) {
      x.pt <- levels(xv)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
        if (checkClass) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] > 0,
                                              pr[, focus], .Machine$double.eps)) -
                                     rowMeans(log(ifelse(pr > 0, pr, .Machine$double.eps))),
                                   w, na.rm=TRUE)
        } else y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        
      }
      if (add) {
        points(1:length(x.pt), y.pt, type="h", lwd=2, ...)
      } else {
        if (plot) barplot(y.pt, width=rep(1, length(y.pt)), col="blue",
                          xlab = xlab, ylab = ylab, main=main,
                          names.arg=x.pt, ...)
      }
    } else {
      if (is.ordered(xv)) xv <- as.numeric(xv)
      x.pt <- seq(min(xv), max(xv), length = n.pt)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- rep(x.pt[i], n)
        if (checkClass) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] == 0,
                                              .Machine$double.eps, pr[, focus]))
                                   - rowMeans(log(ifelse(pr == 0, .Machine$double.eps, pr))),
                                   w, na.rm=TRUE)
        } else {
          y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        }
      }
      if (add) {
        lines(x.pt, y.pt, ...)
      } else {
        if (plot) plot(x.pt, y.pt, type = "l", xlab=xlab, ylab=ylab,
                       main = main, ...)
      }
    }
    invisible(list(x = x.pt, y = y.pt))
  }


#run model
runMod =
  function(
    data, dvs, ftrs, factorFtrs=NULL, modType
  )  {
    data = data.frame(data, stringsAsFactors = FALSE)
    
    #
    rfForms = lapply(
      dvs, function(dv){
        formula(
          paste0(
            dv, '~ .'
          )
        )
      }
    )
    
    #
    if(modType=='classification'){
      for(dv in dvs){
        data[,dv] = factor(data[,dv])	
      }
    }
    
    if(!is.null(factorFtrs)){
      for(fftrs in factorFtrs){
        data[,fftrs] = factor(data[,fftrs])
      }
    }
    
    #
    mods = lapply(
      rfForms, function(f){
        m=randomForest(f, data=data, type=modType)
        d=data
        return(list(m=m,d=d))
      }
    )
    
    names(mods) = dvs
    return(mods)
  }
##################################################

##################################################

#--------------------
#Load Data
#--------------------
#Load all data (from imputation 2, like for main analyses)

#using imputation 2 because that was the dataset that I randomly selected to use for this analysis

data <- read_csv("randomforestdata_3.18.19_withURM.csv")

#-------Delete listwise
#Remove nas from the dataset
RFdata <- na.omit(data)  
RFdata <- RFdata %>% 
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
RFdata <- RFdata %>% 
  select(SS18_Persistence, everything())

colnames(RFdata)

#############################################################################
# run rf with a single binary dv and get partial dep plots

# run
rfUniVar = runMod(
  data=RFdata, # specify data object that contains dvs and ivs
  dvs=c('SS18_Persistence'), # specify dv to model
  ftrs=names(RFdata)[-c(1)], # specify ftr variables, i select everything but outcomes
  factorFtrs=c('FEMALE','URM','FIRSTGEN', 'SouthcomplexBinary_2015'), # specify which of the ftr variables are factors
  modType='classification' # use regression for continuous dvs and classification for factors
)

# gen partial plots for dv from univariate binary rf
## generate the same for the other dv in the multivariate model
## note of interpretation when dv is binary: 
### plots show the relative logit contribution of the variable on the class 
### probability from the perspective of the model. In other words negative 
### values (in the y-axis) mean that the positive class (dv=1) is less likely for that 
### value of the independent variable (x-axis) according to the model. 
### Similarly positive values mean that the positive class is more likely for that 
### value of the independent variable according to the model. Zero implies 
### no average impact on class probability according to the model.	
ftrsToPlot = names(RFdata)[-c(1)]
par(mfrow=c(5,5))
par(mar=c(2, 2, 2, 2))

for(ftr in ftrsToPlot){
  partDepPlot_bovee(
    rfUniVar$'SS18_Persistence'$'m',
    pred.data=rfUniVar$'SS18_Persistence'$'d', 
    x.var=ftr,
    plot=TRUE, main = ftr
  )
}
##################################################
