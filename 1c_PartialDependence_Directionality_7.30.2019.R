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
# partDepPlot_bovee =
#   function (
#     x, pred.data, x.var, which.class, w, plot=TRUE, add=FALSE,
#     n.pt = min(length(unique(pred.data[, xname])), 51), 
#     xlab=x.var, ylab="",
#     main=paste("Effect on", deparse(substitute(x.var))),
#     ...
#   ){
#     checkClass <- x$type != "regression"
#     xname <- if (is.character(x.var)) x.var else {
#       if (is.name(x.var)) deparse(x.var) else {
#         eval(x.var)
#       }
#     }
#     xv <- pred.data[, xname]
#     n <- nrow(pred.data)
#     if (missing(w)) w <- rep(1, n)
#     if (checkClass) {
#       if (missing(which.class)) {
#         focus <- 1
#       }
#       else {
#         focus <- charmatch(which.class, colnames(x$votes))
#         if (is.na(focus))
#           stop(which.class, "is not one of the class labels.")
#       }
#     }
#     if (is.factor(xv) && !is.ordered(xv)) {
#       x.pt <- levels(xv)
#       y.pt <- numeric(length(x.pt))
#       for (i in seq(along = x.pt)) {
#         x.data <- pred.data
#         x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
#         if (checkClass) {
#           pr <- predict(x, x.data, type = "prob")
#           y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] > 0,
#                                               pr[, focus], .Machine$double.eps)) -
#                                      rowMeans(log(ifelse(pr > 0, pr, .Machine$double.eps))),
#                                    w, na.rm=TRUE)
#         } else y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
#         
#       }
#       if (add) {
#         points(1:length(x.pt), y.pt, type="h", lwd=2, ...)
#       } else {
#         if (plot) barplot(y.pt, width=rep(1, length(y.pt)), col="blue",
#                           xlab = xlab, ylab = ylab, main=main,
#                           names.arg=x.pt, ...)
#       }
#     } else {
#       if (is.ordered(xv)) xv <- as.numeric(xv)
#       x.pt <- seq(min(xv), max(xv), length = n.pt)
#       y.pt <- numeric(length(x.pt))
#       for (i in seq(along = x.pt)) {
#         x.data <- pred.data
#         x.data[, xname] <- rep(x.pt[i], n)
#         if (checkClass) {
#           pr <- predict(x, x.data, type = "prob")
#           y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] == 0,
#                                               .Machine$double.eps, pr[, focus]))
#                                    - rowMeans(log(ifelse(pr == 0, .Machine$double.eps, pr))),
#                                    w, na.rm=TRUE)
#         } else {
#           y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
#         }
#       }
#       if (add) {
#         lines(x.pt, y.pt, ...)
#       } else {
#         if (plot) plot(x.pt, y.pt, type = "l", xlab=xlab, ylab=ylab,
#                        main = main, ...)
#       }
#     }
#     invisible(list(x = x.pt, y = y.pt))
#   }

##################################################
#New Functions
##################################################
#FUNCTION 1 
partDepPlot_bovee =
  function (
    x, pred.data, x.var, which.class, w, plot=TRUE, add=FALSE,
    n.pt = min(length(unique(pred.data[, xname])), 51), 
    xlab=x.var, ylab="",
    main=paste("Effect on", deparse(substitute(x.var))),
    barPlotYlim=NULL,
    linePlotYlim=NULL,
    ...
  )
  {
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
                          names.arg=x.pt, ylim=barPlotYlim,xpd = FALSE, ...)
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
        if (plot) plot(x.pt, y.pt, type = "l", xlab=xlab, ylab=ylab, ylim = linePlotYlim,
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
RFdata <- RFdata %>% 
  select(c(V4_ESE, V4_Val), everything())

colnames(RFdata)
##################################################

##################################################
# run rf with multiple continuous dvs and get partial dep plots

# set seed
set.seed(2020)

# run a multivariate rf with continuous dvs
rfMultiVar = runMod(
  data=RFdata, # specify data object that contains dvs and ivs
  dvs=c('V4_ESE', 'V4_Val'), # specify dvs to model
  ftrs=names(RFdata)[-c(1,2)], # specify ftr variables, i select everything but outcomes
  factorFtrs=c('FEMALE','URM','FIRSTGEN'), # specify which of the ftr variables are factors
  modType='regression' # use regression for continuous dvs and classification for factors
)

# gen partial plots for dvs from multivariate rf
## y-axis for plots is just the predicted dv values
## partial dep plots for cmedv

# choose features to get plots for
ftrsToPlot = names(RFdata)[-c(1,2)]

# there are 16 features so i choose to use a four by four grid
par(mfrow=c(4,4), mar = rep(2,4))
for(ftr in ftrsToPlot){
  partDepPlot_bovee(
    rfMultiVar$'V4_ESE'$'m', # pass model results to fn: modelObj$dvName$'m'
    pred.data=rfMultiVar$'V4_ESE'$'d', # pass underlying data to fn: modelObj$dvName$'d'
    x.var=ftr, # loop to go through each var in ftrsToPlot vector
    plot=TRUE, main = ftr, barPlotYlim = c(1, 5), linePlotYlim = c(1,5)# having this as true returns a plot
  )
}
## generate the same for the other dv in the multivariate model
par(mfrow=c(4,4), mar = rep(2,4))
for(ftr in ftrsToPlot){
  partDepPlot_bovee(
    rfMultiVar$'V4_Val'$'m',
    pred.data=rfMultiVar$'V4_Val'$'d', 
    x.var=ftr,
    plot=TRUE, main = ftr, barPlotYlim = c(1, 5), linePlotYlim = c(1,5)
  )
}
