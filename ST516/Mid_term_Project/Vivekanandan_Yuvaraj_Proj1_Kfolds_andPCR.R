########################### Project 1 ########################
library(ISLR)
require(leaps)
require(glmnet)
require(faraway)
require(dplyr)
require(pls)

# Load data 
setwd("F:/MS-OR/Semester_3/ST516_Experimental_Statistics/Exams/Mid-Term/")

bikes = read.csv("bikes.csv")
View(bikes)

# Remove instant column
bikes = subset(bikes,select = -c(instant))

#Bifurcating data for registered and casual riders
bikes_casual = subset(bikes,select = -c(registered))
bikes_registered = subset(bikes,select = -c(casual))

############ K-Folds Subset ##########################
# create function to predict from reg subsets object
# see ISLR, LAb 6.5.3

pred.sbs=function(obj,new,id,...){
  form=as.formula(obj$call[[2]])
  mat=model.matrix(form,new)
  coefi=coef(obj,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


###################### K-Folds - Casual #############################
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:10,nrow(bikes_casual),replace=T) 
# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,3,dimnames=list(NULL,paste(1:3)))

# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-6 predictors fit without kth fold
  # factor(season) + factor(workingday) + factor(weathersit) +
  best.mods=regsubsets(casual~  temp + hum + windspeed + factor(season) + factor(workingday) + factor(weathersit) ,data=bikes_casual[folds!=j,],
                       nvmax=3,method="exhaustive")
  # estimate test error for all 6 models by predicting kth fold 
  for (i in 1:3){
    pred=pred.sbs(best.mods,bikes_casual[folds==j,],id=i)
    cv.err[j,i]=mean((bikes_casual$casual[folds==j]-pred)^2)  # save error est
  }
}

mse.cv=apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:3,mse.cv,type="b",xlab="no. of predictors",ylab="est. test MSE",ylim=c(0,350000))
points(min,mse.cv[min],cex=2,col="red",lwd=2)

###################### K-Folds - Registered #############################
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:10,nrow(bikes_registered),replace=T) 
# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,3,dimnames=list(NULL,paste(1:3)))

# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-6 predictors fit without kth fold
  # factor(season) + factor(workingday) + factor(weathersit) +
  best.mods=regsubsets(registered~  temp + hum + windspeed ,data=bikes_registered[folds!=j,],
                       nvmax=3,method="exhaustive")
  # estimate test error for all 6 models by predicting kth fold 
  for (i in 1:3){
    pred=pred.sbs(best.mods,bikes_registered[folds==j,],id=i)
    cv.err[j,i]=mean((bikes_registered$registered[folds==j]-pred)^2)  # save error est
  }
}

mse.cv=apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:3,mse.cv,type="b",xlab="no. of predictors",ylab="est. test MSE",ylim=c(1500000,1800000))
points(min,mse.cv[min],cex=2,col="red",lwd=2)


############## PCR - Casual ##################################

pcr.mod=pcr(casual~  temp + hum + windspeed + factor(season) + factor(workingday) + factor(weathersit),data=bikes_casual,scale=T,validation="CV")
summary(pcr.mod)
validationplot(pcr.mod,val.type="MSEP")

# plotfitted values for OLS and PCR, compare with actual
lmod=lm(casual~  temp + hum + windspeed + factor(season) + factor(workingday) + factor(weathersit)  ,data=bikes_casual)
fit.pcr=predict(pcr.mod,data=bikes_casual,ncomp=6)
plot(lmod$fitted.values,bikes_casual$casual,pch=19,col="blue")
points(fit.pcr,bikes_casual$casual,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.pcr=cor(fit.pcr,bikes_casual$casual)^2
R2.lm=cor(lmod$fitted.values,bikes_casual$casual)^2
R2.pcr
R2.lm

# Observation : The data matches exactly with linear model if we select same number of "ncomp" 
# in both pcr and lmod. Moreover, the interpretability of the model is of haywire. 

############## PCR - Registered ##################################

pcr.mod=pcr(registered~  temp + hum + windspeed + factor(season) + factor(workingday) + factor(weathersit),data=bikes_registered,scale=T,validation="CV")
summary(pcr.mod)
validationplot(pcr.mod,val.type="MSEP")

# plotfitted values for OLS and PCR, compare with actual
lmod=lm(registered~  temp + hum + windspeed + factor(season) + factor(workingday) + factor(weathersit)  ,data=bikes_registered)
fit.pcr=predict(pcr.mod,data=bikes_registered,ncomp=7)
plot(lmod$fitted.values,bikes_registered$registered,pch=19,col="blue")
points(fit.pcr,bikes_registered$registered,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.pcr=cor(fit.pcr,bikes_registered$registered)^2
R2.lm=cor(lmod$fitted.values,bikes_registered$registered)^2
R2.pcr
R2.lm

# Observation : The data matches exactly with linear model if we select same number of "ncomp" 
# in both pcr and lmod. Moreover, the interpretability of the model is of haywire. 

################### Random Forest - Casual #############################


library(randomForest)
train_df = bikes_casual[1:547, ]
test_df = bikes_casual[547:nrow(bikes_casual), ]

# Create a Random Forest model
rf_model = randomForest(casual ~ temp+ workingday + weathersit + hum + windspeed, data = train_df, ntree = 100)
print(rf_model)

# Predicting on test set
predTrain = predict(rf_model, test_df, type = "class")

# Visualizing the Random Forest Plot
plot(bikes_casual$casual, type = "l", col = "red", xlab = "Day", ylab = "Number of Casual Bike Users", main = "Random Forest Plot for Bike Users")
legend("topleft", c("Actual", "Estimated"), lty = c(1, 1), col = c("red", "blue"))
lines(547:nrow(bikes_casual),predTrain, type = "l", col = "blue")
rm(predTrain, rf_model)


################### Random Forest - Registered #############################

train_df = bikes_registered[1:547, ]
test_df = bikes_registered[547:nrow(bikes_registered), ]

# Create a Random Forest model
rf_model = randomForest(registered ~ temp+ workingday + weathersit + hum + windspeed, data = train_df, ntree = 1000)
print(rf_model)

# Predicting on test set
predTrain = predict(rf_model, test_df, type = "class")

# Visualizing the Random Forest Plot
plot(bikes_registered$registered, type = "l", col = "red", xlab = "Day", ylab = "Number of Registered Bike Users", main = "Random Forest Plot for Bike Users")
legend("topleft", c("Actual", "Estimated"), lty = c(1, 1), col = c("red", "blue"))
lines(547:nrow(bikes_registered),predTrain, type = "l", col = "blue")
rm(predTrain, rf_model)

#################################################################################