########################### Mid-Term Project ########################
library(ISLR)
require(leaps)
require(glmnet)
require(faraway)
require(dplyr)
require(pls)
library(randomForest)
library(tidyverse)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
library(forecast)
library(zoo)
library(tseries)
library(corrplot)
library(e1071)
library(pander)
library(ggplot2)

# Load data 
setwd("F:/MS-OR/Semester_3/ST516_Experimental_Statistics/Exams/Mid-Term/")

bikes = read.csv("bikes.csv")
View(bikes)

# Remove instant column
bikes = subset(bikes,select = -c(instant,dteday))

### CONTINUOUS VARIABLES CORRELATION
bikes_cont = subset(bikes,select = -c(season,yr,workingday,weathersit,casual,registered)) # Continuous Variable data frame
bikes_cont_reg = subset(bikes,select = -c(season,yr,workingday,weathersit,casual)) # Continuous Variables w/ registered riders
cor(bikes_cont_reg) # Registered riders Correlation Matrix
bikes_cont_cas = subset(bikes,select = -c(season,yr,workingday,weathersit,registered)) # Continuous Variables with Casual Riders
cor(bikes_cont_cas) # Casual riders Correlation Matrix

#Bifurcating data for registered and casual riders
bikes_casual = subset(bikes,select = -c(registered))
bikes_registered = subset(bikes,select = -c(casual))

View(bikes_casual)
View(bikes_registered)

##################### Linear model - Casual #######################
summary(lm(casual ~ factor(season) + factor(yr) + factor(workingday) + factor(weathersit) 
                    + temp + hum + windspeed, data = bikes_casual))

predict_casual_lm = predict(lm(casual ~ factor(season) + factor(yr) + factor(workingday) + factor(weathersit) 
   + temp + hum + windspeed, data = bikes_casual))
mse_casual = mean((bikes_casual$casual-predict_casual_lm)^2)
print(mse_casual)

##################### Linear model - Registered #######################
summary(lm(registered ~ factor(season) + factor(yr) + factor(workingday) + factor(weathersit) 
           + temp + hum + windspeed, data = bikes_registered))

predict_registered_lm = predict(lm(registered ~ factor(season) + factor(yr) + factor(workingday) + factor(weathersit) 
                               + temp + hum + windspeed, data = bikes_registered))
mse_registered = mean((bikes_registered$registered-predict_registered_lm)^2)
print(mse_registered)

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
folds=sample(1:k,nrow(bikes_casual),replace=T) 
# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,10,dimnames=list(NULL,paste(1:10)))

# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-6 predictors fit without kth fold
  best.mods=regsubsets(casual ~ factor(season) + factor(yr) + factor(workingday) + factor(weathersit) 
                       + temp + hum + windspeed ,data=bikes_casual[folds!=j,],
                       nvmax=10,method="exhaustive")
  # estimate test error for all 7 models by predicting kth fold 
  for (i in 1:10){
    pred=pred.sbs(best.mods,bikes_casual[folds==j,],id=i)
    cv.err[j,i]=mean((bikes_casual$casual[folds==j]-pred)^2)  # save error est
  }
}



mse.cv=apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE

## Summary of the model
casual_sum = summary(best.mods)
casual_sum$adjr2[min]

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:10,mse.cv,type="b",xlab="no. of predictors",ylab="est. test MSE",ylim=c(50000,350000),main = "Casual Riders")
points(min,mse.cv[min],cex=2,col="red",lwd=2)

###################### K-Folds - Registered #############################
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:5,nrow(bikes_registered),replace=T) 
# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,10,dimnames=list(NULL,paste(1:10)))

# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-7 predictors fit without kth fold
  best.mods=regsubsets(registered ~ factor(season) + factor(yr) + factor(workingday) + factor(weathersit) 
                       + temp + hum + windspeed ,data=bikes_registered[folds!=j,],
                       nvmax=10,method="exhaustive")
  # estimate test error for all 10 models by predicting kth fold 
  for (i in 1:10){
    pred=pred.sbs(best.mods,bikes_registered[folds==j,],id=i)
    cv.err[j,i]=mean((bikes_registered$registered[folds==j]-pred)^2)  # save error est
  }
}

mse.cv=apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE

## Summary of the model
registered_sum = summary(best.mods)
registered_sum$adjr2[min]

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:10,mse.cv,type="b",xlab="no. of predictors",ylab="est. test MSE",ylim=c(450000,1600000),main = "Registered Riders")
points(min,mse.cv[min],cex=2,col="red",lwd=2)

############## PCR - Casual ##################################

pcr.mod=pcr(casual~  temp + hum + windspeed + factor(yr) +  factor(season) + factor(workingday) + factor(weathersit),data=bikes_casual,scale=T,validation="CV")
summary(pcr.mod)
validationplot(pcr.mod,val.type="MSEP")

# plotfitted values for OLS and PCR, compare with actual
lmod=lm(casual~  temp + hum + windspeed + factor(yr) + factor(season) + factor(workingday) + factor(weathersit)  ,data=bikes_casual)
fit.pcr=predict(pcr.mod,data=bikes_casual,ncomp=10)
plot(lmod$fitted.values,bikes_casual$casual,pch=19,col="blue")
points(fit.pcr,bikes_casual$casual,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.pcrmse = mean((fit.pcr-bikes_casual$casual)^2)
R2.pcr=cor(fit.pcr,bikes_casual$casual)^2
R2.lm=cor(lmod$fitted.values,bikes_casual$casual)^2
R2.pcr
R2.pcrmse

# Observation : The data matches exactly with linear model if we select same number of "ncomp" 
# in both pcr and lmod. Moreover, the interpretability of the model is of haywire. 

############## PCR - Registered ##################################

pcr.mod=pcr(registered~  temp + hum + windspeed + factor(yr) + factor(season) + factor(workingday) + factor(weathersit),data=bikes_registered,scale=T,validation="CV")
summary(pcr.mod)
validationplot(pcr.mod,val.type="MSEP")

# plotfitted values for OLS and PCR, compare with actual
lmod=lm(registered~  temp + hum + windspeed + factor(yr) + factor(season) + factor(workingday) + factor(weathersit)  ,data=bikes_registered)
fit.pcr=predict(pcr.mod,data=bikes_registered,ncomp=10)
plot(lmod$fitted.values,bikes_registered$registered,pch=19,col="blue")
points(fit.pcr,bikes_registered$registered,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.pcr=cor(fit.pcr,bikes_registered$registered)^2
R2.lm=cor(lmod$fitted.values,bikes_registered$registered)^2
R2.pcrmse = mean((fit.pcr-bikes_registered$registered)^2)
R2.pcr
R2.pcrmse

# Observation : The data matches exactly with linear model if we select same number of "ncomp" 
# in both pcr and lmod. Moreover, the interpretability of the model is of haywire. 

################### Random Forest - Casual #############################

rf_model = randomForest(casual ~ temp+ hum + windspeed, data = bikes_casual, ntree = 1000)
print(rf_model)

# Predicting on test set
predTrain = predict(rf_model, bikes_casual, type = "class")

# Visualizing the Random Forest Plot
plot(bikes_casual$casual, type = "l", col = "red", xlab = "Day", ylab = "Number of Registered Bike Users", main = "Random Forest Plot for Bike Users")
legend("topleft", c("Actual", "Estimated"), lty = c(1, 1), col = c("red", "blue"))
lines(1:nrow(bikes_casual),predTrain, type = "l", col = "blue")
rm(predTrain, rf_model)

################### Random Forest - Registered #############################

# Create a Random Forest model

rf_model = randomForest(registered ~ temp+ hum + windspeed, data = bikes_registered, ntree = 1000)
print(rf_model)

# Predicting on test set
predTrain = predict(rf_model, bikes_registered, type = "class")

# Visualizing the Random Forest Plot
plot(bikes_registered$registered, type = "l", col = "red", xlab = "Day", ylab = "Number of Registered Bike Users", main = "Random Forest Plot for Bike Users")
legend("topleft", c("Actual", "Estimated"), lty = c(1, 1), col = c("red", "blue"))
lines(1:nrow(bikes_registered),predTrain, type = "l", col = "blue")
rm(predTrain, rf_model)

#################################################################################
################################ Ridge - Casual riders ##########################
x=model.matrix(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_casual)[,-1]
y=bikes_casual$casual

coef(glmnet(x,y,alpha=0,lambda=0)) # alpha=0 performs ridge regression

grid=10^seq(-5,4,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
set.seed(123)
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)

ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef

 
fit.ridge=predict(ridge.mod,s=bestlam.r,x)
R2.ridge=cor(fit.ridge,bikes_casual$casual)^2
R2.ridge
mse.r


################################ Ridge - Registered riders ##########################
x=model.matrix(registered~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_registered)[,-1]
y=bikes_registered$registered

coef(glmnet(x,y,alpha=0,lambda=0)) # alpha=0 performs ridge regression

grid=10^seq(-5,4,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) 

set.seed(123)
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)

ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef


fit.ridge=predict(ridge.mod,s=bestlam.r,x)
R2.ridge=cor(fit.ridge,bikes_registered$registered)^2
R2.ridge
mse.r

################################ Lasso - Casual riders ##########################
x=model.matrix(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_casual)[,-1]
y=bikes_casual$casual

coef(glmnet(x,y,alpha=1,lambda=0)) # alpha=1 performs lasso

# create grid for lambda, fit model using all lambdas
grid=10^seq(-5,4,length=100)  
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef

# compare R2 for each fit
fit.lasso=predict(lasso.mod,s=bestlam.l,x)
R2.lasso=cor(fit.lasso,bikes_casual$casual)^2
R2.lasso
mse.l
################################ Lasso - Registered riders ##########################
x=model.matrix(registered~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_registered)[,-1]
y=bikes_registered$registered

coef(glmnet(x,y,alpha=1,lambda=0)) # alpha=1 performs lasso

# create grid for lambda, fit model using all lambdas
grid=10^seq(-5,4,length=100)  
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef

# compare R2 for each fit
fit.lasso=predict(lasso.mod,s=bestlam.l,x)
R2.lasso=cor(fit.lasso,bikes_registered$registered)^2
R2.lasso
mse.l
########################## Various Plots ########################################
bikes = read.csv("bikes.csv")
#View(bikes)

# Remove instant column
bikes = subset(bikes,select = -c(instant))
#View(bikes)

#Bifurcating data for registered and casual riders
bikes_casual = subset(bikes,select = -c(registered))
bikes_registered = subset(bikes,select = -c(casual))


#Creating new data table for total riders count
cnt=bikes$casual+bikes$registered
bikes_count=data.frame(cbind(bikes, cnt))
colnames(bikes_count)=c("dteday","season","yr","workingday", "weathersit", "temp", "hum","windspeed","casual","registered","cnt")


################# Year Plot ##########################################
#Plotting the graph of yearly ridership
df<-bikes_count%>%group_by(yr)%>%summarise(bikes_cas_yr= sum(casual), bikes_reg_yr= sum(registered))
df<-as.data.frame(df)
df$yr<-as.character(df$yr)
dfm<-melt(df[,c("yr","bikes_cas_yr","bikes_reg_yr")])
point <- format_format(big.mark = ",", scientific = FALSE)

#Box Plot
ggplot(dfm,aes(x = yr,y = value, fill=yr)) + labs(title="               Yearly casual and registered ridership behavior ",x="Year",y="Ridership Count")+
  geom_boxplot(stat = "boxplot", position = "dodge")+ scale_y_continuous(labels = point) +scale_fill_brewer(palette="Dark2")

######################## Working day Plot ############################
#plotting the graph of ridership based on workingday
dfwd<-bikes_count%>%group_by(workingday)%>%summarise(cas_riders= sum(casual), reg_riders= sum(registered))
dfwd<-as.data.frame(dfwd)
dfwd$workingday<-as.character(dfwd$workingday)
dfmwd<-melt(dfwd[,c("workingday","cas_riders","reg_riders")])
point <- format_format(big.mark = ",", scientific = FALSE)

ggplot(dfmwd, aes(x=workingday, y= value))+labs(x = "Working Day", y = "Ridership Count",title="       Casual and Registered riders based on working day")+
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_continuous(labels = point)


######################### Season plot ###############################
dfsr<-bikes_count%>%group_by(season)%>%summarise(bikes_casual= sum(casual), bikes_registered= sum(registered))
dfsr<-as.data.frame(dfsr)
dfsr$season<-as.character(dfsr$season)
dfmsr<-melt(dfsr[,c("season","bikes_casual","bikes_registered")])
point <- format_format(big.mark = ",", scientific = FALSE)


#seasonal riders boxplot
ggplot(dfmsr,aes(x = season,y = value, fill=season)) + labs(title=" Rider count based on seasons ")+
  geom_boxplot(stat = "boxplot", position = "dodge")+ scale_y_continuous(labels = point)


#Seasonal Riders bar plot
ggplot(dfmsr, aes(x= season, y= value))+labs(x = "Season",y="Ridership Count",title="           Rider count based on seasons ")+
  geom_bar(aes(fill=variable),stat = "identity", position="dodge") + scale_y_continuous(labels = point)+scale_fill_brewer(palette="Set1")
#########################################################################

