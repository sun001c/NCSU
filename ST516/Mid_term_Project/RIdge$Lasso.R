require(ISLR)
require(glmnet)
require(faraway)
require(dplyr)
require(leaps)

# Load data 
bikes = read.csv("bikes.csv")
View(bikes)

# Remove instant column
bikes = subset(bikes,select = -c(instant))
View(bikes)

#Bifurcating data for registered and casual riders
bikes_casual = subset(bikes,select = -c(registered, dteday))
bikes_registered = subset(bikes,select = -c(casual, dteday))

###########casual######
#####ridge##########

x=model.matrix(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_casual)[,-1]
y=bikes_casual$casual
View(bikes_casual)

lmod=lm(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_casual)
coef(glmnet(x,y,alpha=0,lambda=0)) # alpha=0 performs ridge regression
summary(lmod)

# compare lambda=100 to OLS
summary(lm(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_casual))
coef(glmnet(x,y,alpha=0,lambda=100))

# create grid for lambda, fit model using all lambdas
grid=10^seq(-5,4,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) 

# plot coefficent values as we change lambda
plot(ridge.mod,xlab="L2 Norm")  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

set.seed(123)
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)
bestlam.r
mse.r

# get coefficents for best model and compare to OLS
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef
summary(lm(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_casual))

# plotfitted values for OLS and Ridge, compare with actual with actual
fit.ridge=predict(ridge.mod,s=bestlam.r,x)
plot(lmod$fitted.values,bikes_casual$casual,pch=19,col="blue")
points(fit.ridge,bikes_casual$casual,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.ridge=cor(fit.ridge,bikes_casual$casual)^2
R2.lm=cor(lmod$fitted.values,bikes_casual$casual)^2
R2.ridge



################lasso################
#########casual##########

x=model.matrix(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_casual)[,-1]
y=bikes_casual$casual

# compare lambda=0 to OLS
# Note: glmnet standardizes coefficient for fitting, but converts estimates back to original scale
lmod=lm(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_casual)
coef(glmnet(x,y,alpha=1,lambda=0)) # alpha=1 performs lasso
summary(lmod)

# create grid for lambda, fit model using all lambdas
grid=10^seq(-5,4,length=100)  
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  
# check coefficent values for each value of lambda
plot(lasso.mod)  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)
bestlam.l
mse.l

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef
summary(lmod)

# plotfitted values for OLS and Ridge, compare with actual with actual
fit.lasso=predict(lasso.mod,s=bestlam.l,x)
plot(lmod$fitted.values,bikes_casual$casual,pch=19,col="blue")
points(fit.lasso,bikes_casual$casual,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.lasso=cor(fit.lasso,bikes_casual$casual)^2
R2.lm=cor(lmod$fitted.values,bikes_casual$casual)^2
R2.lasso
R2.lm


###############registered############
########ridge############

x=model.matrix(registered~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_registered)[,-1]
y=bikes_registered$registered

# compare lambda=0 to OLS
# Note: glmnet standardizes coefficient for fitting, but converts estimates back to original scale
lmod=lm(registered~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_registered)
coef(glmnet(x,y,alpha=0,lambda=0)) # alpha=0 performs ridge regression
summary(lmod)

# compare lambda=100 to OLS
summary(lm(registered~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_registered))
coef(glmnet(x,y,alpha=0,lambda=100))

# create grid for lambda, fit model using all lambdas
grid=10^seq(-5,3,length=100) 
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)  

# plot coefficent values as we change lambda
plot(ridge.mod,xlab="L2 Norm")  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)
bestlam.r
mse.r

# get coefficents for best model and compare to OLS
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef
summary(lm(registered~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_registered))

# plotfitted values for OLS and Ridge, compare with actual with actual
fit.ridge=predict(ridge.mod,s=bestlam.r,x)
plot(lmod$fitted.values,bikes_registered$registered,pch=19,col="blue")
points(fit.ridge,bikes_registered$registered,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.ridge=cor(fit.ridge,bikes_registered$registered)^2
R2.lm=cor(lmod$fitted.values,bikes_registered$registered)^2
R2.ridge
R2.lm


#############registered#########
########lasso#########

x=model.matrix(registered~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,bikes_registered)[,-1]
y=bikes_registered$registered

# compare lambda=0 to OLS
# Note: glmnet standardizes coefficient for fitting, but converts estimates back to original scale
lmod=lm(casual~factor(season) + factor(yr) + factor(workingday) + factor(weathersit) + temp + hum + windspeed,data=bikes_registered)
coef(glmnet(x,y,alpha=1,lambda=0)) # alpha=1 performs lasso
summary(lmod)

# create grid for lambda, fit model using all lambdas
grid=10^seq(-5,4,length=100)  
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  
# check coefficent values for each value of lambda
plot(lasso.mod)  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)
bestlam.l
mse.l

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef
summary(lmod)

# plotfitted values for OLS and Ridge, compare with actual with actual
fit.lasso=predict(lasso.mod,s=bestlam.l,x)
plot(lmod$fitted.values,bikes_registered$registered,pch=19,col="blue")
points(fit.lasso,bikes_registered$registered,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.lasso=cor(fit.lasso,bikes_registered$registered)^2
R2.lm=cor(lmod$fitted.values,bikes_registered$registered)^2
R2.lasso
R2.lm





