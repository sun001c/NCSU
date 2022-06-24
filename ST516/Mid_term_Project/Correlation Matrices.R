# CONVERT VARIABLES TO CATEGORICAL

# Casual
bikes_casual$season=as.factor(bikes_casual$season) # Convert season to categorical variable
bikes_casual$weathersit=as.factor(bikes_casual$weathersit) # Read Weather situation as categorical variable
bikes_casual$yr=as.factor(bikes_casual$yr) # Convert class of yr to factor
bikes_casual$workingday=as.factor(bikes_casual$workingday)

# Registered
bikes_registered$season=as.factor(bikes_registered$season)
bikes_registered$weathersit=as.factor(bikes_registered$weathersit)
bikes_registered$yr=as.factor(bikes_registered$yr)
bikes_registered$workingday=as.factor(bikes_registered$workingday)

### CONTINUOUS VARIABLES CORRELATION
bikes_cont = subset(bikes,select = -c(dteday,season,yr,workingday,weathersit,casual,registered)) # Continuous Variable data frame
bikes_cont_reg = subset(bikes,select = -c(dteday,season,yr,workingday,weathersit,casual)) # Continuous Variables w/ registered riders
cor(bikes_cont_reg) # Registered riders Correlation Matrix
bikes_cont_cas = subset(bikes,select = -c(dteday,season,yr,workingday,weathersit,registered)) # Continuous Variables with Casual Riders
cor(bikes_cont_cas) # Casual riders Correlation Matrix
