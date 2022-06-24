########################### Project 1 #############################

# Load data 
setwd("F:/MS-OR/Semester_3/ST516_Experimental_Statistics/Exams/Mid-Term/")

bikes = read.csv("bikes.csv")
View(bikes)

# Remove instant column
bikes = subset(bikes,select = -c(instant))
View(bikes)

#Bifurcating data for registered and casual riders
bikes_casual = subset(bikes,select = -c(registered, dteday, yr))
bikes_registered = subset(bikes,select = -c(casual, dteday, yr))

bikes_casual$season=as.factor(bikes_casual$season) # Convert season to categorical variable
bikes_casual$weathersit=as.factor(bikes_casual$weathersit) # Read Weather situation as categorical variable
# Repeat same for registered riders
bikes_registered$season=as.factor(bikes_registered$season)
bikes_registered$weathersit=as.factor(bikes_registered$weathersit)

hist(bikes_casual$temp)
plot(casual~temp)
plot(bikes_casual$casual~bikes_casual$hum)

plot(temp~hum)
cor(temp,hum)

plot(temp~windspeed)
cor(temp,windspeed)

plot(windspeed~hum)
cor(windspeed,hum)

pairs(~temp+hum+windspeed,data = bikes_casual)




#############################################################
bikes_casual_1 = subset(bikes,select = -c(dteday,registered))
View(bikes_casual_1)
is.factor(bikes_casual$season) # Check if season is a categorical variable
  ## OUTPUT
  ## is.factor(bikes_casual$season)
  ## [1] FALSE


is.factor(bikes_casual$workingday) # Check again 

is.factor(bikes_casual$dteday)

is.factor(bikes_casual$weathersit)

class(bikes$yr)
is.factor(bikes$yr)
class(bikes$workingday)
is.factor(bikes$workingday)
#remove yr # Histogram for temp, hum, weathersit and riders
bikes_casual$season=as.factor(bikes_casual$season)


cor(bikes_casual_1)
bikes_registered_1 = subset(bikes,select = -c(dteday,casual))

Season_1=split(bikes_casual_1, bikes_casual_1$season)