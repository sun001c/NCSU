library(dplyr)
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

########################### Project 1 ########################
setwd("D:/NCSU/Fall 2019/ST 516/Midterm Project")

# Load data 

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


#Plotting the graph of yearly ridership
df<-bikes_count%>%group_by(yr)%>%summarise(bikes_casual= sum(casual), bikes_registered= sum(registered))
df<-as.data.frame(df)
df$yr<-as.character(df$yr)
dfm<-melt(df[,c("yr","bikes_casual","bikes_registered")])
point <- format_format(big.mark = ",", scientific = FALSE)

#Box Plot
ggplot(dfm,aes(x = yr,y = value, fill=yr)) + labs(title=" Yearly casual and registered ridership ")+
  geom_boxplot(stat = "boxplot", position = "dodge")+ scale_y_continuous(labels = point)

#barplot
ggplot(dfm, aes(x=yr, y= value))+labs(title="Yearly casual and registered ridership ")+
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_continuous(labels = point)



#seasonal riders data frame
dfsr<-bikes_count%>%group_by(season)%>%summarise(bikes_casual= sum(casual), bikes_registered= sum(registered))
dfsr<-as.data.frame(dfsr)
dfsr$season<-as.character(dfsr$season)
dfmsr<-melt(dfsr[,c("season","bikes_casual","bikes_registered")])
point <- format_format(big.mark = ",", scientific = FALSE)


#seasonal riders boxplot
ggplot(dfmsr,aes(x = season,y = value, fill=season)) + labs(title=" Rider count based on seasons ")+
geom_boxplot(stat = "boxplot", position = "dodge")+ scale_y_continuous(labels = point)


#Seasonal Riders bar plot
ggplot(dfmsr, aes(x= season, y= value))+labs(title="Rider count based on seasons ")+
geom_bar(aes(fill=variable),stat = "identity", position="dodge") + scale_y_continuous(labels = point)




#plotting the graph of ridership based on workingday
dfwd<-bikes_count%>%group_by(workingday)%>%summarise(bikes_casual= sum(casual), bikes_registered= sum(registered))
dfwd<-as.data.frame(dfwd)
dfwd$workingday<-as.character(dfwd$workingday)
dfmwd<-melt(dfwd[,c("workingday","bikes_casual","bikes_registered")])
point <- format_format(big.mark = ",", scientific = FALSE)

#boxplot
ggplot(dfmwd,aes(x = workingday,y = value, fill=workingday)) + labs(title="  Casual and Registered riders based on working day ")+
geom_boxplot(stat = "boxplot", position = "dodge")+ scale_y_continuous(labels = point)

#barplot
ggplot(dfmwd, aes(x=workingday, y= value))+labs(title=" Casual and Registered riders based on working day ")+
geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + scale_y_continuous(labels = point)



