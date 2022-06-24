########################### Project 1 ########################

# Load data 

bikes = read.csv("bikes.csv")
View(bikes)

# Remove instant column
bikes = subset(bikes,select = -c(instant))
View(bikes)

#Bifurcating data for registered and casual riders
bikes_casual = subset(bikes,select = -c(registered))
bikes_registered = subset(bikes,select = -c(casual))

#generating interaction terms

temp2=temp^2
hum2=hum^2
windspeed2=windspeed^2
temp_hum=temp*hum
temp_wdsp=temp*windspeed
hum_wdsp=hum*windspeed


#Complete Second Order Model on Casual riders

lm.fit2=lm(casual ~ temp+hum+windspeed+temp2+hum2+windspeed2+temp_wdsp+temp_hum+hum_wdsp, data = bikes)

#Complete Second Order Model on Registered riders

lm.fit3=lm(registered ~ temp+hum+windspeed+temp2+hum2+windspeed2+temp_wdsp+temp_hum+hum_wdsp, data = bikes)

#Variable Centering

temp_ctr=scale(bikes$temp,scale = F)
hum_ctr=scale(bikes$hum,scale = F)
wdsp_ctr=scale(bikes$windspeed,scale = F)
bikes_casual_ctr=data.frame(cbind(bikes_casual, temp_ctr^2, hum_ctr^2, wdsp_ctr^2, temp_ctr*hum_ctr, temp_ctr*wdsp_ctr, hum_ctr*wdsp_ctr))
colnames(bikes_casual_ctr)=c("dteday","season","yr","workingday","weathersit", "temp","hum","windspeed","casual", "tempctr2","humctr2","wdspctr2","temphumctr","tempwdspctr","humwdspctr")
fix(bikes_casual_ctr)
bikes_cas_ctr=bikes_casual_ctr[,-c(1:5)]
lm.fit4=lm(casual~.,bikes_cas_ctr)
