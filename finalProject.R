library(xts)
library(forecast)
library(astsa)
library(aTSA)
library(zoo)
library(stats)
#initial analysis of the data (plots, statistical analysis, etc..)
projectData<-read.csv("input_data_team_9.csv")
#projectData<-read.csv("input_data_team_9_processed.csv")
torontoData<-data.frame(Toronto=projectData[,8])
torontoData<-torontoData[-c(1:5880),]
torontoData.ts<-ts(torontoData)
plot(torontoData.ts,xlab="xth hour",ylab="Electricity Demand",main="Hourly Eletricity Demand in Toronto")


projectData<-read.csv("input_data_team_9_processed.csv")
torontoData<-data.frame(Toronto=projectData[,8])
torontoData<-torontoData[-c(1:5880),]
torontoData.ts<-ts(torontoData)
plot(torontoData.ts,xlab="xth hour",ylab="Electricity Demand",main="Hourly Eletricity Demand in Toronto - Adjusted")

acf(torontoData.ts, main="ACF of Hourly Eletricity Demand in Toronto - Adjusted")
find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}
find.freq(torontoData.ts)
torontoData.ts<-ts(torontoData,frequency=24)
boxplot(torontoData.ts~cycle(torontoData.ts))


#plot(decompose(torontoData.ts))
#torontoData.ts.day<-aggregate(torontoData.ts,nfrequency=1)
#plot(torontoData.ts.day)
par(mfrow=c(2,2))
plot(torontoData.ts,main="Original Data")
plot(torontoData.ts,col="gray",main="1 Day Moving Average")
lines(ma(torontoData.ts,order=24),col="red",lwd=1)
plot(torontoData.ts,col="gray",main="1 Week Moving Average")
lines(ma(torontoData.ts,order=168),col="blue",lwd=1)
plot(torontoData.ts,col="gray",main="1 Year Moving Average")
lines(ma(torontoData.ts,order=8766),col="green",lwd=1)



#par(mfrow = c(2,2))
#plot(torontoData.ts, main = "Original Data")
#plot(diff(torontoData.ts,1), main = "Detrended data")
#plot(diff(torontoData.ts, 24), main = "Seasonally adjusted data")
#plot (diff(diff(torontoData.ts,1), 24), main = "Seasonally adj. and detrended data")

#statiscal analysis
#plot(decompose(torontoData.ts))
#plot(stl(torontoData.ts,s.window="periodic"))

mean(torontoData.ts)
var(torontoData.ts)



#?????summary(auto.arima(torontoData.ts,approximation = FALSE))
#?????acf(torontoData.ts)
#?????acf(diff(torontoData.ts))
#?????acf(diff(diff(torontoData.ts),24))
#?????acf(diff(diff(diff(torontoData.ts),24),168))
torontoData.ts<-ts(torontoData,start=c(1,1),end=c(5359,24),frequency=24)


#Dividing the data to a training set and a test set

View(torontoData.ts)
training<-window(torontoData.ts,end=c(4383,24))
testing<-window(torontoData.ts,start=c(4384,1))

#Linear Regression
#View(training)
trainingTslm<-tslm(training~trend+season)
summary(trainingTslm)
#plot(torontoData.ts,col="gray",main="Training Data")
#lines(traingTslm,col="red",lwd=1)
#abline(trainingTslm,col="red")
#plot(trainingTslm$residuals)

summary(trainingTslm$residuals)
qqnorm(trainingTslm$residuals)
qqline(trainingTslm$residuals)
hist(trainingTslm$residuals)

acf(trainingTslm$residuals)


#Smothing method

#SES
trainingSES<-ses(training,initial="optimal")
summary(trainingSES)
summary(trainingSES$residuals)

qqnorm(trainingSES$residuals,main="Normal QQ Plot for the SES Model")
qqline(trainingSES$residuals)
hist(trainingSES$residuals, main="Histogram for SES Model")
acf(trainingSES$residuals, main="ACF for SES Model")

# We need to cut a very small porportion of it to emplify the effect
#plot(training,col="gray",main="SES Model")
#lines(trainingSES$fitted,col="blue",lwd=1,lyt)

#Holt
trainingHolt<-holt(training,initial="optimal")
summary(trainingHolt)
summary(trainingHolt$residuals)
qqnorm(trainingHolt$residuals,main="Notmal QQ Plot for HOLT Model")
qqline(trainingHolt$residuals)
hist(trainingHolt$residuals,main="Histogram for HOLT Model")
acf(trainingHolt$residuals, main="ACF for HOLT Model")


#HW
trainingHW<-hw(training,initial="optimal")
summary(trainingHW)
summary(trainingHW$residuals)
qqnorm(trainingHW$residuals, main="Notmal QQ Plot for HW Model")
qqline(trainingHW$residuals)
hist(trainingHW$residuals, main="Histogram for HW Model")
acf(trainingHW$residuals, main="ACF for HW Model")


#ETS MODEL
trainingETS<-ets(training)
summary(trainingETS)
summary(trainingETS$residuals)
qqnorm(trainingETS$residuals, main="Notmal QQ Plot for ETS Model")
qqline(trainingETS$residuals)
hist(trainingETS$residuals, main="Histogram for ETS Model")
acf(trainingETS$residuals, main="ACF for ETS Model")


#ARIMA MODEL


plot(stl(training,s.window='periodic'))
acf(stl(training,s.window='periodic'))

pacf(training)

plot(decompose(diff(diff(training,24),1)))


pacf(diff(diff(diff(training),24),33))

acf(diff(diff(training,24)))
acf(diff(training,24))
acf(diff(training))


acf(diff(seasadj(stl(training,s.window='periodic'))))

find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}
find.freq(diff(seasadj(stl(training,s.window='periodic'))))
find.freq(diff(diff(training,24)))
find.freq(diff(diff(diff(training,24)),33))

pacf(diff(seasadj(stl(training,s.window='periodic'))))

find.arima<-function(x)
{
  p1=p2=d1=d2=q1=q2=0
  for(p1 in 0:5)
  {
    for(d1 in 0:5)
    {
      for(q1 in 0:5)
      {
        for(p2 in 0:5)
        {
          for(d2 in 0:5)
          {
            for(q2 in 0:5)
            {
              
              print(c(p1,d1,q1))
              print(c(p2,d2,q2))
              findArima<-arima(x,order=c(p1,d1,q1),seasonal=c(p2,d2,q2))
              #forecastedArima<-forecast(findArima,lead=24)
              
              print(accuracy((forecast(findArima,lead=24))[,2],milk.test)[1,"RMSE"])
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
}



trainingARIMA<-auto.arima(training, approximation = FALSE)
summary(trainingARIMA)


summary(trainingARIMA$residuals)
qqnorm(trainingARIMA$residuals, main="Notmal QQ Plot for autoARIMA Model")
qqline(trainingARIMA$residuals)
hist(trainingARIMA$residuals, main="Histogram for autoARIMA Model")
acf(trainingARIMA$residuals, main="ACF for autoARIMA Model")




forecastedTslm<-forecast(trainingTslm,h=17544)
accuracy(forecastedTslm, testing)

forecastedSES<-ses(training,initial="optimal",h=17544)
accuracy(forecastedSES,testing)

forecastHolt<-holt(training,initial="optimal", h=17544)
accuracy(forecastedHolt, testing)

forecastedHW<-hw(training, initial = "optimal", h=17544)
accuracy(forecastedHW,testing)

forecastedETS<-ets(training,h=17544)
accuracy(forecastedETS,testing)

forecastedARIMA<-forecast(trainingARIMA,h=17544)
accuracy(forecastedARIMA,testing)

forecastedARIMA<-forecast(trainingARIMA,h=17544)
accuracy(forecastedARIMA,testing)

fore








#Model Selection and Fitting: a model (or several models) to be used to forecast future data

find.arima<-function(x)
{
  p1=p2=d1=d2=q1=q2=0
  RMSE1=i=1
  RMSE1<-data.frame(p1,d1,q1,p2,d2,q2,RMSE1)
  for(p1 in 0:5)
  {
    for(d1 in 0:5)
    {
      for(q1 in 0:5)
      {
        for(p2 in 0:5)
        {
          for(d2 in 0:5)
          {
            for(q2 in 0:5)
            {
              result = tryCatch({
                # write your intended code here
                findArima<-arima(x,order=c(p1,d1,q1),seasonal=list(order=c(p2,d2,q2),peroid=frequency(milk.train)),method="ML")
                RMSE1[i,'p1']<-p1
                RMSE1[i,'d1']<-d1
                RMSE1[i,'q1']<-q1
                RMSE1[i,'p2']<-p2
                RMSE1[i,'d2']<-d2
                RMSE1[i,'q2']<-q2
                RMSE1[i,'RMSE']<-accuracy((forecast(findArima,lead=24))[,2],milk.test)[1,"RMSE"]
                i<-i+1
              }, warning = function(w) {
                # log the warning or take other action here
              }, error = function(e) {
                # log the error or take other action here
              }, finally = {
                # this will execute no matter what else happened
              })
              print(c(p1,d1,q1))
              print(c(p2,d2,q2))
              #findArima<-arima(x,order=c(p1,d1,q1),seasonal=c(p2,d2,q2),method="ML")
              #print(accuracy((forecast(findArima,lead=24))[,2],milk.test)[1,"RMSE"])
            }
          }
        }
      }
    }
  }
}
