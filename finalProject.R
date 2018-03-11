library(xts)
library(forecast)
library(astsa)
library(aTSA)
library(zoo)
library(stats)
library(anytime)
#•••Initial analysis of the data (plots, statistical analysis, etc..)
projectData<-read.csv("input_data_team_9.csv")
#projectData<-read.csv("input_data_team_9_processed.csv")
torontoData<-data.frame(Date=projectData[,1],Hour=projectData[,2],Toronto=projectData[,8])
torontoData.ts<-torontoData[-c(1:5880),]
#torontoDataDate<-torontoData[,1]
torontoData.ts[,3]<-ts(torontoData.ts[,3])
#torontoData.ts[,1]<-anytime::anydate(torontoData.ts[,1])
#head(torontoData.ts)
par(mfrow=c(1,1))
plot(torontoData.ts[,3],xlab="xth hour",ylab="Electricity Demand",main="Hourly Eletricity Demand in Toronto")
#Substitute outliers with value of 0 using moving median method.
for(index in 1:nrow(torontoData.ts))
{
  if(torontoData.ts[index,3]==0)
  {
    torontoData.ts[index,3]<-median(torontoData.ts[(index-24):(index-1),3])
    print(index)
  }
}
#View(torontoData.ts)
#for(index in 1:length(torontoData.ts))
#{
#  if(torontoData.ts[index]==0)
#  {
#    print(index)
#  }
#}
#View(torontoData.ts)
#projectData<-read.csv("input_data_team_9_processed.csv")
#torontoData<-data.frame(Toronto=projectData[,8])
#torontoData<-torontoData[-c(1:5880),]
#torontoData.ts<-ts(torontoData)
#plot(torontoData.ts,xlab="xth hour",ylab="Electricity Demand",main="Hourly Eletricity Demand in Toronto - Adjusted")
#ts(torontoData[,2])
acf(torontoData.ts[,3], main="ACF of Hourly Eletricity Demand in Toronto - Adjusted")
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
find.freq(torontoData.ts[,3])
#attr(torontoData.ts[,2],'frequency')<-24
#torontoData.ts[,2]=ts(torontoData.ts[,2],frequency=24)
#torontoData.ts<-ts(torontoData,frequency=24)
torontoData.ts[,3]<-ts(torontoData.ts[,3],start=c(1,1),end=c(5114,24),frequency=24)
#is.ts(torontoData.ts[,2])
boxplot(torontoData.ts[,3]~cycle(torontoData.ts[,3]))
#plot(decompose(torontoData.ts))
#torontoData.ts.day<-aggregate(torontoData.ts,nfrequency=1)
#plot(torontoData.ts.day)
par(mfrow=c(2,2))
plot(torontoData.ts[,2],main="Original Data")
plot(torontoData.ts[,2],col="gray",main="1 Day Moving Average")
lines(ma(torontoData.ts[,2],order=24),col="red",lwd=1)
plot(torontoData.ts[,2],col="gray",main="1 Week Moving Average")
lines(ma(torontoData.ts[,2],order=168),col="blue",lwd=1)
plot(torontoData.ts[,2],col="gray",main="1 Year Moving Average")
lines(ma(torontoData.ts[,2],order=8766),col="green",lwd=1)
#par(mfrow = c(2,2))
#plot(torontoData.ts, main = "Original Data")
#plot(diff(torontoData.ts,1), main = "Detrended data")
#plot(diff(torontoData.ts, 24), main = "Seasonally adjusted data")
#plot (diff(diff(torontoData.ts,1), 24), main = "Seasonally adj. and detrended data")

#•••statiscal analysis
#plot(decompose(torontoData.ts))
#plot(stl(torontoData.ts,s.window="periodic"))
mean(torontoData.ts[,3])
var(torontoData.ts[,3])
#?????summary(auto.arima(torontoData.ts,approximation = FALSE))
#?????acf(torontoData.ts)
#?????acf(diff(torontoData.ts))
#?????acf(diff(diff(torontoData.ts),24))
#?????acf(diff(diff(diff(torontoData.ts),24),168))

#•••Dividing the data to a training set and a test set
#View(torontoData.ts)
training<-torontoData.ts[-c(105193:122736),]
testing<-torontoData.ts[c(105193:122736),]
training.ts<-ts(training[,3],frequency=24)
testing.ts<-ts(testing[,3],frequency=24)

#•••Modeling
#••Linear Regression
#View(training)
trainingTslm<-tslm(training.ts~trend+season)
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


#••Smothing method

#SES
trainingSES<-ses(training.ts,initial="optimal")
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
trainingHolt<-holt(training.ts,initial="optimal")
summary(trainingHolt)
summary(trainingHolt$residuals)
qqnorm(trainingHolt$residuals,main="Notmal QQ Plot for HOLT Model")
qqline(trainingHolt$residuals)
hist(trainingHolt$residuals,main="Histogram for HOLT Model")
acf(trainingHolt$residuals, main="ACF for HOLT Model")

#HW
trainingHW<-hw(training.ts,initial="optimal")
summary(trainingHW)
summary(trainingHW$residuals)
qqnorm(trainingHW$residuals, main="Notmal QQ Plot for HW Model")
qqline(trainingHW$residuals)
hist(trainingHW$residuals, main="Histogram for HW Model")
acf(trainingHW$residuals, main="ACF for HW Model")

#ETS MODEL
trainingETS<-ets(training.ts)
summary(trainingETS)
summary(trainingETS$residuals)
qqnorm(trainingETS$residuals, main="Notmal QQ Plot for ETS Model")
qqline(trainingETS$residuals)
hist(trainingETS$residuals, main="Histogram for ETS Model")
acf(trainingETS$residuals, main="ACF for ETS Model")

#••ARIMA MODEL
acf(training.ts)
pacf(training.ts)
acf(diff(training.ts,24))
acf(diff(diff(training.ts,24)))
pacf(diff(diff(training.ts,24)))

acf(seasadj(stl(training,s.window="periodic")))
pacf(seasadj(stl(training,s.window="periodic")))
acf(diff(training))
pacf(diff(training))
acf(diff(diff(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19),15),29))
find.freq(diff(seasadj(stl(training,s.window="periodic"))))
find.freq(diff(diff(seasadj(stl(training,s.window="periodic"))),5))
find.freq(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3))
find.freq(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8))
find.freq(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13))
find.freq(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24))
find.freq(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11))
find.freq(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19))
find.freq(diff(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19),15))
find.freq(diff(diff(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19),15),29))
acf(decompose(training)$x)
?decompose

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

trainingARIMA2<-arima(training.ts,order=c(2,0,3),seasonal=list(order=c(2,0,3),period=24))



trainingARIMA<-auto.arima(training.ts, approximation = FALSE)
summary(trainingARIMA)


summary(trainingARIMA$residuals)
qqnorm(trainingARIMA$residuals, main="Notmal QQ Plot for autoARIMA Model")
qqline(trainingARIMA$residuals)
hist(trainingARIMA$residuals, main="Histogram for autoARIMA Model")
acf(trainingARIMA$residuals, main="ACF for autoARIMA Model")




forecastedTslm<-forecast(trainingTslm,h=17544)
accuracy(forecastedTslm, testing.ts)

forecastedSES<-ses(training.ts,initial="optimal",h=17544)
accuracy(forecastedSES,testing.ts)

forecastHolt<-holt(training.ts,initial="optimal", h=17544)
accuracy(forecastedHolt, testing.ts)

forecastedHW<-hw(training.ts, initial = "optimal", h=17544)
accuracy(forecastedHW,testing.ts)

forecastedETS<-ets(training.ts,h=17544)
accuracy(forecastedETS,testing.ts)

forecastedARIMA<-forecast(trainingARIMA,h=17544)
accuracy(forecastedARIMA,testing.ts)

forecastedARIMA<-forecast(trainingARIMA,h=17544)
accuracy(forecastedARIMA,testing.ts)

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
