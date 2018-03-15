library(xts)
library(forecast)
library(astsa)
#library(aTSA)
library(zoo)
library(stats)
library(anytime)
library(base)
library(fpp)

#Remove all the objects in R.
#rm(list = ls())

#•••Initial analysis of the data (plots, statistical analysis, etc..)
projectData <- read.csv("input_data_team_9.csv")
#projectData<-read.csv("input_data_team_9_processed.csv")
torontoData <-
  data.frame(Date = projectData[, 1],
             Hour = projectData[, 2],
             Toronto = projectData[, 8])
torontoData.ts <- torontoData[-c(1:111072, 122017:128616),]
#torontoDataDate<-torontoData[,1]
torontoData.ts[, 3] <- ts(torontoData.ts[, 3])
#torontoData.ts[,1]<-anytime::anydate(torontoData.ts[,1])
#head(torontoData.ts)
par(mfrow = c(1, 1))
plot(torontoData.ts[, 3],
     xlab = "xth hour",
     ylab = "Electricity Demand",
     main = "Hourly Eletricity Demand in Toronto")
#Substitute outliers with value of 0 using moving median method.
for (index in 1:nrow(torontoData.ts))
{
  if (torontoData.ts[index, 3] == 0)
  {
    torontoData.ts[index, 3] <-
      median(torontoData.ts[(index - 24):(index - 1), 3])
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
acf(torontoData.ts[, 3], main = "ACF of Hourly Eletricity Demand in Toronto - Adjusted")
find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x), plot = FALSE)
  if (max(spec$spec) > 10)
    # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1 / spec$freq[which.max(spec$spec)])
    if (period == Inf)
      # Find next local maximum
    {
      j <- which(diff(spec$spec) > 0)
      if (length(j) > 0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1 / spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}

#attr(torontoData.ts[,3],'frequency')<-24
find.freq(torontoData.ts[, 3])
torontoData.ts[, 3] = ts(torontoData.ts[, 3], frequency = find.freq(torontoData.ts[, 3]))
#torontoData.ts<-ts(torontoData,frequency=24)
plot(torontoData.ts[, 3], col = "blue")

torontoData.ts.day <- aggregate(torontoData.ts[, 3], nfrequency = 1)
plot(torontoData.ts.day, col = "blue")

find.freq(torontoData.ts.day)
torontoData.ts.day = ts(torontoData.ts.day, frequency = find.freq(torontoData.ts.day))

torontoData.ts.week <- aggregate(torontoData.ts.day, nfrequency = 1)
plot(torontoData.ts.week, col = "blue")

find.freq(torontoData.ts.week)
length(torontoData.ts[, 3]) / 24 / 7

torontoData.ts.week = ts(torontoData.ts.week, frequency = find.freq(torontoData.ts.week))

torontoData.ts.26week <-
  aggregate(torontoData.ts.week, nfrequency = 1)
find.freq(torontoData.ts.26week)



#torontoData.ts.dayTslm<-tslm(torontoData.ts.day~trend+season)
#find.freq(torontoData.ts.day)

#plot(torontoData.ts[,3],col="gray")


torontoData.ts[, 3] <-
  ts(torontoData.ts[, 3], start = c(1, 1), frequency = 24)
#is.ts(torontoData.ts[,2])
boxplot(torontoData.ts[, 3] ~ cycle(torontoData.ts[, 3]))
#plot(decompose(torontoData.ts))
#torontoData.ts.day<-aggregate(torontoData.ts,nfrequency=1)
#plot(torontoData.ts.day)

#??????????????????????Stationary has seasonality?
acf(torontoData.ts[, 3], main = "ACF of Hourly Eletricity Demand in Toronto - Outliers Replaced")
#adf.test(torontoData.ts[,3])
#???? Do we need to forecast the future data even after forecasting the test set.
#??????training performance = best V.S. test not = best
#???? Arima loop consumes too much time. It needs at least 5+ hours for each execution.
#6*6*6*6*6*6


#TAKE THE LAST YEAR/TWO YEARS ONLY USE ONE YEAR
#SHORT TERM PREDICTION, A MONTH OR COUPLE OF WEEKS
#DIFFERENCING IS 0 OR 1, START WITH AUTO ARIMA, THEN ITERATE USING THE INSIGHTS FROM THIS


# Use moving average to observe trend
par(mfrow = c(2, 2))
plot(torontoData.ts[, 3], main = "Original Data")

plot(torontoData.ts[, 3], col = "gray", main = "1 Day Moving Average")
lines(ma(torontoData.ts[, 3], order = 24), col = "red", lwd = 1)
plot(torontoData.ts[, 3], col = "gray", main = "1 Week Moving Average")
lines(ma(torontoData.ts[, 3], order = 168),
      col = "blue",
      lwd = 1)
plot(torontoData.ts[, 3], col = "gray", main = "1 Month Moving Average")
lines(ma(torontoData.ts[, 3], order = 731),
      col = "green",
      lwd = 1)
#par(mfrow = c(2,2))
#plot(torontoData.ts, main = "Original Data")
#plot(diff(torontoData.ts,1), main = "Detrended data")
#plot(diff(torontoData.ts, 24), main = "Seasonally adjusted data")
#plot (diff(diff(torontoData.ts,1), 24), main = "Seasonally adj. and detrended data")

#•••statiscal analysis to our sample data
#plot(decompose(torontoData.ts))
#plot(stl(torontoData.ts,s.window="periodic"))

#Sample mean
par(mfrow = c(1, 1))
rm(sampleMean)
sampleMean <- mean(torontoData.ts[, 3])
sampleMean
plot(torontoData.ts[, 3], col = "gray", main = "Sample Mean")
sampleMean = seq(sampleMean, sampleMean, length.out = length(torontoData.ts[, 3]))
lines(c(1:length(torontoData.ts[, 3])), sampleMean, col = "red", lty = 3)

#Sample Median
sampleMedian <- median(torontoData.ts[, 3])
sampleMedian




#Sample Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
sampleMode <- getmode(torontoData.ts[, 3])
sampleMode

#Dispersion
summary(torontoData.ts[, 3])

#Sample Variance
rm(sampleVariance)
sampleVariance <- var(torontoData.ts[, 3])
sampleVariance

#Sample Standard Deviation
rm(sampleSD)
sampleSD <- sqrt(sampleVariance)
sampleSD

# 95% confidence interval
par(mfrow = c(1, 1))
#plot(c(1:length(torontoData.ts[,3])),torontoData.ts[,3],col="gray",main="95% Confidene Interval for Toronto Data Population Mean")
plot(torontoData.ts[, 3], col = "gray", main = "95% Confidene Interval for Toronto Data")
rm(sampleMean)
sampleMean <- mean(torontoData.ts[, 3])

lowerBound = seq(sampleMean - 2 * sampleSD,
                 sampleMean - 2 * sampleSD,
                 length.out = length(torontoData.ts[, 3]))
upperBound = seq(sampleMean + 2 * sampleSD,
                 sampleMean + 2 * sampleSD,
                 length.out = length(torontoData.ts[, 3]))
lines(c(1:length(torontoData.ts[, 3])), lowerBound, col = "red", lty = 2)
lines(c(1:length(torontoData.ts[, 3])), upperBound, col = "red", lty = 2)


#Sample CV
CV = sqrt(var(torontoData.ts[, 3])) / mean(torontoData.ts[, 3])
CV

#Sample Return To Risk
returnToRisk = 1 / CV
returnToRisk


#summary(auto.arima(torontoData.ts,approximation = FALSE))
#acf(torontoData.ts)
#acf(diff(torontoData.ts))
#acf(diff(diff(torontoData.ts),24))
#acf(diff(diff(diff(torontoData.ts),24),168))



#•••Dividing the data to a training set and a test set
#View(torontoData.ts)
training <- torontoData.ts[-c(8785:10944),]
testing <- torontoData.ts[c(8785:10944),]
training.ts <- ts(training[, 3], frequency = 24)
testing.ts <- ts(testing[, 3], frequency = 24)


#•••Modeling
#••Linear Regression
#View(training)
acf(training.ts)

trainingTslm <- tslm(training.ts ~ trend + season)
#t <- seq(1, length(training.ts), length = length(training.ts))
#t<-ts(t,frequency=24)
#t2 <- t^2
#t2<-ts(t2,frequency=24)
#t
#sin.t <- sin(2*pi*t)
#sin.t<-ts(sin.t, frequency=24)
#cos.t <- cos(2*pi*t)
#cos.t<-ts(cos.t,frequency=24)
#trainingTslm<-tslm(training.ts~trend+season+t2+sin.t)
#?tslm
#sin.t<-2*pi*t
summary(trainingTslm)
summary(training.ts)
anova(trainingTslm)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for TSLM Model")
lines(trainingTslm$fitted.values, col = "red")


#lines(traingTslm,col="red",lwd=1)
#abline(trainingTslm,col="red")
#plot(trainingTslm$residuals)

plot(trainingTslm$residuals)
summary(trainingTslm$residuals)
qqnorm(trainingTslm$residuals)
qqline(trainingTslm$residuals)
hist(trainingTslm$residuals)
acf(trainingTslm$residuals)
pacf(trainingTslm$residuals)

# According to the residuals' acf and pacf, we know that apart from trend and seasonality, our data has autocorrelation cross time lags. Thus, ARMA model is needed.
lag.plot(trainingTslm$residuals, lags = 24, do.lines = FALSE)
# We could observe a strong correlation at lag1, lag2, lag3 and lag4, demonstraing that our data has autocorrelations.

#••Smothing method

#SES
trainingSES <- ses(training.ts, initial = "optimal")
summary(trainingSES)
summary(trainingSES$residuals)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for SES Model")
lines(c(1:length(training.ts)), trainingSES$fitted, col = "red")

qqnorm(trainingSES$residuals, main = "Normal QQ Plot for the SES Model")
qqline(trainingSES$residuals)
hist(trainingSES$residuals, main = "Histogram for SES Model")
acf(trainingSES$residuals, main = "ACF for SES Model")

# We need to cut a very small porportion of it to emplify the effect
#plot(training,col="gray",main="SES Model")
#lines(trainingSES$fitted,col="blue",lwd=1,lyt)

#Holt
trainingHolt <- holt(training.ts, initial = "optimal")
summary(trainingHolt)
summary(trainingHolt$residuals)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for HOLT Model")
lines(c(1:length(training.ts)), trainingHolt$fitted, col = "red")

qqnorm(trainingHolt$residuals, main = "Notmal QQ Plot for HOLT Model")
qqline(trainingHolt$residuals)
hist(trainingHolt$residuals, main = "Histogram for HOLT Model")
acf(trainingHolt$residuals, main = "ACF for HOLT Model")

#HW
trainingHW <- hw(training.ts, initial = "optimal")
summary(trainingHW)
summary(trainingHW$residuals)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for HW Model")
lines(c(1:length(training.ts)), trainingHW$fitted, col = "red")

qqnorm(trainingHW$residuals, main = "Notmal QQ Plot for HW Model")
qqline(trainingHW$residuals)
hist(trainingHW$residuals, main = "Histogram for HW Model")
acf(trainingHW$residuals, main = "ACF for HW Model")

#ETS MODEL
trainingETS <- ets(training.ts)
summary(trainingETS)
summary(trainingETS$residuals)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for ETS Model")
lines(c(1:length(training.ts)), trainingETS$fitted, col = "red")

qqnorm(trainingETS$residuals, main = "Notmal QQ Plot for ETS Model")
qqline(trainingETS$residuals)
hist(trainingETS$residuals, main = "Histogram for ETS Model")
acf(trainingETS$residuals, main = "ACF for ETS Model")

#••Machine Learning MODE
#NNETAR MODEL
trainingNN <- nnetar(training.ts)
summary(trainingNN)
summary(trainingNN$residuals)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for NNETAR Model")
lines(c(1:length(training.ts)), trainingNN$fitted, col = "red")

qqnorm(trainingNN$residuals, main = "Notmal QQ Plot for NNETAR Model")
qqline(trainingNN$residuals)
hist(trainingNN$residuals, main = "Histogram for NNETAR Model")
acf(trainingNN$residuals, main = "ACF for NNETAR Model")

#••ARIMA MODEL
acf(training.ts)
pacf(training.ts)
acf(diff(training.ts, 24))
acf(diff(training.ts))
acf(diff(diff(training.ts, 24)))
pacf(diff(diff(training.ts, 24)))
diffTraining <- diff(diff(training.ts, 24))
summary(diffTraining)

#Check if we have differenced training.ts to white noise
plot(diffTraining)

# The bad performance of linear regression shows that there is almost no trend or seasonality anymore.
diffTrainingTslm <- tslm(diffTraining ~ trend + season)
summary(diffTrainingTslm)

qqnorm(diffTraining)
qqline(diffTraining)

#Use arma model to test reasonable p=2 and q=3 values observed from acf and pacf.
#acf(diffTraining)
#pacf(diffTraining)
#ArmaDiffTraining<-arma(diffTraining,order=c(2,2))
#summary(ArmaDiffTraining)

#accuracy(ArmaDiffTraining$fitted.values,diffTraining)
#forecastedARMA<-forecast(ArmaDiffTraining,diffTraining, h=length(testing.ts))
#predictedARMA<-predict(ArmaDiffTraining,diffTraining,h=length(testing.ts))
#accuracy(predictedARMA,diff(diff(testing.ts),24))



#acf(seasadj(stl(training.ts,s.window="periodic")))
#pacf(seasadj(stl(training.ts,s.window="periodic")))
#acf(diff(training.ts))
#pacf(diff(training.ts))
#acf(diff(diff(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19),15),29))
#find.freq(diff(seasadj(stl(training.ts,s.window="periodic"))))
#find.freq(diff(diff(seasadj(stl(training,s.window="periodic"))),5))
#find.freq(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3))
#find.freq(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8))
#find.freq(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13))
#find.freq(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24))
#find.freq(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11))
#find.freq(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19))
#find.freq(diff(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19),15))
#find.freq(diff(diff(diff(diff(diff(diff(diff(diff(diff(diff(seasadj(stl(training,s.window="periodic"))),5),3),8),13),24),11),19),15),29))
#acf(decompose(training)$x)
#?decompose

#plot(stl(training,s.window='periodic'))
#acf(stl(training,s.window='periodic'))

#pacf(training)

#plot(decompose(diff(diff(training,24),1)))


#pacf(diff(diff(diff(training),24),33))

#acf(diff(diff(training,24)))
#acf(diff(training,24))
#acf(diff(training))


#acf(diff(seasadj(stl(training,s.window='periodic'))))



#trainingARIMA2<-arima(training.ts,order=c(2,0,3),seasonal=list(order=c(2,0,3),period=24))



trainingARIMA <- auto.arima(training.ts, approximation = FALSE)
summary(trainingARIMA)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for AutoARIMA Model")
lines(c(1:length(training.ts)), trainingARIMA$fitted, col = "red")

summary(trainingARIMA$residuals)
qqnorm(trainingARIMA$residuals, main = "Notmal QQ Plot for autoARIMA Model")
qqline(trainingARIMA$residuals)
hist(trainingARIMA$residuals, main = "Histogram for autoARIMA Model")
acf(trainingARIMA$residuals, main = "ACF for autoARIMA Model")


# Arima Model Selection Loop. The chooses of parameters depend on the result of auto.arima
# Initialization
rm(RMSE)
p1 = p2 = d1 = d2 = q1 = q2 = 0
RMSE.train = RMSE.test = i = 1
RMSE <- data.frame(p1, d1, q1, p2, d2, q2, RMSE.train, RMSE.test)

iterate.arima <- function(RMSE, x, y, forecastPeriod)
{
  for (p1 in 0:4)
  {
    for (d1 in 0:1)
    {
      for (q1 in 0:4)
      {
        for (p2 in 0:2)
        {
          for (d2 in 0:1)
          {
            for (q2 in 0:2)
            {
              result = tryCatch({
                # write your intended code here
                
              }, warning = function(w) {
                # log the warning or take other action here
              }, error = function(e) {
                # log the error or take other action here
              }, finally = {
                # this will execute no matter what else happened
                
                findArima <-
                  arima(
                    x,
                    order = c(p1, d1, q1),
                    seasonal = list(
                      order = c(p2, d2, q2),
                      peroid = frequency(x)
                    ),
                    method = "ML"
                  )
                RMSE[i, 'p1'] <- p1
                RMSE[i, 'd1'] <- d1
                RMSE[i, 'q1'] <- q1
                RMSE[i, 'p2'] <- p2
                RMSE[i, 'd2'] <- d2
                RMSE[i, 'q2'] <- q2
                RMSE[i, 'RMSE.train'] <-
                  accuracy(forecast(findArima, forecastPeriod), c(y))[1, "RMSE"]
                RMSE[i, 'RMSE.test'] <-
                  accuracy(forecast(findArima, forecastPeriod), c(y))[2, "RMSE"]
                print("Arima Model Number")
                print(nrow(RMSE))
                print("Non-seasonal Part:")
                print(c(p1, d1, q1))
                print("Seasonal Part:")
                print(c(p2, d2, q2))
                print("Seasonal Period:")
                print(frequency(x))
                i <- i + 1
              })
              
              
              #findArima<-arima(x,order=c(p1,d1,q1),seasonal=c(p2,d2,q2),method="ML")
              #print(accuracy((forecast(findArima,lead=24))[,2],milk.test)[1,"RMSE"])
            }
          }
        }
      }
    }
  }
  
}
iterate.arima(RMSE, training.ts, testing.ts, length(testing.ts))



find.arima <- function(x)
{
  for (i in 2:nrow(RMSE))
  {
    if (RMSE[i, 'RMSE.test'] < RMSE[i - 1, 'RMSE.test'])
    {
      RMSE.test.min.index <- i
    }
    else
    {
      RMSE.test.min.index <- i - 1
    }
    if (RMSE[i, 'RMSE.train'] < RMSE[i - 1, 'RMSE.train'])
    {
      RMSE.train.min.index <- i
    }
    else
    {
      RMSE.train.min.index <- i - 1
    }
    
  }
  
  print("Optimal ARIMA MODEL on Training:")
  print(
    c(
      p = RMSE[RMSE.train.min.index, 'p1'],
      d = RMSE[RMSE.train.min.index, 'd1'],
      q = RMSE[RMSE.train.min.index, 'q1'],
      sp = RMSE[RMSE.train.min.index, 'p2'],
      sd = RMSE[RMSE.train.min.index, 'd2'],
      sq = RMSE[RMSE.train.min.index, 'q2'],
      m = frequency(x),
      RMSE.train = RMSE[RMSE.train.min.index, 'RMSE.train'],
      RMSE.test = RMSE[RMSE.train.min.index, 'RMSE.test']
    )
  )
  print("Optimal ARIMA MODEL on Testing:")
  print(
    c(
      p = RMSE[RMSE.test.min.index, 'p1'],
      d = RMSE[RMSE.test.min.index, 'd1'],
      q = RMSE[RMSE.test.min.index, 'q1'],
      sp = RMSE[RMSE.test.min.index, 'p2'],
      sd = RMSE[RMSE.test.min.index, 'd2'],
      sq = RMSE[RMSE.test.min.index, 'q2'],
      m = frequency(x),
      RMSE.train = RMSE[RMSE.test.min.index, 'RMSE.train'],
      RMSE.test = RMSE[RMSE.test.min.index, 'RMSE.test']
    )
  )
  
}
find.arima(training.ts)

# Model Validation

# Prediction Benchmarks
meanfBenchmark <- meanf(training.ts, h = length(testing.ts))
accuracy(meanfBenchmark, c(testing.ts))
plot(meanfBenchmark)

naiveBenchmark <- naive(training.ts, h = length(testing.ts))
accuracy(naiveBenchmark, c(testing.ts))
plot(naiveBenchmark)

snaiveBenchmark <- snaive(training.ts, h = length(testing.ts))
accuracy(snaiveBenchmark, c(testing.ts))
plot(snaiveBenchmark)

rwfBenchmark <-
  rwf(training.ts, h = length(testing.ts), drift = TRUE)
accuracy(rwfBenchmark, c(testing.ts))
plot(rwfBenchmark)



# Better models
forecastedTslm <- forecast(trainingTslm, h = length(testing.ts))
accuracy(forecastedTslm, c(testing.ts))
plot(forecastedTslm)

forecastedSES <-
  ses(training.ts, initial = "optimal", h = length(testing.ts))
accuracy(forecastedSES, c(testing.ts))
plot(forecastedSES)

forecastedHolt <-
  holt(training.ts, initial = "optimal", h = length(testing.ts))
accuracy(forecastedHolt, c(testing.ts))
plot(forecastedHolt)

forecastedHW <-
  hw(training.ts, initial = "optimal", h = length(testing.ts))
accuracy(forecastedHW, c(testing.ts))
plot(forecastedHW)

forecastedETS <- forecast(ets(training.ts), h = length(testing.ts))
accuracy(forecastedETS, c(testing.ts))
plot(forecastedETS)

forecastedNN <- forecast(trainingNN, h = length(testing.ts))
accuracy(forecastedNN, c(testing.ts))
plot(forecastedNN)

forecastedARIMA <- forecast(trainingARIMA, h = length(testing.ts))
accuracy(forecastedARIMA, c(testing.ts))
plot(forecastedARIMA)

forecastedARIMA <- forecast(trainingARIMA, h = length(testing.ts))
accuracy(forecastedARIMA, c(testing.ts))
plot(forecastedARIMA)
#plot.ts(forecastedARIMA) 
