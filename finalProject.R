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

# ••• Initial Analysis of the Data •••
projectData <- read.csv("input_data_team_9.csv")
torontoData <-
  data.frame(Date = projectData[, 1],
             Hour = projectData[, 2],
             Toronto = projectData[, 8])

#A sub-range of the data from January 1, 2016 to December 31, 2016 was extracted as the training data set for further analysis. Our testing data set consists of the demand data from January 1, 2017 to March 30, 2017.
torontoData.ts <- torontoData[-c(1:111072, 122017:128616),]

#Convert our sample data set into a time series sample data set.
torontoData.ts[, 3] <- ts(torontoData.ts[, 3])
par(mfrow = c(1, 1))
plot(torontoData.ts[, 3],
     xlab = "xth hour",
     ylab = "Electricity Demand",
     main = "Hourly Eletricity Demand in Toronto")

#Substitute outliers whose value is 0 with their corresponding moving medians.

#boxOut<-boxplot(torontoData.ts$Toronto)
#outliers<-boxOut$out

for (index in 1:nrow(torontoData.ts))
{
  if (torontoData.ts[index, 3] == 0)
  {
    torontoData.ts[index, 3] <-
      median(torontoData.ts[(index - 24):(index - 1), 3])
    print(index)
  }
  #for(outlierIndex in 1:length(outliers))
  #{
  #  if (torontoData.ts[index, 3] == outliers[outlierIndex])
  #  {
  #    torontoData.ts[index, 3] <-
  #      median(torontoData.ts[(index - 24):(index - 1), 3])
  #    print(index)
  #  }
  #}
}

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
plot(torontoData.ts[, 3], col = "blue")

#torontoData.ts.day <- aggregate(torontoData.ts[, 3], nfrequency = 1)
#plot(torontoData.ts.day, col = "blue")
#find.freq(torontoData.ts.day)
#torontoData.ts.day = ts(torontoData.ts.day, frequency = find.freq(torontoData.ts.day))

#torontoData.ts.week <- aggregate(torontoData.ts.day, nfrequency = 1)
#plot(torontoData.ts.week, col = "blue")

#find.freq(torontoData.ts.week)
#length(torontoData.ts[, 3]) / 24 / 7

#torontoData.ts.week = ts(torontoData.ts.week, frequency = find.freq(torontoData.ts.week))

#torontoData.ts.26week <-
#  aggregate(torontoData.ts.week, nfrequency = 1)
#find.freq(torontoData.ts.26week)

torontoData.ts[, 3] <-
  ts(torontoData.ts[, 3], start = c(1, 1), frequency = 24)
#is.ts(torontoData.ts[,2])
boxplot(torontoData.ts[, 3] ~ cycle(torontoData.ts[, 3]))
#plot(decompose(torontoData.ts))
#torontoData.ts.day<-aggregate(torontoData.ts,nfrequency=1)
#plot(torontoData.ts.day)

acf(torontoData.ts[, 3], main = "ACF of Hourly Eletricity Demand in Toronto - Outliers Replaced")

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

# ••• Statiscal analysis to our sample data •••
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

# ••• Dividing the data to a training set and a test set •••
#View(torontoData.ts)
training <- torontoData.ts[-c(8785:10944),]
testing <- torontoData.ts[c(8785:10944),]
training.ts <- ts(training[, 3], frequency = 24)
testing.ts <- ts(testing[, 3], frequency = 24)

# ••• Model Selection and Fitting •••
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
lines(trainingTslm$fitted, col = "red")
plot(training.ts, trainingTslm$fitted, main = "Fitted Against Training (Tslm)")

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
plot(c(training.ts), trainingSES$fitted, main = "Fitted Against Training (SES)")

plot(trainingSES$residuals)
summary(trainingSES$residuals)
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
plot(c(training.ts), trainingHolt$fitted, main = "Fitted Against Training (Holt)")

plot(trainingHolt$residuals)
summary(trainingHolt$residuals)
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
plot(c(training.ts), trainingHW$fitted, main = "Fitted Against Training (HW)")

plot(trainingHW$residuals)
summary(trainingHW$residuals)
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
plot(c(training.ts), trainingETS$fitted, main = "Fitted Against Training (ETS)")

plot(trainingETS$residuals)
summary(trainingETS$residuals)
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
plot(c(training.ts), trainingNN$fitted, main = "Fitted Against Training (NNETAR)")

plot(trainingNN$residuals)
summary(trainingNN$residuals)
qqnorm(trainingNN$residuals, main = "Notmal QQ Plot for NNETAR Model")
qqline(trainingNN$residuals)
hist(trainingNN$residuals, main = "Histogram for NNETAR Model")
acf(trainingNN$residuals[-c(1:39)], main = "ACF for NNETAR Model")

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

diffTrainingTslm <- tslm(diffTraining ~ trend + season)
summary(diffTrainingTslm)
# The bad performance of the linear regression using the differenced data shows that there is almost no trend or seasonality anymore. Which means the linear regression model has well captured the trend and seasonality.



# AutoARIMA
trainingARIMA <- auto.arima(training.ts, approximation = FALSE)
summary(trainingARIMA)

par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for AutoARIMA Model")
lines(c(1:length(training.ts)), trainingARIMA$fitted, col = "red")
plot(c(training.ts), trainingARIMA$fitted, main = "Fitted Against Training (autoARIMA)")

plot(trainingARIMA$residuals)
summary(trainingARIMA$residuals)
qqnorm(trainingARIMA$residuals, main = "Notmal QQ Plot for autoARIMA Model")
qqline(trainingARIMA$residuals)
hist(trainingARIMA$residuals, main = "Histogram for autoARIMA Model")
acf(trainingARIMA$residuals, main = "ACF for autoARIMA Model")

# Optimal Arima Model Selection Loop. The chooses of parameters depend on the result of auto.arima
# Initialization
rm(RMSE)
p1 = p2 = d1 = d2 = q1 = q2 = 0
RMSE.train = RMSE.test = i = 1
RMSE <- data.frame(p1, d1, q1, p2, d2, q2, RMSE.train, RMSE.test)

#iterate.arima <- function(RMSE, x, y, forecastPeriod)
#{
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
              findArima <-
                arima(
                  training.ts,
                  order = c(p1, d1, q1),
                  seasonal = list(
                    order = c(p2, d2, q2),
                    period = frequency(training.ts)
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
                accuracy(forecast(findArima, length(testing.ts)), c(testing.ts))[1, "RMSE"]
              RMSE[i, 'RMSE.test'] <-
                accuracy(forecast(findArima, length(testing.ts)), c(testing.ts))[2, "RMSE"]
              print("Arima Model Number")
              print(nrow(RMSE))
              print("Non-seasonal Part:")
              print(c(p1, d1, q1))
              print("Seasonal Part:")
              print(c(p2, d2, q2))
              print("Seasonal Period:")
              print(frequency(x))
              i <- i + 1
            }, warning = function(w) {
              # log the warning or take other action here
            }, error = function(e) {
              # log the error or take other action here
            }, finally = {
              # this will execute no matter what else happened
            })
            #findArima<-arima(x,order=c(p1,d1,q1),seasonal=c(p2,d2,q2),method="ML")
            #print(accuracy((forecast(findArima,lead=24))[,2],milk.test)[1,"RMSE"])
          }
        }
      }
    }
  }
}
#}
#iterate.arima(RMSE, training.ts, testing.ts, length(testing.ts))

RMSE.test.min.index = 1
RMSE.train.min.index = 1

find.arima <- function(x)
{
  for (i in 2:nrow(RMSE))
  {
    if (RMSE[RMSE.test.min.index, 'RMSE.test'] > RMSE[i, 'RMSE.test'])
    {
      RMSE.test.min.index <- i
    }
    else
    {
      
    }
    if (RMSE[RMSE.train.min.index, 'RMSE.train'] > RMSE[i, 'RMSE.train'])
    {
      RMSE.train.min.index <- i
    }
    else
    {
      
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

# OPTIMAL ARIMA MODEL FOUND BY ITERATING HUNDREDS OF COMBINITIONS OF PARAMETERS
optimalArima <-
  arima(
    training.ts,
    order = c(2, 0, 1),
    seasonal = list(order = c(2, 1, 1), period = 24),
    method = "ML"
  )

summary(optimalArima)
par(mfrow = c(1, 1))
plot(training.ts, col = "gray", main = "Fitted Data for OptimalARIMA Model")
lines(c(1:length(training.ts)), fitted(optimalArima), col = "red")
plot(c(training.ts), fitted(optimalArima), main = "Fitted Against Training (optimalARIMA)")

plot(optimalArima$residuals)
summary(optimalArima$residuals)
qqnorm(optimalArima$residuals, main = "Notmal QQ Plot for optimalARIMA Model")
qqline(optimalArima$residuals)
hist(optimalArima$residuals, main = "Histogram for optimalARIMA Model")
acf(optimalArima$residuals, main = "ACF for optimalARIMA Model")

# ••• Model Validation •••

# Prediction Benchmarks
par(mfrow = c(1, 1))

meanfBenchmark <- meanf(training.ts, h = length(testing.ts))
accuracy(meanfBenchmark, c(testing.ts))
plot(meanfBenchmark)
plot(testing.ts, col = "gray", main = "Forecasted Data for Meanf Model")
lines(c(1:length(testing.ts)), meanfBenchmark$mean, col = "blue")
plot(c(testing.ts), meanfBenchmark$mean, main = "Forecasted Against Test Data Set (meanf)")

naiveBenchmark <- naive(training.ts, h = length(testing.ts))
accuracy(naiveBenchmark, c(testing.ts))
plot(naiveBenchmark)
plot(testing.ts, col = "gray", main = "Forecasted Data for Naive Model")
lines(c(1:length(testing.ts)), naiveBenchmark$mean, col = "blue")
plot(c(testing.ts), naiveBenchmark$mean, main = "Forecasted Against Test Data Set (naive)")

snaiveBenchmark <- snaive(training.ts, h = length(testing.ts))
accuracy(snaiveBenchmark, c(testing.ts))
plot(snaiveBenchmark)
plot(testing.ts, col = "gray", main = "Forecasted Data for SNaive Model")
lines(c(1:length(testing.ts)), snaiveBenchmark$mean, col = "blue")
plot(c(testing.ts), snaiveBenchmark$mean, main = "Forecasted Against Test Data Set (snaive)")

rwfBenchmark <-
  rwf(training.ts, h = length(testing.ts), drift = TRUE)
accuracy(rwfBenchmark, c(testing.ts))
plot(rwfBenchmark)
plot(testing.ts, col = "gray", main = "Forecasted Data for RWF Model")
lines(c(1:length(testing.ts)), rwfBenchmark$mean, col = "blue")
plot(c(testing.ts), rwfBenchmark$mean, main = "Forecasted Against Test Data Set (rwf)")

# Better models
forecastedTslm <- forecast(trainingTslm, h = length(testing.ts))
accuracy(forecastedTslm, c(testing.ts))
plot(forecastedTslm)
plot(testing.ts, col = "gray", main = "Forecasted Data for Tslm Model")
lines(c(1:length(testing.ts)), forecastedTslm$mean, col = "blue")
plot(c(testing.ts), forecastedTslm$mean, main = "Forecasted Against Test Data Set (Tslm)")

forecastedSES <-
  ses(training.ts, initial = "optimal", h = length(testing.ts))
accuracy(forecastedSES, c(testing.ts))
plot(forecastedSES)
plot(testing.ts, col = "gray", main = "Forecasted Data for SES Model")
lines(c(1:length(testing.ts)), forecastedSES$mean, col = "blue")
plot(c(testing.ts), forecastedSES$mean, main = "Forecasted Against Test Data Set (SES)")

forecastedHolt <-
  holt(training.ts, initial = "optimal", h = length(testing.ts))
accuracy(forecastedHolt, c(testing.ts))
plot(forecastedHolt)
plot(testing.ts, col = "gray", main = "Forecasted Data for Holt Model")
lines(c(1:length(testing.ts)), forecastedHolt$mean, col = "blue")
plot(c(testing.ts), forecastedHolt$mean, main = "Forecasted Against Test Data Set (Holt)")

forecastedHW <-
  hw(training.ts, initial = "optimal", h = length(testing.ts))
accuracy(forecastedHW, c(testing.ts))
plot(forecastedHW)
plot(testing.ts, col = "gray", main = "Forecasted Data for HW Model")
lines(c(1:length(testing.ts)), forecastedHW$mean, col = "blue")
plot(c(testing.ts), forecastedHW$mean, main = "Forecasted Against Test Data Set (HW)")

forecastedETS <- forecast(ets(training.ts), h = length(testing.ts))
accuracy(forecastedETS, c(testing.ts))
plot(forecastedETS)
plot(testing.ts, col = "gray", main = "Forecasted Data for ETS Model")
lines(c(1:length(testing.ts)), forecastedETS$mean, col = "blue")
plot(c(testing.ts), forecastedETS$mean, main = "Forecasted Against Test Data Set (ETS)")

forecastedNN <- forecast(trainingNN, h = length(testing.ts))
accuracy(forecastedNN, c(testing.ts))
plot(forecastedNN)
plot(testing.ts, col = "gray", main = "Forecasted Data for NNETAR Model")
lines(c(1:length(testing.ts)), forecastedNN$mean, col = "blue")
plot(c(testing.ts), forecastedNN$mean, main = "Forecasted Against Test Data Set (NNETAR)")

forecastedARIMA <- forecast(trainingARIMA, h = length(testing.ts))
accuracy(forecastedARIMA, c(testing.ts))
plot(forecastedARIMA)
plot(testing.ts, col = "gray", main = "Forecasted Data for autoARIMA Model")
lines(c(1:length(testing.ts)), forecastedARIMA$mean, col = "blue")
plot(c(testing.ts), forecastedARIMA$mean, main = "Forecasted Against Test Data Set (autoARIMA)")

forecastedOptimalARIMA <-
  forecast(optimalArima, h = length(testing.ts))
accuracy(forecastedOptimalARIMA, c(testing.ts))
plot(forecastedOptimalARIMA)
plot(testing.ts, col = "gray", main = "Forecasted Data for optimalARIMA Model")
lines(c(1:length(testing.ts)), forecastedOptimalARIMA$mean, col = "blue")
plot(c(testing.ts), forecastedOptimalARIMA$mean, main = "Forecasted Against Test Data Set (optimalARIMA)")

# Save the predicted values to the file "output_team_9.csv".
write.csv(forecastedOptimalARIMA$mean, file = "output_team_9.csv")
