###########################################
# Naive Methods                           #
#                                         #
#                                         #
###########################################


library(timeSeries)
library(forecast)
options(digits=3)
source('EDA/eda.r')



dailyPeakPath ='data/dailyPeaksWFEC.csv' ## from noaa site

dailyPeaksWFEC = read.csv(dailyPeakPath)

 




####### Validation Data Set########
#transform into ts time series
dailyPeaksWFEC_0.ts <-ts(dailyPeaksWFEC$Y.WFEC,start = 1,
                                              frequency = 1)
plot(dailyPeaksWFEC_0.ts)
head(dailyPeaksWFEC_0.ts)



# (1) Compute naive/no-change/random walk/persistence forecast
# First forecast is for Jan 2017
ffcast_0 <- c(2017,1)
naive1_0 <- naive(dailyPeaksWFEC_0.ts, h=1)



# Compute bias, pbias, and MAPE
#Note that bias1=(-ME) and pbias1=(-MPE)
accuracy(naive1_0)
forecast_0 <- window(naive1_0$fitted, start=ffcast_0)
observed_0 <- window(naive1_0$x, start=ffcast_0)
#------------------------------------------------



# (2) Compute seasonal naive / seasonal no-change
# First forecast is for Jan 2017
ffcastS_0 <- c(2017,1)
naiveS_0 <- snaive(dailyPeaksWFEC_0.ts, h=1)



# Compute bias, pbias, and MAPE
#Note that bias2=(-ME) and pbias2=(-MPE)
accuracy(naiveS_0)
forecastS_0 <- window(naiveS_0$fitted, start=ffcastS_0)
observedS_0 <- window(naiveS_0$x, start=ffcastS_0)
#------------------------------------------------



# (3) Compute rolling three-day mean
# First forecast is for Jan 2017
ffcast3_0 <- c(2017,1)
# Calculate three-day mean and store in last of the three days
naive3t_0 <- zoo::rollmean(dailyPeaksWFEC_0.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naive3_0 <- naive(naive3t_0, h=1)



# Compute bias, pbias, and MAPE
#Note that bias3=(-ME) and pbias3=(-MPE)
accuracy(naive3_0)
forecast3_0 <- window(naive3_0$fitted, start=ffcast3_0)
observed3_0 <- window(dailyPeaksWFEC_0.ts, start=ffcast3_0)



# Show observed and forecasts as of Dec 2018
plot(observedS_0, ylab="Monthly demand (TWh)")
lines(forecastS_0, col="red")
lines(window(forecast_0, start=ffcastS_0), col="blue")
lines(window(forecast3_0, start=ffcastS_0), col="cyan")
legend("bottomleft",
       legend=c("Observed","Naive","Seasonal naive","3-day mean"),
       col=c("black","blue","red","cyan"), lty=1)



print(accuracy(naive1_0))
print(accuracy(naiveS_0))
print(accuracy(naive3_0))



####### Test Data Set########
#transform into ts time series
dailyPeaksWFEC.ts <-ts(dailyPeaksWFEC$Y.WFEC,start = c(2016,1),
                                             end = c(2021,2),
                                             frequency =365)
plot(dailyPeaksWFEC.ts)
head(dailyPeaksWFEC.ts)



# (1) Compute naive/no-change/random walk/persistence forecast
# First forecast is for Jan 2019
ffcast <- c(2019,1)
naive1 <- naive(dailyPeaksWFEC.ts, h=1)



# Compute bias, pbias, and MAPE
#Note that bias1=(-ME) and pbias1=(-MPE)
accuracy(naive1)
forecast <- window(naive1$fitted, start=ffcast)
observed <- window(naive1$x, start=ffcast)
#------------------------------------------------



# (2) Compute seasonal naive / seasonal no-change
# First forecast is for Jan 2019
ffcastS <- c(2019,1)
naiveS <- snaive(dailyPeaksWFEC.ts, h=1)



# Compute bias, pbias, and MAPE
#Note that bias2=(-ME) and pbias2=(-MPE)
accuracy(naiveS)
forecastS <- window(naiveS$fitted, start=ffcastS)
observedS <- window(naiveS$x, start=ffcastS)
#------------------------------------------------



# (3) Compute rolling three-day mean
# First forecast is for Jan 2019
ffcast3 <- c(2019,1)
# Calculate three-day mean and store in last of the three days
naive3t <- zoo::rollmean(dailyPeaksWFEC.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naive3 <- naive(naive3t, h=1)



# Compute bias, pbias, and MAPE
#Note that bias3=(-ME) and pbias3=(-MPE)
accuracy(naive3)
forecast3 <- window(naive3$fitted, start=ffcast3)
observed3 <- window(dailyPeaksWFEC.ts, start=ffcast3)



# Show observed and forecasts as of Feb 2020
plot(observedS, ylab="Monthly demand (TWh)")
lines(forecastS, col="red")
lines(window(forecast, start=ffcastS), col="blue")
lines(window(forecast3, start=ffcastS), col="cyan")
legend("bottomleft",
       legend=c("Observed","Naive","Seasonal naive","3-day mean"),
       col=c("black","blue","red","cyan"), lty=1)



print(accuracy(naive1))
print(accuracy(naiveS))
print(accuracy(naive3))