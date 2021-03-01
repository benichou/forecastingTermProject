#********************************************************#
#       Naive Method   
#********************************************************#

library(timeSeries)
library(forecast)
options(digits=3)
source('EDA/dataAggregation.r')

pdf("naiveMethod.pdf")

# Training set  : 2014/01/01-2016/12/31
# Validation set: 2017/01/01-2018/12/31
# Test set      : 2019/01/01-2020/12/31
# Naive method does not require training

# Removing February 29 (2012/02/29, 2016/02/29 & 2020/02/29)
# to facilitate annual seasonality (frequency = 365)
WFEC365 <- dailyPeaksWFEC[-c(425,1886,3347),]
rownames(WFEC365) <- WFEC365$DATE # to make sure the index is dates

# Create 'ts' time series as required by 'naive' and 'snaive'
WFEC365.ts <- ts(WFEC365$Y.WFEC, start=c(2011,1), frequency=365)

# For WEEKLY seasonality only (frequency = 7):
# validation set includes observations from 2016/12/25 to 2018/12/31 
validateWK.ts <- ts(WFEC365$Y.WFEC[(365*6-6):(365*8)],
                    start=c(1,1), frequency=7)

# First forecast = 2017/01/01 & last forecast  = 2018/12/31
# (based on validation period)
ffcastN <- c(2017,1)
lfcastN <- c(2018,365)

#==========================================================
# (1) Naive forecast (one day before)
#==========================================================

naiveVal <- naive(WFEC365.ts, h=1)
fcastN <- window(naiveVal$fitted, start=ffcastN, end=lfcastN) 
obsN <- window(naiveVal$x, start=ffcastN, end=lfcastN)

#==========================================================
# (2) Seasonal Naive forecast
#==========================================================

# Annual seasonality
naiveSY <- snaive(WFEC365.ts, h=1)
fcastSY <- window(naiveSY$fitted, start=ffcastN, end=lfcastN)
obsSY <- window(naiveSY$x, start=ffcastN, end=lfcastN)

# Weekly seasonality
# snaive automatically uses a weekly seasonality when 
# ts is built specifying frequency=7.
ffcastWK <- c(2,1) # start predictions on second week first day
naiveSW <- snaive(validateWK.ts, h=1)
fcastSW <- window(naiveSW$fitted, start=ffcastWK) 
obsSW <- window(naiveSW$x, start=ffcastWK)

#==========================================================
# (3) Moving Average forecast (rolling three-day mean)
#==========================================================

# Calculate three-day mean and store in last of the three days
naive3tVal <- zoo::rollmean(WFEC365.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naiveMA <- naive(naive3tVal, h=1)
fcastMA <- window(naiveMA$fitted, start=ffcastN, end=lfcastN)
# use original ts for the observed values
obsMA <- window(WFEC365.ts, start=ffcastN, end=lfcastN)

#==========================================================
# Performance can be obtained from <accuracy>.
# Note that bias1=(-ME) and pbias1=(-MPE)

val.eval <- rbind(accuracy(fcastN, obsN)[,1:5],
                  accuracy(fcastSY, obsSY)[,1:5],
                  accuracy(fcastSW, obsSW)[,1:5],
                  accuracy(fcastMA, obsMA)[,1:5])
rownames(val.eval) <- make.names(
  c("Naive","SNaive_Annual","SNaive_Weekly","Moving_Avg"))

cat("Validation Set Performance using function <accuracy>:","\n")
print(val.eval)

# Re-configure ts of weekly seasonal forecast for the plot
fcastSW <- ts(fcastSW, start=c(2017,1), frequency = 365)

# Show observed and forecasts for Jan 2017 to Dec 2018
plot(obsN, ylab="WFEC daily peak demand (MWh)")
lines(fcastSW, col="red")
lines(fcastSY, col="orange")
lines(fcastN, col="blue")
lines(fcastMA, col="cyan")

legend("topright", 
       legend=c("observed","naive","annual naive", 
                "weekly naive","3-day mean"),
       col=c("black","blue","orange","red","cyan"), lty=1)

dev.off(dev.cur())