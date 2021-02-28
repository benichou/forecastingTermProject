#********************************************************#
#       Naive Method   
#********************************************************#

library(timeSeries)
library(forecast)
options(digits=3)
source('dataAggregation.r')

# Create 'ts' time series as required by 'naive' and 'snaive'
# Assumed weekly seasonality, so frequency=7

# To divide data into training, validation and test sets
# Training set  : 2011/01/01-2016/12/31
# Validation set: 2017/01/01-2018/12/31
# Test set      : 2019/01/01-2020/12/31

ndTrain = 2192    # 6 yrs * 365 + 2 extra days in 2012 & 2016
ndValidate = 730  # 2 yrs * 365
ndTest = 731      # 365 (in 2019) + 366 (in 2020)


# Data for validation.ts includes observed values from 2016/12/29
# (for 3-day moving average)
validate.ts <- ts(dailyPeaksWFEC$
                    Y.WFEC[(ndTrain-2):(ndTrain+ndValidate)],
                  start=c(2016,363), frequency=365)

# Data for validateWK.ts includes observed values from 2016/12/25
# (for weekly seasonality)
validateWK.ts <- ts(dailyPeaksWFEC$
                     Y.WFEC[(ndTrain-6):(ndTrain+ndValidate)],
                   start=c(1,1), frequency=7)

#==========================================================
# (1) Naive forecast (one day before)
#==========================================================
ffcastN <- c(2017,1)

naiveVal <- naive(validate.ts, h=1)

# Compute bias, pbias, and MAPE
fcastN <- window(naiveVal$fitted, start=ffcastN) 
obsN <- window(naiveVal$x, start=ffcastN)

#==========================================================
# (2) Seasonal Naive forecast (one week before)
# snaive automatically uses a weekly seasonality since 
# train.ts & validateSN.ts were built specifying frequency=7.
#==========================================================
ffcastSW <- c(2,1)

naiveSW <- snaive(validateWK.ts, h=1)

fcastSW <- window(naiveSW$fitted, start=ffcastSW) 
obsSW <- window(naiveSW$x, start=ffcastSW)

#==========================================================
# (3) Moving Average forecast (rolling three-day mean)
#==========================================================

# Calculate three-day mean and store in last of the three days
naive3tVal <- zoo::rollmean(validate.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naiveMA <- naive(naive3tVal, h=1)

# Compute bias, pbias, and MAPE
fcastMA <- window(naiveMA$fitted, start=ffcastN)
# use original ts for the observed values
obsMA <- window(validate.ts, start=ffcastN)

#==========================================================
# Performance can be obtained from <accuracy>.
# Note that bias1=(-ME) and pbias1=(-MPE)

val.eval <- rbind(accuracy(fcastN, obsN)[,1:5],
                  accuracy(fcastSW, obsSW)[,1:5],
                  accuracy(fcastMA, obsMA)[,1:5])
rownames(val.eval) <- make.names(paste("Naive",c("","Seasonal","MovingAvg")))
cat("Validation Set Performance using function <accuracy>:","\n")
print(val.eval)
