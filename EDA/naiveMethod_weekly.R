#********************************************************#
#   Naive Method   
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

stdate <- c(1,1)

train.ts <- ts(dailyPeaksWFEC$Y.WFEC[1:(ndTrain)],
               start=stdate, frequency=7) 

# ts for Naive method, data starts at 2016/12/31 =(ndTrain)th day
# in order to provide forecast for Jan. 1, 2017
validateN.ts <- ts(dailyPeaksWFEC$
                    Y.WFEC[(ndTrain):(ndTrain+ndValidate)],
                  start=stdate, frequency=7)

# ts for Seasonal Naive, data starts at 2016/12/25 =(ndTrain-6)th day
# in order to provide forecast for Jan. 1, 2017
validateSN.ts <- ts(dailyPeaksWFEC$
                     Y.WFEC[(ndTrain-6):(ndTrain+ndValidate)],
                   start=stdate, frequency=7)

# ts for Moving Average, data starts at 2016/12/29 =(ndTrain-2)th day
# in order to provide forecast for Jan. 1, 2017
validateMA.ts <- ts(dailyPeaksWFEC$
                     Y.WFEC[(ndTrain-2):(ndTrain+ndValidate)],
                   start=stdate, frequency=7)

#==========================================================
# (1) Naive forecast (one day before)
#==========================================================
ffcastN <- c(1,2)

#------------------
# Training set
#------------------
naiveTrain <- naive(train.ts, h=1)

fcastTrain <- window(naiveTrain$fitted, start=ffcastN) 
obsTrain <- window(naiveTrain$x, start=ffcastN)

#------------------
# Validation set
#------------------
naiveValidate <- naive(validateN.ts, h=1)

fcastVal <- window(naiveValidate$fitted, start=ffcastN) 
obsVal <- window(naiveValidate$x, start=ffcastN)

#==========================================================
# (2) Seasonal Naive forecast (one week before)
# snaive automatically uses a weekly seasonality since 
# train.ts & validateSN.ts were built specifying frequency=7.
#==========================================================
ffcastSN <- c(2,1)

#------------------
# Training set
#------------------
naiveSTrain <- snaive(train.ts, h=1)

fcastSTrain <- window(naiveSTrain$fitted, start=ffcastSN) 
obsSTrain <- window(naiveSTrain$x, start=ffcastSN)

#------------------
# Validation set
#------------------
naiveSValidate <- snaive(validateSN.ts, h=1)

fcastSVal <- window(naiveSValidate$fitted, start=ffcastSN) 
obsSVal <- window(naiveSValidate$x, start=ffcastSN)

#==========================================================
# (3) Moving Average forecast (rolling three-day mean)
#==========================================================
ffcastMA <- c(1,4)

#------------------
# Training set
#------------------
# Calculate three-day mean and store in last of the three days
naive3tTrain <- zoo::rollmean(train.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naive3Train <- naive(naive3tTrain, h=1)

fcastMATrain <- window(naive3Train$fitted, start=ffcastMA) 
obsMATrain <- window(train.ts, start=ffcastMA)

#------------------
# Validation set
#------------------
# Calculate three-day mean and store in last of the three days
naive3tVal <- zoo::rollmean(validateMA.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naive3Validate <- naive(naive3tVal, h=1)

fcastMAVal <- window(naive3Validate$fitted, start=ffcastMA)
obsMAVal <- window(validateMA.ts, start=ffcastMA)


#******************************************************************
# Performance can  be obtained from <accuracy>.
# Note that bias1=(-ME) and pbias1=(-MPE)
cat("Training Set Performance using function <accuracy>:","\n")
print(accuracy(fcastTrain, obsTrain)[,1:5])     # naive
print(accuracy(fcastSTrain, obsSTrain)[,1:5])   # seasonal naive
print(accuracy(fcastMATrain, obsMATrain)[,1:5]) # moving average

cat("Validation Set Performance using function <accuracy>:","\n")
print(accuracy(fcastVal, obsVal)[,1:5])     # naive
print(accuracy(fcastSVal, obsSVal)[,1:5])   # seasonal naive
print(accuracy(fcastMAVal, obsMAVal)[,1:5]) # moving average
