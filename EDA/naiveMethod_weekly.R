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

stdate <- c(1,1)

train.ts <- ts(dailyPeaksWFEC$Y.WFEC[1:(ndTrain)],
               start=stdate, frequency=7) 

validateN.ts <- ts(dailyPeaksWFEC$
                    Y.WFEC[(ndTrain):(ndTrain+ndValidate)],
                  start=stdate, frequency=7)

validateSN.ts <- ts(dailyPeaksWFEC$
                     Y.WFEC[(ndTrain-6):(ndTrain+ndValidate)],
                   start=stdate, frequency=7)

validateMA.ts <- ts(dailyPeaksWFEC$
                     Y.WFEC[(ndTrain-2):(ndTrain+ndValidate)],
                   start=stdate, frequency=7)

# test.ts <- ts(dailyPeaksWFEC$
#                 Y.WFEC[(ndTrain+ndValidate+1):
#                          (ndTrain+ndValidate+ndTest)],
#               start=stdate, frequency=7) 

#==========================================================
# (1) Naive forecast (one day before)
#==========================================================
ffcastN <- c(1,2)

#------------------
# Training set
#------------------
naiveTrain <- naive(train.ts, h=1)

# Compute bias, pbias, and MAPE
fcastTrain <- window(naiveTrain$fitted, start=ffcastN) 
obsTrain <- window(naiveTrain$x, start=ffcastN)
bias1Train  <- mean(fcastTrain-obsTrain)
pbias1Train <- mean((fcastTrain-obsTrain)/obsTrain)*100
mape1Train  <- mean(abs((fcastTrain-obsTrain)/obsTrain)*100)

#------------------
# Validation set
#------------------
naiveValidate <- naive(validateN.ts, h=1)

# Compute bias, pbias, and MAPE
fcastVal <- window(naiveValidate$fitted, start=ffcastN) 
obsVal <- window(naiveValidate$x, start=ffcastN)
bias1Val  <- mean(fcastVal-obsVal)
pbias1Val <- mean((fcastVal-obsVal)/obsVal)*100
mape1Val  <- mean(abs((fcastVal-obsVal)/obsVal)*100)

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

# Compute bias, pbias, and MAPE
fcastSTrain <- window(naiveSTrain$fitted, start=ffcastSN) 
obsSTrain <- window(naiveSTrain$x, start=ffcastSN)
biasSTrain  <- mean(fcastSTrain-obsSTrain)
pbiasSTrain <- mean((fcastSTrain-obsSTrain)/obsSTrain)*100
mapeSTrain  <- mean(abs((fcastSTrain-obsSTrain)/obsSTrain)*100)

#------------------
# Validation set
#------------------
naiveSValidate <- snaive(validateSN.ts, h=1)

# Compute bias, pbias, and MAPE
fcastSVal <- window(naiveSValidate$fitted, start=ffcastSN) 
obsSVal <- window(naiveSValidate$x, start=ffcastSN)
biasSVal  <- mean(fcastSVal-obsSVal)
pbiasSVal <- mean((fcastSVal-obsSVal)/obsSVal)*100
mapeSVal  <- mean(abs((fcastSVal-obsSVal)/obsSVal)*100)

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


# Compute bias, pbias, and MAPE
fcastMATrain <- window(naive3Train$fitted, start=ffcastMA) 
obsMATrain <- window(train.ts, start=ffcastMA)
biasMATrain  <- mean(fcastMATrain-obsMATrain)
pbiasMATrain <- mean((fcastMATrain-obsMATrain)/obsMATrain)*100
mapeMATrain  <- mean(abs((fcastMATrain-obsMATrain)/obsMATrain)*100)

#------------------
# Validation set
#------------------
# Calculate three-day mean and store in last of the three days
naive3tVal <- zoo::rollmean(validateMA.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naive3Validate <- naive(naive3tVal, h=1)

# Compute bias, pbias, and MAPE
fcastMAVal <- window(naive3Validate$fitted, start=ffcastMA)
obsMAVal <- window(validateMA.ts, start=ffcastMA)
biasMAVal  <- mean(fcastMAVal-obsMAVal) 
pbiasMAVal <- mean((fcastMAVal-obsMAVal)/obsMAVal)*100
mapeMAVal  <- mean(abs((fcastMAVal-obsMAVal)/obsMAVal)*100)


#==========================================================
# cat(paste("Results for training set 2011-2016:", "\n"))
# cat("         no-change: bias=",bias1Train,
#     "%bias=",pbias1Train,"mape=",mape1Train,"\n")
# cat(paste("Results for validation set 2017-2018:", "\n"))
# cat("         no-change: bias=",bias1Val,
#     "%bias=",pbias1Val,"mape=",mape1Val,"\n")
# 
# cat(paste("Results for training set 2011-2016:", "\n"))
# cat("seasonal no-change: bias=",biasSTrain,
#      "%bias=",pbiasSTrain,"mape=",mapeSTrain,"\n")
# cat(paste("Results for validation set 2017-2018:", "\n"))
# cat("seasonal no-change: bias=",biasSVal,
#     "%bias=",pbiasSVal,"mape=",mapeSVal,"\n")
# 
# cat(paste("Results for training set 2011-2016:", "\n"))
# cat("rolling 3-day mean: bias=",biasMATrain, 
#     "%bias=",pbiasMATrain,"mape=",mapeMATrain,"\n\n")
# 
# cat(paste("Results for validation set 2017-2018:", "\n"))
# cat("rolling 3-day mean: bias=",biasMAVal, 
#     "%bias=",pbiasMAVal,"mape=",mapeMAVal,"\n\n")


# Performance can also be obtained from <accuracy>.
# Note that bias1=(-ME) and pbias1=(-MPE)
cat("Training Set Performance using function <accuracy>:","\n")
print(accuracy(fcastTrain, obsTrain)[,1:5])     # naive
print(accuracy(fcastSTrain, obsSTrain)[,1:5])   # seasonal naive
print(accuracy(fcastMATrain, obsMATrain)[,1:5]) # moving average

cat("Validation Set Performance using function <accuracy>:","\n")
print(accuracy(fcastVal, obsVal)[,1:5])     # naive
print(accuracy(fcastSVal, obsSVal)[,1:5])   # seasonal naive
print(accuracy(fcastMAVal, obsMAVal)[,1:5]) # moving average
