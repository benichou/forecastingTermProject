# EDA for Part 1 of term project
#
#
#
#

#install timeseries package

library(timeSeries)
library(timeDate)

# load the R data
load('C:/Users/franc/Documents/HEC_MONTREAL/COURSES/2020/winter/forecastingMethods/termProject/sppdata.rdata')

head(sppdata)

sppData = data.frame(Y=as.matrix(sppdata), date=time(sppdata)) # transform time series object into an R dataframe

# export as a csv

write.csv(sppData,'C:/Users/franc/Documents/HEC_MONTREAL/COURSES/2020/winter/forecastingMethods/termProject/sppdata.rdatasppData.csv', row.names = FALSE)

<<<<<<< Updated upstream

sum(is.na(sppdata$WFEC)) # counting the number of na in column WFEC
=======
###################################
library(timeSeries)
library(forecast)
options(digits=3)
aggregatedDailyPeaksWFEC = aggregate(Y.WFEC ~ sppData$Date, sppData, max)
plot(aggregatedDailyPeaksWFEC)

aggregatedDailyPeaksWFEC_month = aggregate(Y.WFEC ~ sppData$Month, sppData, max)
plot(aggregatedDailyPeaksWFEC_month)

head(aggregatedDailyPeaksWFEC)
plot(aggregatedDailyPeaksWFEC)

#transform into ts time series
aggregatedDailyPeaksWFEC.ts <-ts(aggregatedDailyPeaksWFEC$Y.WFEC,start = c(2013,10),end = c(2018,9),frequency =365)
plot(aggregatedDailyPeaksWFEC.ts)
print(aggregatedDailyPeaksWFEC.ts)

# (1) Compute naive/no-change/random walk/persistence forecast
# First forecast is for Oct 2014
ffcast <- c(2014,10)
naive1 <- naive(aggregatedDailyPeaksWFEC.ts, h=1)

# Compute bias, pbias, and MAPE
#Note that bias1=(-ME) and pbias1=(-MPE)
accuracy(naive1)
#------------------------------------------------

# (2) Compute seasonal naive / seasonal no-change
# First forecast is for Oct 2014
ffcastS <- c(2014,10)
naiveS <- snaive(aggregatedDailyPeaksWFEC.ts, h=1)

# Compute bias, pbias, and MAPE
#Note that bias1=(-ME) and pbias1=(-MPE)
accuracy(naiveS)
#------------------------------------------------

# (3) Compute rolling three-month mean
# First forecast is for Oct 2014
ffcast3 <- c(2014,10)
# Calculate three-day mean and store in last of the three days
naive3t <- zoo::rollmean(aggregatedDailyPeaksWFEC.ts, 3, align="right")
# Use function naive to move three-day mean forward by one day
naive3 <- naive(naive3t, h=1)

# Compute bias, pbias, and MAPE
#Note that bias1=(-ME) and pbias1=(-MPE)
accuracy(naive3)
>>>>>>> Stashed changes

