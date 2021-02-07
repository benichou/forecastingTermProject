#install timeseries package

library(timeSeries)
library(timeDate)
# install.packages("dplyr", repos='http://cran.us.r-project.org')
# library(dplyr)

# load the R data
filePath = 'C:/Users/franc/Documents/HEC_MONTREAL/COURSES/2020/winter/forecastingMethods/termProject/sppdata.rdata'
load(filePath)
head(sppdata)

## create a dataframe from the time series object
sppData = data.frame(Y=as.matrix(sppdata), date=time(sppdata)) # transform time series object into an R dataframe

## get the name of the columns in the dataframe

colNamesList = c(colnames(sppData))
ourColumns = colNamesList[11:12] ## FEC (target variable) and the time series are at position 11 and 12
## limit the sppData dataframe to WFEC (team WFEC) and the time series
sppData = sppData[c(ourColumns[1], ourColumns[2])]
head(sppData)
## identify whether there are missing values and print message if it the case

if(any(is.na(sppData))){
  print("There is missing data in the DataFrame")
}
## identify the rows with missing value


identifyMissingRows <- function(df) {
  missingRows = df[is.na(df$Y.WFEC),]
  return(missingRows)
}
identifyMissingRows(sppData)

## target variable is missed for Dec 12 2018 from 2am to 7am (CST) and on Sept 27 2017 at 7pm

## check type of the date column
typeof(sppData$GMT.x..i..)

# convert it to a datetime type with CST time zone
sppData$GMT.x..i.. = as.POSIXct(sppData$GMT.x..i..)

sppData$CSTTime = format(sppData$GMT.x..i.., tz="America/North_Dakota/Center",usetz=TRUE)
## check it is the CST time zone
head(sppData$CSTTime)

# Create Day and Month Variable
sppData$Year = strftime(sppData$CSTTime, "%Y")
sppData$Month = strftime(sppData$CSTTime, "%m")
sppData$Day = strftime(sppData$CSTTime, "%d")
sppData$Hour = strftime(sppData$CSTTime, "%H")

head(sppData)


## impute the right WFEC demand for each missed hour of Dec 12 2018 based on the average of the 3 previous years for each hour
## select the 2am to 7am for Dec 12 for year 2015, 2016, 2017

threePreviousYearsDec12 = sppData[sppData$CSTTime >= as.Date("2015-12-12") & sppData$CSTTime<=as.Date("2017-12-13") & as.numeric(sppData$Month) == 12 & as.numeric(sppData$Day) == 12 & as.numeric(sppData$Hour) >= 2 & as.numeric(sppData$Hour) <= 7, ]
## get the average for each of the hours from 8am to 13pm on the Dec 12th on 2015, 2016, 2017 (3 years prior to 2018)
averageForMissingDataDec12 = aggregate( Y.WFEC ~ Hour, threePreviousYearsDec12, mean )
## fill out the missing values with the results of the average demand for the 3 previous years for each hour
sppData[is.na(sppData$Y.WFEC) & as.numeric(sppData$Month == 12), "Y.WFEC"] = averageForMissingDataDec12[,"Y.WFEC"]

## check there are no more missing values on Dec 12

if(sum(sppData[is.na(sppData$Y.WFEC) & sppData$Month == 12, "Y.WFEC"]) == 0){
  print("The missing rows on Dec 12 have been imputed by their average of the 3 previous years")
}

sppData[as.numeric(sppData$Year) == 2018 & as.numeric(sppData$Month) == 12 & as.numeric(sppData$Day) == 12, ]

## Identify the last missing rows
identifyMissingRows(sppData)

## impute the right WFEC demand for 7pm Sep 27 2017
## select 7pm for Sep 27 for year 2014, 2015, 2016

threePreviousYearsSep28 = sppData[sppData$CSTTime >= as.Date("2014-09-27") & sppData$CSTTime<=as.Date("2016-09-28") & as.numeric(sppData$Month) == 9 & as.numeric(sppData$Day) == 27 & as.numeric(sppData$Hour) == 19, ]
## get the average for each of the hours from 8am to 13pm on the Dec 12th on 2015, 2016, 2017 (3 years prior to 2018)
averageForMissingDataSep28 = aggregate( Y.WFEC ~ Hour, threePreviousYearsSep28, mean )
## fill out the missing values with the results of the average demand for the 3 previous years for each hour
sppData[is.na(sppData$Y.WFEC) & as.numeric(sppData$Year) == 2017 & as.numeric(sppData$Month) == 9 & as.numeric(sppData$Day) == 27, "Y.WFEC"] = averageForMissingDataSep28[,"Y.WFEC"]

## make sure there are no more missing data anywhere 
identifyMissingRows(sppData)
sum(is.na(sppData))
# none
# 0

## transform the CSTTime as a date before doing the grouping by max for daily peak 
sppData$Date = as.Date(sppData$CSTTime)

## aggregate the data by day and then pick the max 
aggregatedDailyPeaksWFEC = aggregate(Y.WFEC ~ sppData$Date, sppData, max)


# export as a csv
yourPath = 'C:/Users/franc/Documents/HEC_MONTREAL/COURSES/2020/winter/forecastingMethods/termProject/aggregatedDailyPeaksWFEC.csv'
write.csv(aggregatedDailyPeaksWFEC, yourPath, row.names = FALSE)
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
forecast <- window(naive1$fitted, start=ffcast) 
observed <- window(naive1$x, start=ffcast)
#------------------------------------------------

# (2) Compute seasonal naive / seasonal no-change
# First forecast is for Oct 2014
ffcastS <- c(2014,10)
naiveS <- snaive(aggregatedDailyPeaksWFEC.ts, h=1)

# Compute bias, pbias, and MAPE
#Note that bias1=(-ME) and pbias1=(-MPE)
accuracy(naiveS)
forecastS <- window(naiveS$fitted, start=ffcastS)
observedS <- window(naiveS$x, start=ffcastS)
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
forecast3 <- window(naive3$fitted, start=ffcast3)
observed3 <- window(aggregatedDailyPeaksWFEC.ts, start=ffcast3)

# Show observed and forecasts as of Jan 2016
plot(observedS, ylab="Monthly demand (TWh)")
lines(forecastS, col="red")
lines(window(forecast, start=ffcastS), col="blue")
lines(window(forecast3, start=ffcastS), col="cyan")
legend("bottomleft", 
       legend=c("Observed","Naive","Seasonal naive","3-day mean"),
       col=c("black","blue","red","cyan"), lty=1)
