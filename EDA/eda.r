# EDA for Part 1 of term project
#
#
#
#

#######################################################################################
#                                                                                     #
#                                 TEAM F - Data Wrangling                             #
#                                                                                     #
#                                                                                     #
#######################################################################################

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
sppData = select(sppData, ourColumns[1], ourColumns[2])
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





## notes for the explanatory variables part

## identify the missing data
## add useful variables
## count of tornadoes per day
#gmt conversion
# daily peak identification
## weighted average of the temperature by the proportion of the population in each counties
## day light hours
## weekday indicator
## week end indicator
## Summer Indicator
# Not Summer Indicator#
## Covid Days
## Not Covid Days
## wind intensity
## tornado indicator
## severe storms indicator
## relative humidity indicator
## observed temperature for the day of reference
## Holidays and Close to holidays indicators
## precipitation

# creation of the moving average (7 days, 30 days, 90 days, 365 days)


## Target Analysis of the distibution


## Analysis of the seasonality and trends
## Seasonality analysis: Distribution over quarters
## ("Seasonality analysis: Distribution over weekdays")








## Univariate distribution of the demand for WFEC


## train test split

train.ts <- window(demand.ts, start = c(1998,12), end = c(2014,9))
test.ts <- window(demand.ts, start = c(2014,10), end = c(2018,9))



## seasonal plot 

ggseasonplot(train.ts)





