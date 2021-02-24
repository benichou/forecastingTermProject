#install timeseries package

library(timeSeries)
library(timeDate)


# load the R data
filePath = '../data/sppdata.rdata'
load(filePath)
head(sppdata)

# create a dataframe from the time series object
# transform time series object into an R dataframe
sppData = data.frame(Y=as.matrix(sppdata), date=time(sppdata))

# get the name of the columns in the dataframe
colNamesList = c(colnames(sppData))

# WFEC(target variable) and the time series are at position 11 and 12
ourColumns = colNamesList[11:12]

# limit the sppData dataframe to WFEC (team WFEC) and the time series
sppData = sppData[c(ourColumns[1], ourColumns[2])]
head(sppData)

# check whether there are missing values
if(any(is.na(sppData))){
  print("There is missing data in the DataFrame")
}

# identify the rows with missing value

identifyMissingRows <- function(df) {
  missingRows = df[is.na(df$Y.WFEC),]
  return(missingRows)
}
identifyMissingRows(sppdata)

# target variable is missed for Dec 12 2018 from 2am to 7am (CST) 
# and on Sept 27 2017 at 7pm

# check type of the date column
typeof(sppData$GMT.x..i..)

# convert it to a datetime type with CST time zone
sppData$GMT.x..i.. = as.POSIXct(sppData$GMT.x..i..)

sppData$CSTTime = format(sppData$GMT.x..i.., 
                         tz="America/North_Dakota/Center",usetz=TRUE)
# check it is the CST time zone
head(sppData$CSTTime)

# Create Day and Month Variable
sppData$Year = as.integer(strftime(sppData$CSTTime, "%Y"))
sppData$Month = as.integer(strftime(sppData$CSTTime, "%m"))
sppData$Day = as.integer(strftime(sppData$CSTTime, "%d"))
sppData$Hour = as.integer(strftime(sppData$CSTTime, "%H"))

head(sppData)


# impute the right WFEC demand for each missed hour of Dec 12 2018 
# based on the average of the 3 previous years for each hour
# select the 2am to 7am for Dec 12 for year 2015, 2016, 2017

PrevYearsData = sppData[2015<=sppData$Year & sppData$Year<=2017 &
                          sppData$Month==12 & sppData$Day==12 &
                          sppData$Hour>=2 & sppData$Hour<=7, ]

# get the average for each of the hours from 8am to 13pm 
# on the Dec 12th on 2015, 2016, 2017 (3 years prior to 2018)
averageForMissingDataDec12 = aggregate( Y.WFEC ~ Hour, 
                                        PrevYearsData, mean)

# fill out the missing values with the results of the average demand
# for the 3 previous years for each hour
imputeData = averageForMissingDataDec12[,"Y.WFEC"]
sppData[is.na(sppData$Y.WFEC) & 
          sppData$Month==12, "Y.WFEC"] = imputeData

# check there are no more missing values on Dec 12
if(sum(sppData[is.na(sppData$Y.WFEC) & 
               sppData$Month==12, "Y.WFEC"]) == 0){
  print("The missing rows on Dec 12 have been imputed.")
}

sppData[sppData$Year==2018 & sppData$Month==12 & sppData$Day==12, ]

# Identify the last missing rows
identifyMissingRows(sppData)

# impute the right WFEC demand for 7pm Sep 27 2017
# select 7pm for Sep 27 for year 2014, 2015, 2016
PrevYearsSep28 = sppData[2014<=sppData$Year & sppData$Year<=2016 &
                           sppData$Month==9 & sppData$Day==27 & 
                           sppData$Hour==19, ]

# get the average for each of the hours from 8am to 13pm 
# on the Dec 12th on 2015, 2016, 2017 (3 years prior to 2018)
averageForSep28 = aggregate( Y.WFEC ~ Hour, PrevYearsSep28, mean )

# fill out the missing values with the results of the average demand
# for the 3 previous years for each hour
sppData[is.na(sppData$Y.WFEC) & 
          sppData$Year==2017 & 
          sppData$Month==9 & 
          sppData$Day==27, "Y.WFEC"] = averageForSep28[,"Y.WFEC"]

# make sure there are no more missing data anywhere 
identifyMissingRows(sppData)
sum(is.na(sppData))
# none
# 0

# transform the CSTTime as a date 
# before doing the grouping by max for daily peak 
sppData$Date = as.Date(sppData$CSTTime)

## aggregate the data by day and then pick the max 
dailyPeaksWFEC = aggregate(Y.WFEC ~ sppData$Date, 
                           sppData, 
                           max)