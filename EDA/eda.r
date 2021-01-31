# EDA for Part 1 of term project
#
#
#
#

#install timeseries package

library(timeSeries)
library(timeDate)
install.packages("dplyr", repos='http://cran.us.r-project.org')
library(dplyr)

# load the R data
load('C:/Users/franc/Documents/HEC_MONTREAL/COURSES/2020/winter/forecastingMethods/termProject/sppdata.rdata')
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

## target variable is missed for Dec 12 2018

## check type of the date column
typeof(sppData$GMT.x..i..)

# convert it to a date type
sppData$GMT.x..i.. = as.POSIXct(sppData$GMT.x..i..)

sppData$CST =  format(sppData$GMT.x..i.., tz="America/North_Dakota/Center",usetz=TRUE)

sppData$GMT.x..i

# Create Day and Month Variable
sppData$Month = format(sppData$GMT.x..i..,"%m")
sppData$Day = format(sppData$GMT.x..i..,"%d")





# export as a csv

write.csv(sppData,'C:/Users/franc/Documents/HEC_MONTREAL/COURSES/2020/winter/forecastingMethods/termProject/sppdata.rdatasppData.csv', row.names = FALSE)


sum(is.na(sppdata$WFEC)) # counting the number of na in column WFEC







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





