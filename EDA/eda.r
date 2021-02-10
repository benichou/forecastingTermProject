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

threePreviousYearsDec12 = sppData[sppData$CSTTime >= as.Date("2015-12-12") & 
                                  sppData$CSTTime<=as.Date("2017-12-13") &
                                  as.numeric(sppData$Month) == 12 & 
                                  as.numeric(sppData$Day) == 12 & 
                                  as.numeric(sppData$Hour) >= 2 & 
                                  as.numeric(sppData$Hour) <= 7, ]
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

threePreviousYearsSep28 = sppData[sppData$CSTTime >= as.Date("2014-09-27") & 
                                  sppData$CSTTime<=as.Date("2016-09-28") & 
                                  as.numeric(sppData$Month) == 9 & 
                                  as.numeric(sppData$Day) == 27 & 
                                  as.numeric(sppData$Hour) == 19, ]
## get the average for each of the hours from 8am to 13pm on the Dec 12th on 2015, 2016, 2017 (3 years prior to 2018)
averageForMissingDataSep28 = aggregate( Y.WFEC ~ Hour, threePreviousYearsSep28, mean )
## fill out the missing values with the results of the average demand for the 3 previous years for each hour
sppData[is.na(sppData$Y.WFEC) & 
        as.numeric(sppData$Year) == 2017 & 
        as.numeric(sppData$Month) == 9 & 
        as.numeric(sppData$Day) == 27, "Y.WFEC"] = averageForMissingDataSep28[,"Y.WFEC"]

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




################################################################################
#                                                                              #
#                Data Exploration Y vs Explanatory variables                   #
#                                                                              # 
#                                                                              #
################################################################################

## before we work with the temperature data, we will need to check that 
## the different stations spanning across the WFEC territory show consistent numbers
## If they do not show consistent numbers, then we we should consider some sort of weithing 
## of the temperature per station and their associated territories (based on the energy size consumption of each territory)


meteoDataPath ='C:/Users/franc/Documents/HEC_MONTREAL/COURSES/2020/winter/forecastingMethods/termProject/meteoFile.csv' ## from noaa site

meteoData = read.csv(meteoDataPath)
head(meteoData)
## We only select the meteorological data of the following stations out of the meteoData frame because the other stations do not cover the entire 2011 2021 period or have too big missing gaps

blanchardStation = meteoData[meteoData['NAME'] == 'BLANCHARD 2 SSW, OK US',  
                            c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

cherokeeStation = meteoData[meteoData['NAME'] == 'CHEROKEE 1 SSW MESONET, OK US',  
                            c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

clovisNewMexicoStation = meteoData[meteoData['NAME'] == 'CLOVIS 13 N, NM US',  
                                  c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

seminoleOkStation = meteoData[meteoData['NAME'] == 'SEMINOLE 4 SSW MESONET, OK US',  
                              c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

tiptonOkStation = meteoData[meteoData['NAME'] == 'TIPTON 4 S, OK US',  
                            c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

# only keep the stations you need in a filtered meteoData dataframe

stationList = c('BLANCHARD 2 SSW, OK US', 
                'CHEROKEE 1 SSW MESONET, OK US', 
                'CLOVIS 13 N, NM US', 
                'SEMINOLE 4 SSW MESONET, OK US', 
                'TIPTON 4 S, OK US')

filteredMeteoData = meteoData[meteoData$NAME %in% stationList, c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

summary(filteredMeteoData)

## We can see here that TOBS has 8950 NA's, that TMAX has 75 NA, that TMIN has 79 NA and that PRCP (precipitation level) has 633 NA
## For the sake of our visualizations, we will remove all rows with NA in 

filteredMeteoDataViz = filteredMeteoData[complete.cases(filteredMeteoData[ , 5:6]),]

filteredMeteoDataViz[filteredMeteoDataViz['NAME'] == 'BLANCHARD 2 SSW, OK US', c("NAME")] = "Blanchard Station"
filteredMeteoDataViz[filteredMeteoDataViz['NAME'] == 'CHEROKEE 1 SSW MESONET', c("NAME")] = "Cherokee Station"
filteredMeteoDataViz[filteredMeteoDataViz['NAME'] == 'CLOVIS 13 N, NM US', c("NAME")] = "Clovis Station"
filteredMeteoDataViz[filteredMeteoDataViz['NAME'] == 'SEMINOLE 4 SSW MESONET, OK US', c("NAME")] = "Seminole Station"
filteredMeteoDataViz[filteredMeteoDataViz['NAME'] == 'TIPTON 4 S, OK US', c("NAME")] = "Tipton Station"


pdf("EDA/viz/tempAcrossStations.pdf")


boxplot(TOBS~ NAME, data=filteredMeteoDataViz[, c("NAME", "TOBS")], 
                    names = c("Blanchard Station", "Cherokee Station", "Clovis Station", "Seminole Station", "Tipton Station"), xaxs = FALSE)  ## The box plot disctibutions of the different stations across the WFEC territories are similar

tempStations = c(blanchardStation$TOBS, 
                 clovisNewMexicoStation$TOBS, 
                 seminoleOkStation$TOBS, 
                 tiptonOkStation$TOBS)

datesStations = c(blanchardStation$DATE, 
                  clovisNewMexicoStation$DATE, 
                  seminoleOkStation$DATE, 
                  tiptonOkStation$DATE)
## all
tempSeriesAll = timeSeries(tempStations, datesStations, format="%Y-%m-%d")

plot(tempSeriesAll, ylab="Evolution of the Temperature in Celsius", main="Evolution of Temperature over 2011 to 2021 in OK, NM, KS, and NM For All Weather Stations") 
plot(tempSeriesAll, ylab="Evolution of the Temperature in Celsius", type="p", main="Evolution of Temperature over 2011 to 2021 in OK, NM, KS, and NM For All Weather Stations") 

## blanchard
tempSeriesBlanchard = timeSeries(blanchardStation$TOBS, blanchardStation$DATE, format="%Y-%m-%d")
plot(tempSeriesBlanchard, ylab="Evolution of the Temperature in Celsius", type="p", main="Evolution of Temperature over 2011 to 2021 - Blanchard Station") 

## New Mexico

tempSeriesNM = timeSeries(clovisNewMexicoStation$TOBS, clovisNewMexicoStation$DATE, format="%Y-%m-%d")
plot(tempSeriesNM, ylab="Evolution of the Temperature in Celsius", type="p", main="Evolution of Temperature over 2011 to 2021 - New Mexico Station") ## same usual trend in terms of temperature

## Seminole OK Station

tempSeriesSeminole = timeSeries(seminoleOkStation$TOBS, seminoleOkStation$DATE, format="%Y-%m-%d")
plot(tempSeriesSeminole, ylab="Evolution of the Temperature in Celsius", type="p", main="Evolution of Temperature over 2011 to 2021 - Seminole Station") ## same usual trend in terms of temperature

## Tipton
tempSeriesTipton = timeSeries(tiptonOkStation$TOBS, tiptonOkStation$DATE, format="%Y-%m-%d")
plot(tempSeriesTipton, ylab="Evolution of the Temperature in Celsius", type="p", main="Evolution of Temperature over 2011 to 2021 - Tipton Station") ## Follow same trend but we do not have enough data

## We again see that the temperature are quite close to one another and thus we could just pick one weather station for our territory out of the weather stations covering the WFEC territory

dev.off(dev.cur())






## create a table that group by stats of temperature average, std, min, max

# summary stats for Tobs in the stations
tapply(filteredMeteoData$TOBS, filteredMeteoData$NAME, summary) 

## There is a consistent distibution across the stations except with lower temp in New Mexico. Let us bear in mind that New Mexico does not represent a major part of our consumers

# summary precipitations for the stations
tapply(filteredMeteoData$PRCP, filteredMeteoData$NAME, summary)

## There is a consistent distibution across the stations except with lower preceipitaion levels in New Mexico. Let us bear in mind that New Mexico does not represent a major part of our consumers



## We will pick the Blanchard station because it is the most accurate and i
## s equidistant to all areas covered by WFEC in addition to having quite close temperature and precipitation levels compared
## to the other weather stations

head(blanchardStation)

## summary stats

summary(blanchardStation$TOBS) ## summary statistics for observed temperature for each day in the 2011-2021 period
summary(blanchardStation$TMIN) ## summary statistics for observed min temperature for each day in the 2011-2021 period
summary(blanchardStation$TMAX) ## summary statistics for observed max temperature for each day in the 2011-2021 period
summary(blanchardStation$PRCP) ## summary statistics for observed precipitation for each day in the 2011-2021 period


blanchardStation["Year"] = strftime(blanchardStation$DATE, "%Y")
blanchardStation["Month"] = strftime(blanchardStation$DATE, "%m")
blanchardStation["Day"] = strftime(blanchardStation$DATE, "%d")

## rename column to "DATE "
names(aggregatedDailyPeaksWFEC)[1] = "DATE"

aggregatedDailyPeaksWFEC["Year"] = strftime(aggregatedDailyPeaksWFEC$DATE, "%Y")
aggregatedDailyPeaksWFEC["Month"] = strftime(aggregatedDailyPeaksWFEC$DATE, "%m")
aggregatedDailyPeaksWFEC["Day"] = strftime(aggregatedDailyPeaksWFEC$DATE, "%d")


## Energy consumption frame + the meteoroligcal data BELOW


avgDailyWithMeteoData = merge(aggregatedDailyPeaksWFEC, blanchardStation, by=c("Year", "Month", "Day"), all.x=TRUE) ## left merge to make sure we do not lose any daily peaks 
## when we do not have any temperature data ## TODO: Imputation of missing data for modelling phase



## Identifying Temperature of Reference --> a reference temperature that should be chosen in an adequate way in
## order to separate the hot and cold ‘ends’ of the demand-temperature
## relationship


pdf("EDA/viz/frequencyOfTempBlanchard.pdf")


hist(avgDailyWithMeteoData$TOBS, main="Frequency of Observed Temperatures at Blanchard Station", xlab="Observed Temp (Celsius)")

dev.off(dev.cur())

## Binning

avgDailyWithMeteoData["TemperatureCategory"] = 0

# Very Cold
avgDailyWithMeteoData[!is.na(avgDailyWithMeteoData["TOBS"]) & 
                      avgDailyWithMeteoData["TOBS"] <= -10, c("TemperatureCategory")] = "A/ Very Cold (<=-10)"
# Cold
avgDailyWithMeteoData[!is.na(avgDailyWithMeteoData["TOBS"]) & 
                      avgDailyWithMeteoData["TOBS"] > -10 &
                      avgDailyWithMeteoData["TOBS"] <=0, c("TemperatureCategory")] = "B/ Cold (-10 to 0)"
# Little cold
avgDailyWithMeteoData[!is.na(avgDailyWithMeteoData["TOBS"]) & 
                      avgDailyWithMeteoData["TOBS"] > 0 &
                      avgDailyWithMeteoData["TOBS"] <=10, c("TemperatureCategory")] = "C/ Little Cold (0 to 10)"
## Mild
avgDailyWithMeteoData[!is.na(avgDailyWithMeteoData["TOBS"]) & 
                      avgDailyWithMeteoData["TOBS"] > 10 &
                      avgDailyWithMeteoData["TOBS"] <= 20, c("TemperatureCategory")] = "D/ Mild (10 to 20)"

## Hot
avgDailyWithMeteoData[!is.na(avgDailyWithMeteoData["TOBS"]) & 
                      avgDailyWithMeteoData["TOBS"] > 20 &
                      avgDailyWithMeteoData["TOBS"] <= 30, c("TemperatureCategory")] = "E/ Hot (20 to 30)"
## Very Hot
avgDailyWithMeteoData[!is.na(avgDailyWithMeteoData["TOBS"]) & 
                      avgDailyWithMeteoData["TOBS"] > 30, c("TemperatureCategory")] = "F/ Very Hot (More than 30)"


## It is very clear here that the temperature of reference is 


pdf("EDA/viz/visualizingTRefForCoolingAndHeating.pdf")


boxplot(Y.WFEC~ TemperatureCategory, data=avgDailyWithMeteoData[avgDailyWithMeteoData["TemperatureCategory"] != 0, c("TemperatureCategory", "Y.WFEC")], main="What temperatures drive the energy consumption in WFEC?")

plot(avgDailyWithMeteoData$TOBS, avgDailyWithMeteoData$Y.WFEC, ylab="Evolution of the Temperature in Celsius", type="l", main="What temperatures drive the energy consumption in WFEC?") ## Follow same trend but we do not have enough data
plot(avgDailyWithMeteoData$TOBS, avgDailyWithMeteoData$Y.WFEC, ylab="Evolution of the Temperature in Celsius", type="p", main="What temperatures drive the energy consumption in WFEC?") ## Follow same trend but we do not have enough data

dev.off(dev.cur())

## This helped us see that the temperature of reference for CDD (when cooling starts to be required in a building) might be around 20 degrees
## This also helped us see that the temperature of reference for HDD (when heating starts to be required in a building) might be around 0 to 5 degrees

# Let us just compare the different Y.WFEC energy consumption at different temperature level


## a reference temperature that should be chosen in an adequate way in
## order to separate the hot and cold ‘ends’ of the demand-temperature

## Summary statistics about the daily demand

summary(avgDailyWithMeteoData$Y.WFEC)


# summary statistics about the daily temperature in WFEC
summary(avgDailyWithMeteoData$TOBS)

# summary statistics about the daily precipitation levels in WFEC
summary(avgDailyWithMeteoData$PRCP)




# CDD (Temperature of reference is then 19 degrees for CDD)
## --> When temp is between 18 and 19, energy consumption is at 1198.274
mean(avgDailyWithMeteoData[avgDailyWithMeteoData$TOBS >= 18 &
                      avgDailyWithMeteoData$TOBS <= 19 &
                      !is.na(avgDailyWithMeteoData$TOBS), c("Y.WFEC")])

## --> When temp is between 19 and 20, energy consumption is at 1260.899 --> Biggest jump from one increase in celsius degree --> We shall set the Temperature of Reference to 19 degrees for air conditionning and irrigation works possibly
mean(avgDailyWithMeteoData[avgDailyWithMeteoData$TOBS >= 19 &
                      avgDailyWithMeteoData$TOBS <= 20 &
                      !is.na(avgDailyWithMeteoData$TOBS), c("Y.WFEC")])

## --> When temp is between 20 and 21, energy consumption is at 1281
mean(avgDailyWithMeteoData[avgDailyWithMeteoData$TOBS >= 20 &
                      avgDailyWithMeteoData$TOBS <= 21 &
                      !is.na(avgDailyWithMeteoData$TOBS), c("Y.WFEC")])



## HDD (Temperature of reference to know when heating will start inside buildings is different than the one for CDD) --> Tref for HDD is 1 degree
## --> When temp is between 8 and 9 degrees, energy consumption is at 1015.552
mean(avgDailyWithMeteoData[avgDailyWithMeteoData$TOBS >=8 &
                      avgDailyWithMeteoData$TOBS <= 9 &
                      !is.na(avgDailyWithMeteoData$TOBS), c("Y.WFEC")])

## --> When temp is between 2 and3, energy consumption is at 1012
mean(avgDailyWithMeteoData[avgDailyWithMeteoData$TOBS >= 6 &
                      avgDailyWithMeteoData$TOBS <= 7 &
                      !is.na(avgDailyWithMeteoData$TOBS), c("Y.WFEC")])

## --> When temp is between 20 and 21, energy consumption is at 1026 --> constitutes a break in the energy demand/temperature relationship where we can think that heating indoors of the industrial plants and cattle farms start to be required
mean(avgDailyWithMeteoData[avgDailyWithMeteoData$TOBS >= 5 &
                      avgDailyWithMeteoData$TOBS <= 6 &
                      !is.na(avgDailyWithMeteoData$TOBS), c("Y.WFEC")])


## Tt is a weighted average of temperature for day t / Tt = (Tmin + Tmax)/2 

avgDailyWithMeteoData["Tt"] = (avgDailyWithMeteoData$TMIN + avgDailyWithMeteoData$TMAX)/2

###########################
#
#   HDDt and CDDt creation + effective temperature column
#

trefHDD = 5
trefCDD = 19

avgDailyWithMeteoData["CDDTref"] = trefCDD

avgDailyWithMeteoData["HDDTref"] = trefHDD

## HDDt HDDt = max (Tref − Tt, 0)
avgDailyWithMeteoData["HDDt"] = pmax(avgDailyWithMeteoData$HDDTref - avgDailyWithMeteoData$Tt, 0)

## CDDt CDDt = max (Tt − Tref , 0)

avgDailyWithMeteoData["CDDt"] = pmax(avgDailyWithMeteoData$Tt - avgDailyWithMeteoData$CDDTref, 0)


## Calculation of the effective temperature column

avgDailyWithMeteoData["TetMinus1"] = 0

avgDailyWithMeteoData[avgDailyWithMeteoData$DATE.x > as.Date("2011-01-01"), c("TetMinus1")] = avgDailyWithMeteoData[avgDailyWithMeteoData$DATE.x >= as.Date("2011-01-01") &
                                                                                                              avgDailyWithMeteoData$DATE.x < as.Date("2021-01-01") , c("TOBS")]
avgDailyWithMeteoData["Tet"] = 0.5* avgDailyWithMeteoData$TOBS +0.5*avgDailyWithMeteoData$TetMinus1


## creation of the dummy variables for day of the week

avgDailyWithMeteoData["DayOfWeekRaw"] = as.numeric(strftime(avgDailyWithMeteoData$DATE.x, "%u"))

## 1: Monday (reference)
## 2: Tuesday 
## 3: Wednesday
## 4: Thursday
## 5: Friday
## 6: Saturday
## 7: Sunday

## Monday is reference then no variables for it
## Tuesday, if day=2 then 1 otherwise 0

avgDailyWithMeteoData["TuesdayDummy"] = ifelse(avgDailyWithMeteoData["DayOfWeekRaw"]  == 2, 1, 0)

## Wednesday 3 == 1 otherwise 0

avgDailyWithMeteoData["WedneadayDummy"] = ifelse(avgDailyWithMeteoData["DayOfWeekRaw"]  == 3, 1, 0)

## Thursday 4 ==1 otherwise 0

avgDailyWithMeteoData["ThursdayDummy"] = ifelse(avgDailyWithMeteoData["DayOfWeekRaw"]  == 4, 1, 0)
## Friday 5 ==1 otherwise 0

avgDailyWithMeteoData["FridayDummy"] = ifelse(avgDailyWithMeteoData["DayOfWeekRaw"]  == 5, 1, 0)

## Saturday 6 ==1 otherwise 0

avgDailyWithMeteoData["SaturdayDummy"] = ifelse(avgDailyWithMeteoData["DayOfWeekRaw"]  == 6, 1, 0)

## Saturday 6 ==1 otherwise 0

avgDailyWithMeteoData["SaturdayDummy"] = ifelse(avgDailyWithMeteoData["DayOfWeekRaw"]  == 6, 1, 0)

## Saturday 6 ==1 otherwise 0

avgDailyWithMeteoData["SundayDummy"] = ifelse(avgDailyWithMeteoData["DayOfWeekRaw"]  == 7, 1, 0)

## Transform for further group by plotting the column of dayof week raw into factor for categorical analysis



## Dummy variable for months of the year , 2 Feb, 3 March .. 12 Dec

avgDailyWithMeteoData["MonthNumeric"] = as.numeric(format(avgDailyWithMeteoData$DATE.x,"%m"))
avgDailyWithMeteoData["DayNumeric"] = as.numeric(format(avgDailyWithMeteoData$DATE.x,"%d"))

avgDailyWithMeteoData["FebDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 2, 1, 0)
avgDailyWithMeteoData["MarchDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 3, 1, 0)
avgDailyWithMeteoData["AprilDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 4, 1, 0)
avgDailyWithMeteoData["MayDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 5, 1, 0)
avgDailyWithMeteoData["JuneDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 6, 1, 0)
avgDailyWithMeteoData["JulyDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 7, 1, 0)
avgDailyWithMeteoData["AugDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 8, 1, 0)
avgDailyWithMeteoData["SepDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 9, 1, 0)
avgDailyWithMeteoData["OctDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 10, 1, 0)
avgDailyWithMeteoData["NovDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 11, 1, 0)
avgDailyWithMeteoData["DecDummy"] = ifelse(avgDailyWithMeteoData["MonthNumeric"]  == 12, 1, 0)

head(avgDailyWithMeteoData)


## List of Public Holidays in Oklahoma, Texas, and Kansas, and NM


## New Year's Day	Fri., Jan. 1, 2021
## Martin Luther King Jr. Day	Mon., Jan. 18, 2021
## Washington's Birthday	Mon., Feb. 15, 2021
## Memorial Day	Mon., May 31, 2021
## Independence Day	Mon., Jul. 5, 2021
## Labor Day	Mon., Sep. 6, 2021
## Veterans Day	Thu., Nov. 11, 2021
## Thanksgiving	Thu., Nov. 25, 2021
## Christmas Eve	Fri., Dec. 24, 2021
## Christmas Day	Fri., Dec. 25, 2021
## New Year's Day	Fri., Dec. 31, 
avgDailyWithMeteoData["Public Holidays"] = 0

avgDailyWithMeteoData[(avgDailyWithMeteoData$DayNumeric == 1 & avgDailyWithMeteoData$MonthNumeric == 1) |
                      (avgDailyWithMeteoData$DayNumeric == 18 & avgDailyWithMeteoData$MonthNumeric == 1) |
                      (avgDailyWithMeteoData$DayNumeric == 15 & avgDailyWithMeteoData$MonthNumeric == 2) |
                      (avgDailyWithMeteoData$DayNumeric == 31 & avgDailyWithMeteoData$MonthNumeric == 5) |
                      (avgDailyWithMeteoData$DayNumeric == 5 & avgDailyWithMeteoData$MonthNumeric == 7) |
                      (avgDailyWithMeteoData$DayNumeric == 6 & avgDailyWithMeteoData$MonthNumeric == 9) |
                      (avgDailyWithMeteoData$DayNumeric == 11 & avgDailyWithMeteoData$MonthNumeric == 11) |
                      (avgDailyWithMeteoData$DayNumeric == 25 & avgDailyWithMeteoData$MonthNumeric == 11) |
                      (avgDailyWithMeteoData$DayNumeric == 24 & avgDailyWithMeteoData$MonthNumeric == 12) |
                      (avgDailyWithMeteoData$DayNumeric == 25 & avgDailyWithMeteoData$MonthNumeric == 12), c("Public Holidays")] = 1




## List of Days following public (fixed holidays)

avgDailyWithMeteoData["Days Following Public Holidays"] = 0


avgDailyWithMeteoData[(avgDailyWithMeteoData$DayNumeric == 2 & avgDailyWithMeteoData$MonthNumeric == 1) |
                      (avgDailyWithMeteoData$DayNumeric == 19 & avgDailyWithMeteoData$MonthNumeric == 1) |
                      (avgDailyWithMeteoData$DayNumeric == 16 & avgDailyWithMeteoData$MonthNumeric == 2) |
                      (avgDailyWithMeteoData$DayNumeric == 1 & avgDailyWithMeteoData$MonthNumeric == 6) |
                      (avgDailyWithMeteoData$DayNumeric == 6 & avgDailyWithMeteoData$MonthNumeric == 7) |
                      (avgDailyWithMeteoData$DayNumeric == 7 & avgDailyWithMeteoData$MonthNumeric == 9) |
                      (avgDailyWithMeteoData$DayNumeric == 12 & avgDailyWithMeteoData$MonthNumeric == 11) |
                      (avgDailyWithMeteoData$DayNumeric == 26 & avgDailyWithMeteoData$MonthNumeric == 11) |
                      (avgDailyWithMeteoData$DayNumeric == 25 & avgDailyWithMeteoData$MonthNumeric == 12) |
                      (avgDailyWithMeteoData$DayNumeric == 26 & avgDailyWithMeteoData$MonthNumeric == 12), c("Days Following Public Holidays")] = 1





## List of religious holidays. We focus on christian holidays given the relative domination of christianity in this rural belt state









## check for the impact of temperature on demand

## do the same thing with effective temperature



## see if there is an effect of the HDD and CDD, and with lags too



## Try to identify week days, week ends

## Try to identify holidays and days close to holidays

## Also look at the temperature vs demand with week end points and weekday points to check for week end weekday differences

## do the same thing with the precipiation levels


## plot one year only and check for weekend and weekday differences

## plot each year, month by month









