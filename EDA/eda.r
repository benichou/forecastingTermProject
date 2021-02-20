#install timeseries package

library(timeSeries)
library(timeDate)


# load the R data
filePath = 'data/sppdata.rdata'
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


# export as a csv
yourPath = 'data/dailyPeaksWFEC.csv'
write.csv(dailyPeaksWFEC, yourPath, row.names = FALSE)

###############################################################
#                                                             #
#           Data Exploration Y Differencing                   #
#                                                             # 
#                                                             #
###############################################################
pdf("viz/WFEC_differencing.pdf")

DailyPeaksWFEC <- timeSeries(dailyPeaksWFEC$Y.WFEC, 
                             dailyPeaksWFEC$`sppData$Date`,
                             format="%Y-%m-%d")



par(mfrow=c(2,1))
plot(DailyPeaksWFEC, 
     col="grey", 
     ylab="WFEC daily max demand (in MW)")
lines(time(DailyPeaksWFEC), 
      supsmu(as.numeric(time(DailyPeaksWFEC)), 
             DailyPeaksWFEC)$y, col="red")
abline(b=0, a=mean(DailyPeaksWFEC), col="blue")



lag365diff <- diff(DailyPeaksWFEC, 365)
plot(lag365diff, 
     col="grey", 
     ylab="lag 365 differences of daily demand")
lines(time(lag365diff)[-(1:365)],
      supsmu(as.numeric(time(lag365diff)), lag365diff)$y, col="red")



dev.off(dev.cur())

###############################################################
#                                                             #
#           Data Exploration Y vs Explanatory variables       #
#                                                             # 
#                                                             #
###############################################################

# before we work with the temperature data, we will need to check 
# that the different stations spanning across the WFEC territory 
# show consistent numbers. If they do not show consistent numbers, 
# then we should consider some sort of weighting of the temperature
# per station and their associated territories, 
# based on the energy size consumption of each territory.

meteoPath ='data/meteoFile.csv' ## from noaa site

meteo = read.csv(meteoPath)
head(meteo)

# oil data from https://www.eia.gov/dnav/pet/hist/RWTCD.htm
oilDataPath ='data/oilData.csv' 

oilData = read.csv(oilDataPath)
head(oilData)
names(oilData)[1] ="DATE"
names(oilData)[2] ="OilPrice"
oilData$DATE = format(as.POSIXct(oilData$DATE, format = '%m/%d/%Y'),
                      format = '%Y-%m-%d') 
oilData["Year"] = strftime(oilData$DATE, "%Y")
oilData["Month"] = strftime(oilData$DATE, "%m")
oilData["Day"] = strftime(oilData$DATE, "%d")


# We only select the meteorological data of the following stations 
# out of the meteo frame because the other stations do not 
# cover the entire 2011 2021 period or have too big missing gaps

blanchardSt = meteo[meteo['NAME']=='BLANCHARD 2 SSW, OK US', 
                    c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

cherokeeSt = meteo[meteo['NAME']=='CHEROKEE 1 SSW MESONET, OK US',
                   c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

clovisNMSt = meteo[meteo['NAME']=='CLOVIS 13 N, NM US', 
                   c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

seminoleOkSt = meteo[meteo['NAME']=='SEMINOLE 4 SSW MESONET, OK US', 
                    c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

tiptonOkSt = meteo[meteo['NAME']=='TIPTON 4 S, OK US',
                   c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

# only keep the stations you need in a filtered meteo dataframe

stList = c('BLANCHARD 2 SSW, OK US', 
          'CHEROKEE 1 SSW MESONET, OK US', 
          'CLOVIS 13 N, NM US', 
          'SEMINOLE 4 SSW MESONET, OK US',
          'TIPTON 4 S, OK US')

meteo = meteo[meteo$NAME %in% stList, 
              c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]

summary(meteo)

# We can see here that TOBS has 8950 NA's, that TMAX has 75 NA, 
# that TMIN has 79 NA and that PRCP (precipitation level) has 633 NA
# For the sake of visualizations, we will remove all rows with NA in 

meteoViz = meteo[complete.cases(meteo[ , 5:6]),]

meteoViz[meteoViz['NAME']==stList[1],c("NAME")] = "Blanchard Station"
meteoViz[meteoViz['NAME']==stList[2],c("NAME")] = "Cherokee Station"
meteoViz[meteoViz['NAME']==stList[3],c("NAME")] = "Clovis Station"
meteoViz[meteoViz['NAME']==stList[4],c("NAME")] = "Seminole Station"
meteoViz[meteoViz['NAME']==stList[5],c("NAME")] = "Tipton Station"


pdf("viz/tempAcrossStations.pdf")

# The box plot disctibutions of the different stations 
# across the WFEC territories are similar
boxplot(TOBS~ NAME, data=meteoViz[, c("NAME", "TOBS")], 
                    names = c("Blanchard Station", 
                              "Cherokee Station", 
                              "Clovis Station", 
                              "Seminole Station", 
                              "Tipton Station"), xaxs = FALSE)

tempStations = c(blanchardSt$TOBS, 
                 clovisNMSt$TOBS, 
                 seminoleOkSt$TOBS, 
                 tiptonOkSt$TOBS)

datesStations = c(blanchardSt$DATE, 
                  clovisNMSt$DATE, 
                  seminoleOkSt$DATE, 
                  tiptonOkSt$DATE)
## all
tempSeriesAll = timeSeries(tempStations, 
                           datesStations, 
                           format="%Y-%m-%d")

plot(tempSeriesAll, 
     ylab="Evolution of the Temperature in Celsius", 
     main="Evolution of Temperature over 2011 to 2021 
           in OK, NM, KS, and NM For All Weather Stations") 

plot(tempSeriesAll, 
     ylab="Evolution of the Temperature in Celsius", 
     type="p", 
     main="Evolution of Temperature over 2011 to 2021 
           in OK, NM, KS, and NM For All Weather Stations") 

# blanchard
tempSeriesBlanchard = timeSeries(blanchardSt$TOBS, 
                                 blanchardSt$DATE, format="%Y-%m-%d")

plot(tempSeriesBlanchard, 
     ylab="Evolution of the Temperature in Celsius", 
     type="p", 
     main="Evolution of Temperature over 2011 to 2021 
           - Blanchard Station") 

# New Mexico

tempSeriesNM = timeSeries(clovisNMSt$TOBS, 
                          clovisNMSt$DATE, format="%Y-%m-%d")

# same usual trend in terms of temperature
plot(tempSeriesNM, 
     ylab="Evolution of the Temperature in Celsius", 
     type="p", 
     main="Evolution of Temperature over 2011 to 2021 
           - New Mexico Station") 

## Seminole OK Station

tempSeriesSeminole = timeSeries(seminoleOkSt$TOBS, 
                                seminoleOkSt$DATE, 
                                format="%Y-%m-%d")

# same usual trend in terms of temperature
plot(tempSeriesSeminole, 
     ylab="Evolution of the Temperature in Celsius", 
     type="p", 
     main="Evolution of Temperature over 2011 to 2021 
           - Seminole Station") 

## Tipton
tempSeriesTipton = timeSeries(tiptonOkSt$TOBS, 
                              tiptonOkSt$DATE, format="%Y-%m-%d")

# Follow same trend but we do not have enough data
plot(tempSeriesTipton, 
     ylab="Evolution of the Temperature in Celsius", 
     type="p", 
     main="Evolution of Temperature over 2011 to 2021 
           - Tipton Station") 

# We again see that the temperature are quite close to one another 
# and thus we could just pick one weather station for our territory 
# out of the weather stations covering the WFEC territory

dev.off(dev.cur())



# create a table that group by stats of temperature 
# average, std, min, max

# summary stats for Tobs in the stations
tapply(meteo$TOBS, meteo$NAME, summary) 

# There is a consistent distibution across the stations except with 
# lower temp in New Mexico. Let us bear in mind that New Mexico does
# not represent a major part of our consumers

# summary precipitations for the stations
tapply(meteo$PRCP, meteo$NAME, summary)

# There is a consistent distibution across the stations except with 
# lower precipitation levels in New Mexico. Let us bear in mind that 
# New Mexico does not represent a major part of our consumers



# We will pick the Blanchard station because it is the most accurate
# and is equidistant to all areas covered by WFEC in addition to 
# having quite close temperature and precipitation levels compared to
# the other weather stations

head(blanchardSt)

# summary statistics 

# for observed temperature for each day in the 2011-2021 period
summary(blanchardSt$TOBS) 

# for observed min temperature for each day in the 2011-2021 period
summary(blanchardSt$TMIN) 

# for observed max temperature for each day in the 2011-2021 period
summary(blanchardSt$TMAX) 

# for observed precipitation for each day in the 2011-2021 period
summary(blanchardSt$PRCP) 


blanchardSt["Year"] = strftime(blanchardSt$DATE, "%Y")
blanchardSt["Month"] = strftime(blanchardSt$DATE, "%m")
blanchardSt["Day"] = strftime(blanchardSt$DATE, "%d")

# rename column to "DATE "
names(dailyPeaksWFEC)[1] = "DATE"

dailyPeaksWFEC["Year"] = strftime(dailyPeaksWFEC$DATE, "%Y")
dailyPeaksWFEC["Month"] = strftime(dailyPeaksWFEC$DATE, "%m")
dailyPeaksWFEC["Day"] = strftime(dailyPeaksWFEC$DATE, "%d")


# Energy consumption frame + the meteoroligcal data BELOW

# left merge to make sure we do not lose any daily peaks 
avgWithMeteo = merge(dailyPeaksWFEC, 
                     blanchardSt, 
                     by=c("Year", "Month", "Day"), 
                     all.x=TRUE)

# when we do not have any temperature data 
# TODO: Imputation of missing data for modeling phase
names(avgWithMeteo)[4] = "DATE"

# merge with the Oil Data TOO!
# left merge to make sure we do not lose any daily peaks
avgWithMeteo = merge(avgWithMeteo, 
                     oilData, 
                     by=c("Year", "Month", "Day"), 
                     all.x=TRUE) 

 



# Identifying Temperature of Reference --> a reference temperature
# that should be chosen in an adequate way in order to separate 
# the hot and cold ‘ends’ of the demand-temperature relationship


pdf("viz/frequencyOfTempBlanchard.pdf")


hist(avgWithMeteo$TOBS, 
     main="Frequency of Observed Temperatures at Blanchard Station", 
     xlab="Observed Temp (Celsius)")

dev.off(dev.cur())

## Binning

avgWithMeteo["TemperatureCategory"] = 0

# Very Cold
avgWithMeteo[!is.na(avgWithMeteo["TOBS"]) & 
             avgWithMeteo["TOBS"] <= -10, 
             c("TemperatureCategory")] = "A/ Very Cold (<=-10)"
# Cold
avgWithMeteo[!is.na(avgWithMeteo["TOBS"]) & 
             avgWithMeteo["TOBS"] > -10 &
             avgWithMeteo["TOBS"] <=0, 
             c("TemperatureCategory")] = "B/ Cold (-10 to 0)"
# Little cold
avgWithMeteo[!is.na(avgWithMeteo["TOBS"]) & 
             avgWithMeteo["TOBS"] > 0 &
             avgWithMeteo["TOBS"] <=10, 
             c("TemperatureCategory")] = "C/ Little Cold (0 to 10)"
## Mild
avgWithMeteo[!is.na(avgWithMeteo["TOBS"]) & 
             avgWithMeteo["TOBS"] > 10 &
             avgWithMeteo["TOBS"] <= 20, 
             c("TemperatureCategory")] = "D/ Mild (10 to 20)"

## Hot
avgWithMeteo[!is.na(avgWithMeteo["TOBS"]) & 
             avgWithMeteo["TOBS"] > 20 &
             avgWithMeteo["TOBS"] <= 30, 
             c("TemperatureCategory")] = "E/ Hot (20 to 30)"
## Very Hot
avgWithMeteo[!is.na(avgWithMeteo["TOBS"]) & 
             avgWithMeteo["TOBS"] > 30, 
             c("TemperatureCategory")] = "F/ Very Hot (More than 30)"


## It is very clear here that the temperature of reference is 


pdf("viz/visualizingTRefForCoolingAndHeating.pdf")


boxplot(Y.WFEC~ TemperatureCategory, 
        data=avgWithMeteo[avgWithMeteo["TemperatureCategory"] != 0, 
                          c("TemperatureCategory", "Y.WFEC")], 
        main="What temperatures drive the energy consumption in WFEC?")

# Follow same trend but we do not have enough data
plot(avgWithMeteo$TOBS, 
     avgWithMeteo$Y.WFEC, 
     ylab="Evolution of the Demand in function of the Temperature", 
     type="l", 
     main="What temperatures drive the energy consumption in WFEC?") 


dev.off(dev.cur())

# This helped us see that the temperature of reference for CDD 
# (when cooling starts to be required in a building) might be 
# around 20 degrees. This also helped us see that the temperature of
# reference for HDD (when heating starts to be required in buildings)
# might be around 0 to 5 degrees

# Let us just compare the different Y.WFEC energy consumption at 
# different temperature level


# a reference temperature that should be chosen in an adequate way 
# in order to separate the hot and cold ‘ends’ of the 
# demand-temperature

# Summary statistics about the daily demand

summary(avgWithMeteo$Y.WFEC)

# summary statistics about the daily temperature in WFEC
summary(avgWithMeteo$TOBS)

# summary statistics about the daily precipitation levels in WFEC
summary(avgWithMeteo$PRCP)

## summary statistics about the daily oil price

summary(avgWithMeteo$OilPrice)



# CDD (Temperature of reference is then 19 degrees for CDD)
# --> When temp is between 18 and 19, energy consumption is 1198.274
mean(avgWithMeteo[avgWithMeteo$TOBS >= 18 &
                  avgWithMeteo$TOBS <= 19 &
                  !is.na(avgWithMeteo$TOBS), c("Y.WFEC")])

# --> When temp is between 19 and 20, energy consumption is 1260.899 
# --> Biggest jump from one increase in celsius degree 
# --> We shall set the Temperature of Reference to 19 degrees 
#     for air conditionning and irrigation works possibly
mean(avgWithMeteo[avgWithMeteo$TOBS >= 19 &
                  avgWithMeteo$TOBS <= 20 &
                  !is.na(avgWithMeteo$TOBS), c("Y.WFEC")])

# --> When temp is between 20 and 21, energy consumption is 1281
mean(avgWithMeteo[avgWithMeteo$TOBS >= 20 &
                  avgWithMeteo$TOBS <= 21 &
                  !is.na(avgWithMeteo$TOBS), c("Y.WFEC")])



# HDD (Temperature of reference to know when heating will start
# inside buildings is different than the one for CDD) 
# --> Tref for HDD is 1 degree
# --> When temp is between 8 and 9 degrees, 
#     energy consumption is 1015.552
mean(avgWithMeteo[avgWithMeteo$TOBS >=8 &
                  avgWithMeteo$TOBS <= 9 &
                  !is.na(avgWithMeteo$TOBS), c("Y.WFEC")])

## --> When temp is between 2 and3, energy consumption is 1012
mean(avgWithMeteo[avgWithMeteo$TOBS >= 6 &
                  avgWithMeteo$TOBS <= 7 &
                  !is.na(avgWithMeteo$TOBS), c("Y.WFEC")])

# --> When temp is between 5 and 6, energy consumption is 1026 
# --> constitutes a break in energy demand/temperature relationship
# where we can think that heating indoors of the industrial plants
# and cattle farms start to be required
mean(avgWithMeteo[avgWithMeteo$TOBS >= 5 &
                  avgWithMeteo$TOBS <= 6 &
                  !is.na(avgWithMeteo$TOBS), c("Y.WFEC")])


# Tt is a weighted average of temperature for day t 
# Tt = (Tmin + Tmax)/2

# temperature weighted by min max
TempWeighted = (avgWithMeteo$TMIN + avgWithMeteo$TMAX)/2
avgWithMeteo["TempWeighted"] = TempWeighted 

###########################
#
#   HDDt and CDDt creation + effective temperature column
#

trefHDD = 5
trefCDD = 20

avgWithMeteo["CDDTref"] = trefCDD
avgWithMeteo["HDDTref"] = trefHDD

# HDDt HDDt = max (Tref − TempWeighted, 0)
HDD = pmax(avgWithMeteo$HDDTref - avgWithMeteo$TempWeighted, 0)
avgWithMeteo["HDDt"] = HDD

# CDDt CDDt = max (TempWeighted − Tref , 0)
CDDT = pmax(avgWithMeteo$TempWeighted - avgWithMeteo$CDDTref, 0)
avgWithMeteo["CDDt"] = CDDT


## Calculation of the effective temperature column

avgWithMeteo["TetMinus1"] = 0
TetMinus1 = avgWithMeteo[avgWithMeteo$DATE.x>=as.Date("2011-01-01") &
                         avgWithMeteo$DATE.x<as.Date("2021-01-01"), 
                         c("TOBS")] 
# backshift
avgWithMeteo[avgWithMeteo$DATE.x > as.Date("2011-01-01"), 
             c("TetMinus1")] = TetMinus1 

# series for effective temperature
effectiveTemp = 0.5* avgWithMeteo$TOBS +0.5*avgWithMeteo$TetMinus1 
avgWithMeteo["effectiveTemp"] = effectiveTemp


## Binning
avgWithMeteo["effectTempCategory"] = 0
# Very Cold
avgWithMeteo[!is.na(avgWithMeteo["effectiveTemp"]) & 
             avgWithMeteo["effectiveTemp"] <= -10, 
             c("effectTempCategory")] = "A/ Very Cold (<=-10)"
# Cold
avgWithMeteo[!is.na(avgWithMeteo["effectiveTemp"]) & 
             avgWithMeteo["effectiveTemp"] > -10 &
             avgWithMeteo["effectiveTemp"] <=0, 
             c("effectTempCategory")] = "B/ Cold (-10 to 0)"
# Little cold
avgWithMeteo[!is.na(avgWithMeteo["effectiveTemp"]) & 
             avgWithMeteo["effectiveTemp"] > 0 &
             avgWithMeteo["effectiveTemp"] <=10, 
             c("effectTempCategory")] = "C/ Little Cold (0 to 10)"
## Mild
avgWithMeteo[!is.na(avgWithMeteo["effectiveTemp"]) & 
             avgWithMeteo["effectiveTemp"] > 10 &
             avgWithMeteo["effectiveTemp"] <= 20, 
             c("effectTempCategory")] = "D/ Mild (10 to 20)"

## Hot
avgWithMeteo[!is.na(avgWithMeteo["effectiveTemp"]) & 
             avgWithMeteo["effectiveTemp"] > 20 &
             avgWithMeteo["effectiveTemp"] <= 30, 
             c("effectTempCategory")] = "E/ Hot (20 to 30)"
## Very Hot
avgWithMeteo[!is.na(avgWithMeteo["effectiveTemp"]) & 
             avgWithMeteo["effectiveTemp"] > 30, 
             c("effectTempCategory")] = "F/ Very Hot (More than 30)"


## creation of the dummy variables for day of the week
DOWRaw = as.numeric(strftime(avgWithMeteo$DATE.x, "%u"))
avgWithMeteo["DOWRaw"] = DOWRaw 

## 1: Monday (reference)
## 2: Tuesday 
## 3: Wednesday
## 4: Thursday
## 5: Friday
## 6: Saturday
## 7: Sunday

## Monday is reference then no variables for it
## Tuesday, if day=2 then 1 otherwise 0

avgWithMeteo["TueDummy"] = ifelse(avgWithMeteo["DOWRaw"]==2, 1, 0)
## Wednesday 3 == 1 otherwise 0
avgWithMeteo["WedDummy"] = ifelse(avgWithMeteo["DOWRaw"]==3, 1, 0)
## Thursday 4 ==1 otherwise 0
avgWithMeteo["ThuDummy"] = ifelse(avgWithMeteo["DOWRaw"]==4, 1, 0)
## Friday 5 ==1 otherwise 0
avgWithMeteo["FriDummy"] = ifelse(avgWithMeteo["DOWRaw"]==5, 1, 0)
## Saturday 6 ==1 otherwise 0
avgWithMeteo["SatDummy"] = ifelse(avgWithMeteo["DOWRaw"]==6, 1, 0)
## Sunday 6 ==1 otherwise 0
avgWithMeteo["SunDummy"] = ifelse(avgWithMeteo["DOWRaw"]==7, 1, 0)

## create week end week day indicator

avgWithMeteo["WeekEndInd"] = ifelse(avgWithMeteo["DOWRaw"]==7 , 1, 0)
avgWithMeteo["WeekEndInd"] = ifelse(avgWithMeteo["DOWRaw"]==6 , 1, 0)


# Transform for further group by plotting the column 
# of dayof week raw into factor for categorical analysis

dfHolidays = aggregate(Y.WFEC ~ sppData$Date, sppData, max)

DailyPeaksWFEC <- timeSeries(dfHolidays$Y.WFEC,
                             dfHolidays$`sppData$Date`,
                             format="%Y-%m-%d")

avgWithMeteo["Holidays"] = as.numeric(isHoliday(time(DailyPeaksWFEC)))


avgWithMeteo["MonthNum"] = as.numeric(format(avgWithMeteo$DATE.x,"%m"))
avgWithMeteo["DayCategory"] = weekdays(as.Date(avgWithMeteo$DATE.x))

avgWithMeteo["FebDummy"] = ifelse(avgWithMeteo["MonthNum"]==2, 1, 0)
avgWithMeteo["MarDummy"] = ifelse(avgWithMeteo["MonthNum"]==3, 1, 0)
avgWithMeteo["AprDummy"] = ifelse(avgWithMeteo["MonthNum"]==4, 1, 0)
avgWithMeteo["MayDummy"] = ifelse(avgWithMeteo["MonthNum"]==5, 1, 0)
avgWithMeteo["JunDummy"] = ifelse(avgWithMeteo["MonthNum"]==6, 1, 0)
avgWithMeteo["JulDummy"] = ifelse(avgWithMeteo["MonthNum"]==7, 1, 0)
avgWithMeteo["AugDummy"] = ifelse(avgWithMeteo["MonthNum"]==8, 1, 0)
avgWithMeteo["SepDummy"] = ifelse(avgWithMeteo["MonthNum"]==9, 1, 0)
avgWithMeteo["OctDummy"] = ifelse(avgWithMeteo["MonthNum"]==10, 1, 0)
avgWithMeteo["NovDummy"] = ifelse(avgWithMeteo["MonthNum"]==11, 1, 0)
avgWithMeteo["DecDummy"] = ifelse(avgWithMeteo["MonthNum"]==12, 1, 0)



## check for the impact of effective temperature on demand


pdf("viz/tempExploration.pdf")


boxplot(Y.WFEC~ effectTempCategory, 
          data=avgWithMeteo[avgWithMeteo["effectTempCategory"] != 0, 
        c("effectTempCategory", "Y.WFEC")], 
        main="What effectivet emperatures 
              drive the energy consumption in WFEC?")


# Create time series for mean temperature, heating degree days 
# and cooling degree days variables.

# series for effective temperature
Teffective = timeSeries(avgWithMeteo$effectiveTemp, 
                        avgWithMeteo$DATE.x, 
                        format="%Y-%m-%d")

TweigtedTemp = timeSeries(avgWithMeteo$TempWeighted, 
                          avgWithMeteo$DATE.x, 
                          format="%Y-%m-%d")

HDDt = timeSeries(avgWithMeteo$HDDt, 
                  avgWithMeteo$DATE.x, 
                  format="%Y-%m-%d")

CDDt = timeSeries(avgWithMeteo$CDDt, 
                  avgWithMeteo$DATE.x, 
                  format="%Y-%m-%d")

Yt = timeSeries(avgWithMeteo$Y.WFEC, 
                avgWithMeteo$DATE.x, 
                format="%Y-%m-%d")

Prcp = timeSeries(avgWithMeteo$PRCP, 
                  avgWithMeteo$DATE.x, 
                  format="%Y-%m-%d")

# Is there a CDD or HDD effect with demand?
plot(series(CDDt), series(Yt),
     ylab="daily demand in WFEC", 
     xlab="CDD", 
     pch=23)

plot(series(HDDt), series(Yt),
     ylab="daily demand in WFEC", 
     xlab="HDD", 
     pch=23)

# Is there a lagged HDD effect?
plot(lag(HDDt,1), series(Yt), 
     xlab="lag-1 HDDt",
     ylab="daily demand in WFEC")

plot(lag(HDDt,2), series(Yt), 
     xlab="lag-2 HDDt",
     ylab="daily demand in WFEC")

# Is there a lagged CDD effect?
plot(lag(CDDt,1), series(Yt), 
     xlab="lag-1 CDDt",
     ylab="daily demand in WFEC")

plot(lag(CDDt,2), series(Yt), 
     xlab="lag-2 CDDt",
     ylab="daily demand in WFEC")

## Is there a precipitation effect?
plot(series(Prcp), series(Yt),
     ylab="daily demand in WFEC", 
     xlab="Precipitation levels", 
     pch=23)

## lagged precipitation effect?
plot(lag(Prcp,1), series(Yt), 
     xlab="lag-1 CDDt",
     ylab="daily demand in WFEC")

plot(lag(Prcp,2), series(Yt), 
      xlab="lag-2 CDDt",
     ylab="daily demand in WFEC")

# Is there a weigted average temperature effect?
plot(series(TweigtedTemp), series(Yt),
     ylab="daily demand in WFEC", 
     xlab="Weigted Temperature average", 
     pch=23)

# Is there an effective temperature effect?
plot(series(Teffective), series(Yt),
     ylab="daily demand in WFEC", 
     xlab="Effective Temperature levels", 
     pch=23)

# Day of the week effect?
# Need to create a variable that contains the corresponding 
# 'day of the week' for each date and plot the demand against
# the day to 'see' if there is possibly an effect.
dayofweek <- substr(weekdays(avgWithMeteo$DATE.x,), 1, 3)
dow <- ifelse(dayofweek=="Sun", 1, 
       ifelse(dayofweek=="Mon", 2,
       ifelse(dayofweek=="Tue", 3, 
       ifelse(dayofweek=="Wed", 4,
       ifelse(dayofweek=="Thu", 5, 
       ifelse(dayofweek=="Fri", 6, 7))))))

boxplot(series(Yt) ~ dow,
        names=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        ylab="Daily Demand in WFEC")


boxplot(Y.WFEC~ MonthNum, 
        data=avgWithMeteo[, c("MonthNum", "Y.WFEC")], 
        main="Evolution of the demand per month", 
        ylab="Energy Demand in WFEC", 
        xlab="Months (Chronological Order)")

boxplot(Y.WFEC~ Holidays, 
        data=avgWithMeteo[, c("Holidays", "Y.WFEC")], 
        main="Demand During Normal Days vs Holidays", 
        ylab="Energy Demand in WFEC")


## WeekDays or Week ends
boxplot(Y.WFEC~ WeekEndInd + MonthNum, 
        data=avgWithMeteo[, c("WeekEndInd", 
                              "MonthNum", 
                              "Y.WFEC")], 
        main="Demand During WeekDays Vs Week Ends", 
        ylab="Energy Demand in WFEC")


<<<<<<< HEAD
boxplot(Y.WFEC~ Holidays + MonthNum, 
        data=avgWithMeteo[, c("Holidays", 
                              "Y.WFEC", 
                              "MonthNum")], 
        main="Demand During Normal Days vs Holidays Across Months", 
        ylab="Energy Demand in WFEC")
=======
boxplot(Y.WFEC~ Holidays + MonthNumeric, data=avgDailyWithMeteoData[, c("Holidays", "Y.WFEC", "MonthNumeric")], main="Demand During Normal Days vs Holidays Across Months", ylab="Energy Demand in WFEC")
>>>>>>> develop

year1 = series(window(DailyPeaksWFEC,
                start=timeDate("2011-01-01", format="%Y-%m-%d"),
                end=timeDate("2011-12-31", format="%Y-%m-%d")))
year2 = series(window(DailyPeaksWFEC,
                start=timeDate("2012-01-01", format="%Y-%m-%d"),
                end=timeDate("2012-12-31", format="%Y-%m-%d")))
year3 = series(window(DailyPeaksWFEC,
                start=timeDate("2013-01-01", format="%Y-%m-%d"),
                end=timeDate("2013-12-31", format="%Y-%m-%d")))

year4 = series(window(DailyPeaksWFEC,
                start=timeDate("2014-01-01", format="%Y-%m-%d"),
                end=timeDate("2014-12-31", format="%Y-%m-%d")))

plot(year1, axes=F,
     lty=1, type="l",
     ylab="WFEC daily demand (in MW)", xlab="", col="blue")
lines(year2, lty=3, lwd=1.8,col="green")
lines(year3, lty=3, lwd=1.8, col="red")
lines(year4, lty=3, lwd=1.8, col="black")
axis(1, at =seq(0,31*11+30, by=31),
     labels=c("Jan","Feb","Mar","Apr","May","June","July", 
               "Aug", "Sep", "Oct", "Nov", "Dec"))
legend("bottomleft", legend=c("2011","2012", "2013", "2014"), 
       lty=c(1,2), col=c("blue", "green", "red", "black"))


year1 = series(window(DailyPeaksWFEC,
                start=timeDate("2015-01-01", format="%Y-%m-%d"),
                end=timeDate("2015-12-31", format="%Y-%m-%d")))
year2 = series(window(DailyPeaksWFEC,
                start=timeDate("2016-01-01", format="%Y-%m-%d"),
                end=timeDate("2016-12-31", format="%Y-%m-%d")))
year3 = series(window(DailyPeaksWFEC,
                start=timeDate("2017-01-01", format="%Y-%m-%d"),
                end=timeDate("2017-12-31", format="%Y-%m-%d")))

year4 = series(window(DailyPeaksWFEC,
                start=timeDate("2018-01-01", format="%Y-%m-%d"),
                end=timeDate("2018-12-31", format="%Y-%m-%d")))


plot(year1, axes=F,
     lty=1, type="l",
     ylab="WFEC daily demand (in MW)", xlab="", col="blue")
lines(year2, lty=3, lwd=1.8,col="green")
lines(year3, lty=3, lwd=1.8, col="red")
lines(year4, lty=3, lwd=1.8, col="black")
axis(1, at =seq(0,31*11+30, by=31),
     labels=c("Jan","Feb","Mar","Apr","May","June","July", 
               "Aug", "Sep", "Oct", "Nov", "Dec"))
legend("bottomleft", legend=c("2015","2016", "2017","2018"), 
       lty=c(1,2), col=c("blue", "green", "red", "black"))



year1 = series(window(DailyPeaksWFEC,
                start=timeDate("2019-01-01", format="%Y-%m-%d"),
                end=timeDate("2019-12-31", format="%Y-%m-%d")))
year2 = series(window(DailyPeaksWFEC,
                start=timeDate("2020-01-01", format="%Y-%m-%d"),
                end=timeDate("2020-12-31", format="%Y-%m-%d")))
plot(year1, axes=F,
     lty=1, type="l",
     ylab="WFEC daily demand (in MW)", xlab="", col="blue")
lines(year2, lty=3, lwd=1.8,col="green")
axis(1, at =seq(0,31*11+30, by=31),
     labels=c("Jan","Feb","Mar","Apr","May","June","July", 
               "Aug", "Sep", "Oct", "Nov", "Dec"))
legend("bottomleft", legend=c("2019","2020"), 
       lty=c(1,2), col=c("blue", "green", "red", "black"))



dev.off(dev.cur())


pdf("viz/oilPriceOnDemand.pdf")



plot(avgWithMeteo$OilPrice, 
     avgWithMeteo$Y.WFEC, 
     ylab="Energy Demand", 
     type="p", 
     main="Demand vs Effective Temperature", 
     col = "green")


plot(lag(avgWithMeteo$OilPrice, 1), 
     avgWithMeteo$Y.WFEC, 
     ylab="Energy Demand", 
     type="p", 
     main="Demand vs Effective Temperature", 
     col = "red")



dev.off(dev.cur())


pdf("viz/oilPriceVariation.pdf")

oilPrice = timeSeries(oilData$OilPrice[oilData$DATE > "2011-01-01"], 
                      oilData$DATE[oilData$DATE > "2011-01-01"], 
                      format="%Y-%m-%d")
plot(oilPrice, 
     ylab="Gas Price", 
     type="l", 
     main="Evolution of the Gas Price as an 
           Economic and Electricity Benchmark since 2011", 
     col = "red") ## same usual trend in terms of temperature


dev.off(dev.cur())


print("Done")