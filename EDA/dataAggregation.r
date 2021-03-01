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
# with the daily hours from the previous day on Dec 11 2018

PrevHourDaysData = sppData[2018==sppData$Year & sppData$Month==12 
                        & sppData$Day==11 & sppData$Hour>=2 &
                        sppData$Hour<=7, ]


# fill out the missing values with the results of the average demand
# for the 3 previous years for each hour
imputeData = PrevHourDaysData[,'Y.WFEC']
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
# with demand at 7pm on Sep 26
PrevHourSep28 = sppData[2017==sppData$Year & sppData$Month==9 & 
                               sppData$Day==26 & sppData$Hour==19, ]



# fill out the missing values with the results of the average demand
# for the 3 previous years for each hour
sppData[is.na(sppData$Y.WFEC) & 
          sppData$Year==2017 & 
          sppData$Month==9 & 
          sppData$Day==27, "Y.WFEC"] = PrevHourSep28[,"Y.WFEC"]

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

## INTEGRATE THE METEO, OIL PRICE DATA, MONTHLY OIL PRODUCTION INFO
## Plus Oil Production Data

meteoPath ='data/meteoFile.csv' ## from noaa site

meteo = read.csv(meteoPath)
head(meteo)

# oil data from https://www.eia.gov/dnav/pet/hist/RWTCD.htm
oilDataPath ='data/oilPriceData.csv' 

oilData = read.csv(oilDataPath)
head(oilData)
names(oilData)[1] ="DATE"
names(oilData)[2] ="OilPrice"

oilData$DATE = format(as.POSIXct(oilData$DATE, format = '%m/%d/%Y'),
                      format = '%Y-%m-%d') 
oilData["Year"] = strftime(oilData$DATE, "%Y")
oilData["Month"] = strftime(oilData$DATE, "%m")
oilData["Day"] = strftime(oilData$DATE, "%d")

blanchardSt = meteo[meteo['NAME']=='BLANCHARD 2 SSW, OK US', 
                    c("NAME","DATE", "PRCP", "TMAX", "TMIN", "TOBS")]


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


# merge with the Oil Data TOO!
# left merge to make sure we do not lose any daily peaks
avgWithMeteo = merge(avgWithMeteo, 
                     oilData, 
                     by=c("Year", "Month", "Day"), 
                     all.x=TRUE) 



## Put the daily WTI price that are NA to the last WTI price
## usually happens during week ends since no WTI trading on 
## week ends
library(zoo)
oilPrice = zoo(avgWithMeteo['OilPrice'])
avgWithMeteo[,'OilPrice'] = na.locf(oilPrice, 
                                    fromLast = TRUE, 
                                    na.rm=FALSE)

avgWithMeteo[3654,'OilPrice'] = avgWithMeteo[3653,'OilPrice']

## OIL PRODUCTION DAILY INFORMATION TO INTEGRATE IN THE DATA FRAME

oilDataProductionPath ='data/oilProductionDailyUS.csv' 

oilDataProduction = read.csv(oilDataProductionPath)

names(oilDataProduction)[1] = "DATE"

oilDataProduction$DATE = oilDataProduction[,'DATE']

oilDataProduction["Year"] = strftime(oilDataProduction$DATE, "%Y")
oilDataProduction["Month"] = strftime(oilDataProduction$DATE, "%m")

## merge the oil production in us data with the frame

avgWithMeteo = merge(avgWithMeteo, 
                     oilDataProduction, 
                     by=c("Year", "Month"), 
                     all.x=TRUE) 

names(avgWithMeteo)[4] = "DATE"


monthlyOilAndGasInfo ='data/monthlyOilAndGasInfo.csv' 

monthlyOil = read.csv(monthlyOilAndGasInfo)

names(monthlyOil)[1] = "DATE"

monthlyOil$DATE = monthlyOil[,'DATE']

monthlyOil["Year"] = strftime(monthlyOil$DATE, "%Y")
monthlyOil["Month"] = strftime(monthlyOil$DATE, "%m")


df = merge(avgWithMeteo, 
                     monthlyOil, 
                     by=c("Year", "Month"), 
                     all.x=TRUE) 

names(df)[4] = "DATE"

## drop unnecessary columns

drops = c("DATE.y", "ï..Date", 
          "DATE.y", "ï..DATE", 
          "DATE.x", "DATE.y")

df = df[ , !(names(df) %in% drops)]

names(df)[14] = "Monthly Rig Count"
names(df)[15] = "Monthly Total Oil Production Anarkado "
names(df)[16] =  "Monhtly Total Natural Gas Production Anarkado"

## integrate the final weather data from Mesonet that include
## info about the wind chill, humidity, solar radiation, etc..

mesonetWeather ='data/weatherDataMesonet.csv' 

weatherMesonet = read.csv(mesonetWeather)
# ensure the keys are the same type to complete the merge
weatherMesonet[,'Year'] = as.numeric(weatherMesonet[,'Year'])
weatherMesonet[, 'Month'] = as.numeric(weatherMesonet[,'Month'])
weatherMesonet[, 'Day'] = as.numeric(weatherMesonet[,'Day'])

df$Year = as.numeric(df$Year)
df$Month = as.numeric(df$Month)
df$Day = as.numeric(df$Day)

finalDf = merge(df, 
           weatherMesonet, 
           by=c("Year", "Month", "Day"), 
           all.x=TRUE) 
           
           
finalDf = finalDf[order(as.Date(finalDf$DATE, format="%Y/%m/%d")),]


# temperature weighted by min max
TempWeighted = (avgWithMeteo$TMIN + avgWithMeteo$TMAX)/2
finalDf["TempWeighted"] = TempWeighted 

###########################
#
#   HDDt and CDDt creation + effective temperature column
#

trefHDD = 5
trefCDD = 20

finalDf["CDDTref"] = trefCDD
finalDf["HDDTref"] = trefHDD

# HDDt HDDt = max (Tref − TempWeighted, 0)
HDD = pmax(finalDf$HDDTref - finalDf$TempWeighted, 0)
finalDf["HDDt"] = HDD

# CDDt CDDt = max (TempWeighted − Tref , 0)
CDDT = pmax(finalDf$TempWeighted - finalDf$CDDTref, 0)
finalDf["CDDt"] = CDDT


## Calculation of the effective temperature column

finalDf["TetMinus1"] = 0
TetMinus1 = finalDf[finalDf$DATE.x>=as.Date("2011-01-01") &
                         finalDf$DATE.x<as.Date("2021-01-01"), 
                         c("TOBS")] 
# backshift
finalDf[finalDf$DATE.x > as.Date("2011-01-01"), 
             c("TetMinus1")] = TetMinus1 

# series for effective temperature
effectiveTemp = 0.5* finalDf$TOBS +0.5*finalDf$TetMinus1 
finalDf["effectiveTemp"] = effectiveTemp


## Binning
finalDf["effectTempCategory"] = 0
# Very Cold
finalDf[!is.na(finalDf["effectiveTemp"]) & 
             finalDf["effectiveTemp"] <= -10, 
             c("effectTempCategory")] = 1
# Cold
finalDf[!is.na(finalDf["effectiveTemp"]) & 
             finalDf["effectiveTemp"] > -10 &
             finalDf["effectiveTemp"] <=0, 
             c("effectTempCategory")] = 2
# Little cold
finalDf[!is.na(finalDf["effectiveTemp"]) & 
             finalDf["effectiveTemp"] > 0 &
             finalDf["effectiveTemp"] <=10, 
             c("effectTempCategory")] = 3
## Mild
finalDf[!is.na(finalDf["effectiveTemp"]) & 
             finalDf["effectiveTemp"] > 10 &
             finalDf["effectiveTemp"] <= 20, 
             c("effectTempCategory")] = 4

## Hot
finalDf[!is.na(finalDf["effectiveTemp"]) & 
             finalDf["effectiveTemp"] > 20 &
             finalDf["effectiveTemp"] <= 30, 
             c("effectTempCategory")] = 5
## Very Hot
finalDf[!is.na(finalDf["effectiveTemp"]) & 
             finalDf["effectiveTemp"] > 30, 
             c("effectTempCategory")] = 6


dfHolidays = aggregate(Y.WFEC ~ sppData$Date, sppData, max)

DailyPeaksWFEC <- timeSeries(dfHolidays$Y.WFEC,
                             dfHolidays$`sppData$Date`,
                             format="%Y-%m-%d")

finalDf["Holidays"] =as.numeric(isHoliday(time(DailyPeaksWFEC), 
                                          "UnitedStates"))


# Day of the week effect?
# Need to create a variable that contains the corresponding 
# 'day of the week' for each date and plot the demand against
# the day to 'see' if there is possibly an effect.
dayofweek <- substr(weekdays(finalDf$DATE,), 1, 3)
dow <- ifelse(dayofweek=="Sun", 7, 
       ifelse(dayofweek=="Mon", 1,
       ifelse(dayofweek=="Tue", 2, 
       ifelse(dayofweek=="Wed", 3,
       ifelse(dayofweek=="Thu", 4, 
       ifelse(dayofweek=="Fri", 5, 6))))))

finalDf['Day of Week'] = dow

finalDf['WeekendIdentifier'] = ifelse(finalDf['Day of Week']== 1, 0, 
       ifelse(finalDf['Day of Week']== 2, 0,
       ifelse(finalDf['Day of Week']== 3, 0,
       ifelse(finalDf['Day of Week']== 4, 0,
       ifelse(finalDf['Day of Week']== 5, 0, 
       ifelse(finalDf['Day of Week']== 6, 1,
       ifelse(finalDf['Day of Week']== 7, 1, 0
       )))))))

## The tables below show there is a 

weeklyEffectPeriod1 = aggregate(finalDf[1:1000, 'Y.WFEC'], 
list(finalDf[1:1000, 'Day of Week']), mean)

names(weeklyEffectPeriod1)[1] = "Day Id"
names(weeklyEffectPeriod1)[2] = "Electricity Demand"

weeklyEffectPeriod2 = aggregate(finalDf[1000:2000, 'Y.WFEC'], 
list(finalDf[1000:2000, 'Day of Week']), mean)
names(weeklyEffectPeriod2)[1] = "Day Id"
names(weeklyEffectPeriod2)[2] = "Electricity Demand"

weeklyEffectPeriod3 = aggregate(finalDf[2000:3364, 'Y.WFEC'], 
list(finalDf[2000:3364, 'Day of Week']), mean)
names(weeklyEffectPeriod3)[1] = "Day Id"
names(weeklyEffectPeriod3)[2] = "Electricity Demand"


boxplot(Y.WFEC~ Holidays, 
        data=finalDf[2000:3364, c("Holidays", "Y.WFEC")], 
        main="Demand During Normal Days vs Holidays", 
        ylab="Energy Demand in WFEC")

boxplot(Y.WFEC~ Holidays, 
        data=finalDf[1000:2000, c("Holidays", "Y.WFEC")], 
        main="Demand During Normal Days vs Holidays", 
        ylab="Energy Demand in WFEC")
boxplot(Y.WFEC~ Holidays, 
        data=finalDf[1:1000, c("Holidays", "Y.WFEC")], 
        main="Demand During Normal Days vs Holidays", 
        ylab="Energy Demand in WFEC")


yourPath = 'data/finalDf.csv'
write.csv(finalDf, yourPath, row.names = FALSE)





