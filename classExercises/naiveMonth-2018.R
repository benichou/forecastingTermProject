#
# Program: naiveMonth.R
#
# Purpose: Computing some naive forecasts.
#          An evaluation is also provided.
#
# Description: We aggregate our hourly data for Ontario
#              demand to obtain a total MONTHLY demand.  We then
#              compute the bias and MAPE for three different
#              naive forecasts for next MONTH's demand.
#
# Written by: D.J. Dupuis, 25 December 2019
#
# Updated: 3 January 2021
# 
# ------------------------------------------------------
#
# Set locale to English language
Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(forecast)
options(digits=3)

pdf("naiveMonth-2018.pdf")

# function to read and aggregate data over all years
read.data <- function(years,path="../data/Ontario/PUB_Demand_") {

  day <- character(); hour <- Ontario <- numeric()
  
  for (year in years) { 

    dem <- read.csv(paste(path,year,".csv",sep=""), 
                col.names=c("Date","Hour","Market","Ontario"),
                colClasses=c("character",rep("integer",3)),skip=3)
    day  <- c(day,dem$Date)
    hour <- c(hour,dem$Hour)
    Ontario <- c(Ontario,dem$Ontario)
                      }
 
    myday  <- paste(day,hour,sep="-")
    Ont <- timeSeries(Ontario,myday,format="%Y-%m-%d-%H")

    return(list(Ont=Ont,day=day))

}
# Read the data for 2015-2018
years <- 2015:2018
aa <- read.data(years)
Ont <- aa$Ont
day <- aa$day

# Aggregating to MONTHLY demand
df <- data.frame(month=substr(day,1,7),Ont=Ont)

# Data are in MWh. Divide by 1000000 to get TWh.
dem.month <- aggregate(Ont ~ month, df, 
                       function(x) c(monthdemandTWh = sum(x)/1000000))

# Create 'ts' time series as required by 'naive' and 'snaive'
sdate  <- c(2015,1)
dem.month.ts <- ts(dem.month$TS.1, start=sdate, frequency=12)

#plot(dem.month.ts,ylab="Monthly demand in Ontario (in TWh)",type="b")

# (1) Compute naive/no-change/random walk/persistence forecast
# First forecast is for February 2015
ffcast <- c(2015,2)
naive1 <- naive(dem.month.ts, h=1)

# Compute bias, pbias, and MAPE
forecast <- window(naive1$fitted, start=ffcast) 
observed <- window(naive1$x, start=ffcast)
bias1  <- mean(forecast-observed)
pbias1 <- mean((forecast-observed)/observed)*100
mape1  <- mean(abs((forecast-observed)/observed)*100)

# Performance can also be obtained from accuracy(naive1).
# Note that bias1=(-ME) and pbias1=(-MPE)
# ---------------------------------------

# (2) Compute seasonal naive / seasonal no-change
# First forecast is for January 2018
ffcastS <- c(2018,1)

# snaive automatically uses a monthly seasonality since 
# dem.month.ts was built specifying frequency=12.
naiveS <- snaive(dem.month.ts, h=1)

# Compute bias, pbias, and MAPE
forecastS <- window(naiveS$fitted, start=ffcastS)
observedS <- window(naiveS$x, start=ffcastS)
biasS  <- mean(forecastS-observedS) 
pbiasS <- mean((forecastS-observedS)/observedS)*100
mapeS  <- mean(abs((forecastS-observedS)/observedS)*100)

# ---------------------------------------

# (3) Compute rolling three-month mean
# First forecast is for April 2015 
ffcast3 <- c(2015,4)
# Calculate three-month mean and store in last of the three months
naive3t <- zoo::rollmean(dem.month.ts, 3, align="right")
# Use function naive to move three-month mean forward by one month
naive3 <- naive(naive3t, h=1)

# Compute bias, pbias, and MAPE
forecast3 <- window(naive3$fitted, start=ffcast3)
observed3 <- window(dem.month.ts, start=ffcast3)
bias3  <- mean(forecast3-observed3) 
pbias3 <- mean((forecast3-observed3)/observed3)*100
mape3  <- mean(abs((forecast3-observed3)/observed3)*100)
# ---------------------------------------

# (4) Compute rolling six-month mean
# First forecast is for July 2015 
ffcast6 <- c(2015,7)
# Calculate six-month mean and store in last of the six months
naive6t <- zoo::rollmean(dem.month.ts, 6, align="right")
# Use function naive to move three-month mean forward by one month
naive6 <- naive(naive6t, h=1)

# Compute bias, pbias, and MAPE
forecast6 <- window(naive6$fitted, start=ffcast6)
observed6 <- window(dem.month.ts, start=ffcast6)
bias6  <- mean(forecast6-observed6) 
pbias6 <- mean((forecast6-observed6)/observed6)*100
mape6  <- mean(abs((forecast6-observed6)/observed6)*100)
# ---------------------------------------

cat(paste("Results for ", min(years), "-", max(years), sep="", "\n"))
cat("         no-change: bias=",bias1,
    "%bias=",pbias1,"mape=",mape1,"\n")
cat("seasonal no-change: bias=",biasS,
    "%bias=",pbiasS,"mape=",mapeS,"\n")
cat("rolling 3-mth mean: bias=",bias3,
    "%bias=",pbias3,"mape=",mape3,"\n\n")
cat("rolling 6-mth mean: bias=",bias6,
    "%bias=",pbias6,"mape=",mape6,"\n\n")

cat("Using function <accuracy>:","\n")
print(accuracy(forecast, observed)[,1:5])
print(accuracy(forecastS, observedS)[,1:5])
print(accuracy(forecast3, observed3)[,1:5])
print(accuracy(forecast6, observed6)[,1:5])

cat("\n Performance measures cannot be compared as currently calculated.\n
 Validation sample is not the same for all three forecasting methods!\n")
# ---------------------------------------


# Show observed and forecasts as of Jan 2018
plot(observedS, ylab="Monthly demand (TWh)")
#lines(forecastS, col="red")
lines(window(forecast, start=ffcastS), col="blue")
lines(window(forecast3, start=ffcastS), col="cyan")
lines(window(forecast6, start=ffcastS), col="green")
legend("bottomleft", 
       legend=c("observed","naive","3-mth mean", "6-mth mean"),
       col=c("black","blue","cyan","green"), lty=1)

# -----------------------------------------------------------------
# NOTE: If we wish to use the data from 2003 in any analysis, the |
#       loads associated with the NorthEast blackout need to be   |
#       replaced with other <reasonable> loads.                   |
# -----------------------------------------------------------------

dev.off(dev.cur())
