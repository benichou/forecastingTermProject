#
# Program: empForQC
#
# Purpose: Logging exercise
#
# Written by: D.J. Dupuis, 10 August 2015
#
# Updated: 3 January 2017
#         29 December 2017
#
# ------------------------------------------------------
#
# Set locale to English language
Sys.setlocale("LC_TIME", "C")
library(astsa)
library(timeSeries)
library(timeDate)

load("EmpForQc.RData") ## load the data


ts1 <- timeSeries(y$TS.1, time(y))

plot(ts1, ylab="number of employees paid by the hour in the Forestry, Logging and Support sector in the province of Quebec (Monthly)") ## it does not look very stationary

acf2(ts1, main="personal savings rate (US), %-seasonally adj.") 


acf2(diff(ts1)[-6]) # bad
acf2(diff(ts1)[-4]) # bad

acf2(diff(ts1)[-12]) # annual cycle is good in the logging industry 


## We thus observed that the number of paid hours for employees of the quebec logging industry are not stationary and follow a seasonal cycle
## Doing a 12 month differencing given the industry works on an annual cycle is a good strategy

