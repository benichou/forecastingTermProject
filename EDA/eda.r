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


sum(is.na(sppdata$WFEC)) # counting the number of na in column WFEC

