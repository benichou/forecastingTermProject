#********************************************************#
#   State Space Model
#********************************************************#

library(timeSeries)
library(forecast)
options(digits=4)
source('dataAggregation.r')

# Training set  : 2014/01/01-2016/12/31
# Validation set: 2017/01/01-2018/12/31
# Test set      : 2019/01/01-2020/12/31

WFEC.ts <- ts(dailyPeaksWFEC$Y.WFEC, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1097    # 2014/01/01
endTrain <- 2192    # 2016/12/31
endValid <- 2922    # 2018/12/31

in.sample  <- ts(window(WFEC.ts, begTrain, endTrain))
out.sample <- ts(window(WFEC.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.opt <- ets(in.ts)

ets.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(WFEC.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=ets.opt, h=1, use.initial.values=T)
  ets.val[i] <- fc$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval <- rbind(accuracy(ets.opt$fitted, in.ts)[,1:5],
                  accuracy(ets.val, out.sample)[,1:5])
rownames(ets.eval) <- make.names(c("Training","Validation"))
print(ets.eval)