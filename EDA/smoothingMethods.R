#********************************************************#
#  Smoothing methods
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

#==========================================================
# (1) Simple exponential smoothing
#==========================================================
ses.opt <-ses(in.sample, h=1)
cat("SES - Optimal value of alpha: ", ses.opt$model$par[1], "\n")

ses.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameter
for (i in 1:n.out) {
  tmp.ts <- ts(window(WFEC.ts, begTrain, endTrain+i-1))
  fc <- forecast(tmp.ts, model=ses.opt, h=1, use.initial.values=T)
  ses.val[i] <- fc$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Simple Exponential Smoothing:","\n")
ses.eval <- rbind(accuracy(ses.opt$fitted, in.sample)[,1:5],
                  accuracy(ses.val, out.sample)[,1:5])
rownames(ses.eval) <- make.names(c("Training","Validation"))
print(ses.eval)


#==========================================================
# (2) Holt Method
#==========================================================
holt.opt <- holt(in.sample, h=1, initial="simple")
cat("Holt - optimal value of alpha:", holt.opt$model$par[1],
    "; optimal value of beta:",holt.opt$model$par[2],"\n")

holt.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
  tmp.ts <- ts(window(WFEC.ts, begTrain, endTrain+i-1))
  fc <- forecast(tmp.ts, model=holt.opt, h=1, use.initial.values=T)
  holt.val[i] <- fc$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt Method:","\n")
holt.eval <- rbind(accuracy(holt.opt$fitted, in.sample)[,1:5],
                   accuracy(holt.val, out.sample)[,1:5])
rownames(holt.eval) <- make.names(c("Training","Validation"))
print(holt.eval)


#==========================================================
# (3) Holt-Winters Method
#==========================================================
# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)

hw.a.opt <- hw(in.ts, seasonal="additive", h=1, initial="simple")
hw.m.opt <- hw(in.ts, seasonal="multiplicative", h=1,initial="simple")

cat("Holt-Winters Additive - optimal value of \n",
    "alpha =", hw.a.opt$model$par[1], "beta =", hw.a.opt$model$par[2],
    "gamma =", hw.a.opt$model$par[3], "\n")
cat("Holt-Winters Multiplicative - optimal value of \n",
    "alpha =", hw.m.opt$model$par[1], "beta =", hw.m.opt$model$par[2],
    "gamma =", hw.m.opt$model$par[3], "\n")

hw.a.val <- ts(numeric(n.out))
hw.m.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
  tmp.ts <- ts(window(WFEC.ts, begTrain, endTrain+i-1), frequency=7)
  fc.a <- forecast(tmp.ts, model=hw.a.opt, h=1, use.initial.values=T)
  fc.m <- forecast(tmp.ts, model=hw.m.opt, h=1, use.initial.values=T)
  hw.a.val[i] <- fc.a$mean[1]
  hw.m.val[i] <- fc.m$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt-Winter Method:","\n")
hw.eval <- rbind(accuracy(hw.a.opt$fitted, in.ts)[,1:5],
                 accuracy(hw.a.val, out.sample)[,1:5],
                 accuracy(hw.m.opt$fitted, in.ts)[,1:5],
                 accuracy(hw.m.val, out.sample)[,1:5])
rownames(hw.eval) <- make.names(c("Training    Additive",
                                  "Validation  Additive",
                                  "Training    Multiplicative",
                                  "Validation  Multiplicative"))
print(hw.eval)


#==========================================================
# (4) Taylor Method
#==========================================================
# in.sample with double seasonality m1=7 & m2=364
# use 364 (multiple of 7) instead of 365 for the annual seasonality
in.msts <- msts(in.sample, seasonal.periods=c(7,364))

dshw.opt <- dshw(in.msts, h=1)

dshw.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
  tmp <- msts(window(WFEC.ts, begTrain, endTrain+i-1),
              seasonal.periods=c(7,364))
  # refit model with smoothing parameters found in optimal model
  tmp.fit <- dshw(tmp,model=dshw.opt)
  dshw.val[i] <- forecast(tmp.fit, h=1)$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Taylor Method:","\n")
dshw.eval <- rbind(accuracy(dshw.opt$fitted, in.msts)[,1:5],
                   accuracy(dshw.val, out.sample)[,1:5])
rownames(dshw.eval) <- make.names(c("Training","Validation"))
print(dshw.eval)
