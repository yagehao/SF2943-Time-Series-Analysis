# import library
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
#library(sarima)
#library(itsmr)

# data preparation
data <- read.csv("C:/Users/haoya/OneDrive/Desktop/SF2943/project/PART1/GlobalTemperatures.csv")
datalim =data[1500:3180,1:3] # time 1874.12-2014.12
datalim = na.omit(datalim)
data_ts_raw = ts(data = datalim$LandAverageTemperature, start = decimal_date(ymd('1875-01-01'))) #, frequency = 12) # convert into time series

# explanatory data analysis
plot(data_ts_raw) # plot time series
plot(datalim$LandAverageTemperatureUncertainty) # plot uncertainty
temp = datalim$LandAverageTemperature
plotc(trend(temp, 1)) # plot trend
plotc(season(temp, 12)) # plot seasonality

# remove trend and seasonality
data_ts1 = diff(data_ts_raw) # generate first difference of ts
plot(data_ts1)
data_ts2 = diff(data_ts_raw, lag = 12) # generate a diff of ts with lag=12
plot(data_ts2)
data_ts <- data_ts2

# ACF and PACF
layout(1:2)
acf(data_ts, lag.max=50)
pacf(data_ts, lag.max=50)

# model fit and residual check
# MA(12)
model1 <- arima(data_ts, order = c(0,0,12)) 
AIC(model1)
checkresiduals(model1)
# ARMA(12,12)
model2 <- arima(data_ts, order = c(12,0,12)) 
AIC(model2)
checkresiduals(model2)
# ARMA(1,12)
model3 <- arima(data_ts, order = c(1,0,12)) 
AIC(model3)
checkresiduals(model3)
# choose parameter p and q automatically
automodel <- auto.arima(data_ts, stepwise=FALSE, parallel=TRUE, num.cores=4, approximation=FALSE)
AIC(automodel)
checkresiduals(automodel)





