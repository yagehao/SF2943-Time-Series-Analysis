# import library
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
#library(sarima)
library(itsmr)

# data preparation
data <- read.csv("C:/Users/haoya/OneDrive/Desktop/SF2943/project/PART1/GlobalTemperatures.csv")
datalim =data[1501:3132,1:3] # time 1875.1-2010.12
datalim = na.omit(datalim)
data_ts_raw = ts(data = datalim$LandAverageTemperature, start = decimal_date(ymd('1875-01-01')))#, frequency = 12) # convert into time series

# explanatory data analysis
plot(data_ts_raw)
data_ts_plot = ts(data = datalim$LandAverageTemperature[500:800], start = decimal_date(ymd('1875-01-01')), frequency=12)
plot(data_ts_plot) # plot part of the whole time series
plot(datalim$LandAverageTemperatureUncertainty) # plot uncertainty
temp = datalim$LandAverageTemperature
plotc(trend(temp, 1)) # plot trend
plotc(season(temp, 12)) # plot seasonality

# remove trend and seasonality
data_ts1 = diff(data_ts_raw) # generate first difference of ts
plot(data_ts1) # remove trend
data_ts = diff(data_ts1, lag = 12) # generate a diff of ts with lag=12
plot(data_ts) # remove seasonality

# ACF and PACF
layout(1:2)
acf(data_ts, lag.max=50)
pacf(data_ts, lag.max=50)

# model fit and residual check
# auto.arima on preprocessed stationary time series
automodel1 <- auto.arima(data_ts, stepwise=FALSE, parallel=TRUE, num.cores=4, approximation=FALSE)
summary(automodel1)
checkresiduals(automodel1)
# auto.arima on original seasonal increasing time series
train = data[1501:3132, 1:3]
test = data[3133:3192, 1:3]
train_ts = ts(data = train$LandAverageTemperature, start = decimal_date(ymd('1875-01-01')), frequency = 12)
automodel2 <- auto.arima(train_ts, parallel=TRUE, num.cores=4, stepwise=FALSE, approximation=FALSE)
summary(automodel2)
checkresiduals(automodel2)
# manually fit ARMA(4,1), choose parameters based on auto.arima result
model1 <- arima(data_ts, order = c(4,0,1)) 
summary(model1)
checkresiduals(model1)
