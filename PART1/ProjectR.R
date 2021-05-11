library(itsmr)
library('forecast')


rm(list = ls()) ## clearing r2studio environment

data <- read.csv(file = 'GlobalTemperatures.csv')


train_data <- data[1501:3132, "LandAverageTemperature"]

test_data <- data[3133:nrow(data), "LandAverageTemperature"]

time_series <- data[1501:nrow(data),  "LandAverageTemperature"]


# Acf and pacf
acf(time_series)
pacf(time_series)


# Remove the seasonality and trend
time_series <- time_series - season(time_series, 12) - trend(time_series, 1)

train_data_stationary = time_series[1:1632]
test_data_stationary = time_series[(1633):length(time_series)]


###
plotc(time_series)
acf(time_series, lag.max=200)
pacf(time_series, lag.max=200)


### Finding the parameters that results in the lowest AIC
#best_aic = Inf
## loop over AR order
#for (p in 0:15) {
  ## loop over MA
#  for (q in 0:12) {
    
#    fit_model <- arima(train_data_stationary, order=c(p, 0, q))
#    current_aic = fit_model$aic
    
#    if (current_aic < best_aic) {
#      best_p = p
#      best_q = q
#      best_aic = current_aic
#    }
#  }
#}

best_p = 4
best_q = 13


fit_model <- arima(train_data_stationary, order=c(best_p, 0, best_q))
current_aic = fit_model$aic
checkresiduals(fit_model)

## comment out to fit on train data
# test_data_stationary = train_data_stationary

# Do forecasting
forecast_data <- forecast :: forecast(object=test_data_stationary, model=fit_model)
autoplot(forecast_data)

MSRE = mean(forecast_data$residuals^2)



# And plot the forecasted values vs. the actual test data:
plot(x=test_data_stationary, y=forecast_data$fitted,xlab='Actual', ylab='Predicted')


# It could help more to look at the following plot:
plot(test_data_stationary, type='l', col=rgb(0, 0, 1, alpha=0.7),
     xlab='Time', ylab='Value', xaxt='n', ylim=c(min(forecast_data$fitted)-1,max(forecast_data$fitted)+1))

ticks <- seq(from=1, to=length(test_data_stationary), by=floor(length(test_data_stationary)/4))
times <- full_data$UnixHour[-training_indices]
axis(1, lwd=0, lwd.ticks=1, at=ticks, labels=times[ticks])
lines(forecast_data$fitted, col=rgb(1, 0, 0, alpha=0.7))
legend('topright', legend=c('Actual', 'Predicted'), col=c('blue', 'red'),
       lty=1, bty='n')


