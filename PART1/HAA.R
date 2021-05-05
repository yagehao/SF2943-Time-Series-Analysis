library(itsmr)
data <- read.csv("~/KTH/Time series/GlobalTemperatures.csv")

# Removing the na values and plots the uncertainty
datalim = na.omit(datalim) 
plot(datalim$LandAverageTemperatureUncertainty)

#removes the first values since they have large uncertainty
datalim =data[1500:3180,1:3]


# Plot trend and seasonality
temp = datalim$LandAverageTemperature
plotc(trend(temp, 1))
plotc(season(temp, 12))


# Remove the seasonality and trend
temp_clean <- temp - season(temp, 12) - trend(temp, 1)
plotc(temp_clean)

# Acf and pacf
acf(temp_clean)
pacf(temp_clean)


# different estimators
#print(yw(temp_clean, p=1))
#print(burg(temp_clean, p=1)) 
#print(arma(temp_clean,p=12,q=12))

# AR for 1-12 coeffecicents 

error = rep(0, 12)
for (i in 1:12) {
  model3 = ar(temp_clean[0:1620],i)
  
  pred = predict(model3, n.ahead = 61)
  error[i] <- sum(temp_clean[1621:1681] - pred$pred)
  print(i)
  
}
plot(1:12, error)

