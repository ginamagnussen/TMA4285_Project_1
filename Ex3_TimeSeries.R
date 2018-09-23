# TMA4285 Time series models, autumn 2018

library("itsmr") # Time series analysis using the Innovations Algorithm
library("TTR") # Functions to create Technical Trading Rules
library("aTSA")
library("tseries") # Time series analysis and computational finance
library("forecast")
library("ggplot2")


# Import ARMA-series
dataseries <- ts(read.table("DataEx3G14.txt"))


# 1. Plotting of relevant statistics
###############################################

plot.ts(dataseries) # Plot of the time series itself

acf(dataseries) # Autocorrelation function

Pacf(dataseries) # Partial ACF

#Check stationarity
adf.test(dataseries) # Augmented Dickey-Fuller test
# p-value < 0.05 indicates the TS is stationary


# Estimate ARMA model coefficients using maximum likelihood
############################################################

## ARIMA
arimaFit <- arima(dataseries, order = c(2,0,1), method = "ML")
arimaFit
res_scaled <- arimaFit$residuals/sqrt(arimaFit$sigma2)
plot(res_scaled, main ="Rescaled residuals")
hist(res_scaled, main ="Histogram of rescaled residuals")

## Order selection

# To check if chosen model is good
acf(res_scaled) 
pacf(res_scaled) 

acf(dataseries) # To find order of MA(q) (lags)
pacf(dataseries) # To find order of AR(p)

vcov(arimaFit)


# QQplots
qqnorm(res_scaled, main = "Q-Q Plot: ARMA model"); qqline(res_scaled) #Axis?

# Predicted values of the model for the last 100 samples of the dataseries
Xhat <- list()
phi1 <- arimaFit$coef[1]; phi2 <- arimaFit$coef[2]; theta1 <- arimaFit$coef[3]; res <- arimaFit$residuals
for (t in 3:500){
  Xhat[t] = phi1*dataseries[t-1] + phi2*dataseries[t-2] + theta1*res[t-1] + res[t]
}
plot(401:500, dataseries[401:500], "l", main = "Time series and predicted values", xlab = "Time step", ylab = "Value")
lines(401:500, Xhat[401:500], col = "red")



# Forecast
##################################################################
fcast = forecast(arimaFit, h=20)
pred <- predict(arimaFit, n.ahead = 20, se.fit = TRUE)
pplot(fcast)



# AIC
arimaFit$aic

# Plots of AIC
# Which order p,q is best, compare AIC
frame <- data.frame(p = 0, q = 0, aic = 0)
for(p in 1:5){
  for(q in 1:5){
    fit <- arima(dataseries, order = c(p,0,q), method = "ML")
    frame <- rbind(frame, c(p,q,fit$aic))
  }
}
frame <- frame[-1,]
ggplot(frame, aes(p,q)) + geom_raster(aes(fill = aic))



