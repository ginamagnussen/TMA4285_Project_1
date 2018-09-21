# TMA4285 Time series models, autumn 2018

library("itsmr") # Time series analysis using the Innovations Algorithm
library("TTR") # Functions to create Technical Trading Rules
library("aTSA")
library("tseries") # Time series analysis and computational finance
library("forecast")
library("ggplot2")


# Import ARMA-series
#setwd("~/Tidsrekker/TMA4285_Project_1")
dataseries <- ts(read.table("DataEx3G14.txt"))


# 1. Plotting of relevant statistics
###############################################

plot.ts(dataseries) # Plot of the time series itself

acf(dataseries) # Autocorrelation function

Pacf(dataseries) # Partial ACF

#Check stationarity
adf.test(dataseries) # Augmented Dickey-Fuller test
                      # p-value < 0.05 indicates the TS is stationary
kpss.test(dataseries)


# The time series is assumed to be more or less without trend or seasonality, 
# meaning that the sample points should be approx iid N(0, sigma^2). 
# An ARMA model should then fit quite well.

# Estimate ARMA model coefficients using maximum likelihood, returns ARMA model
############################################################

## ARIMA (to forecast)
arimaFit <- arima(dataseries, order = c(2,0,1))
arimaFit
arimaFit$sigma2
res_scaled <- arimaFit$residuals/sqrt(arimaFit$sigma2)
plot(res_scaled) # Scale these?
acf(res_scaled)
pacf(res_scaled)
hist(res_scaled)


# QQplots
qqnorm(res_scaled, main = "Q-Q Plot: ARMA model"); qqline(res_scaled) #Axis?

# Predicted values of the model for the last 100 samples of the dataseries
Xhat <- list()
phi1 <- arimaFit$coef[1]; phi2 <- arimaFit$coef[2]; theta1 <- arimaFit$coef[3]; res <- arimaFit$residuals
for (t in 3:500){
  Xhat[t] = phi1*dataseries[t-1] + phi2*dataseries[t-2] + theta1*res[t-1] + res[t]
}
plot(401:500, dataseries[401:500], "l")
lines(401:500, Xhat[401:500], col = "red")
# Add title, xlab and ylab


# Forecasting
#######################################################################
 # Here we can i) Forecast like on p. 101 in the book or ii) Use the forecast function in R
class(arimaFit)
fcast = forecast(arimaFit, h=20)
plot(fcast)


# AICC
arimaFit$aic


# Estimate parameters: phi, theta, sigma^2, mean
frame <- list()
B <- 100
for (j in 1:B){
  smpl <- sample(res_scaled, 500, replace = TRUE)
  fit <- arima(smpl, order = c(2,0,1))
}

