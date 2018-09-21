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
summary(arimaFit)
plot(arimaFit$residuals) # Scale these?
acf(arimaFit$residuals)
pacf(arimaFit$residuals)
hist(arimaFit$residuals)
arimaFit$var.coef


# Forecasting
#######################################################################
 # Here we can i) Forecast like on p. 101 in the book or ii) Use the forecast function in R
class(armaFit)
fcast = forecast(dataseries, h=20)


ets(dataseries) # Exponential smoothing state space model
plot(ets(dataseries, model = 'ZZZ')) #model = 'ZZZ' : error type, trend type and season type, Z = automatically selected

# AICC

# ARIMA
# Forecast plot
# AICC
# 
# Rescaled residuals
# Histogram of residuals
# QQ-plot
# (P)ACF plot
# 

