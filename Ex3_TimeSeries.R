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


# Estimate ARMA model coefficients using maximum likelihood
############################################################

## ARIMA
arimaFit <- arima(dataseries, order = c(2,0,1), method = "ML")
arimaFit
arimaFit$sigma2
res_scaled <- arimaFit$residuals/sqrt(arimaFit$sigma2)
plot(res_scaled)
hist(res_scaled)

## Order selection
acf(res_scaled) # To find order of MA(q) (lags)
pacf(res_scaled) # To find order of AR(p)

acf(dataseries)
pacf(dataseries)

vcov(arimaFit)


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
ggplot(frame, aes(p,q)) + geom_raster(aes(fill = aic)) #+ scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) # Alternative color gradient



