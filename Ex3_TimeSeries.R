# TMA4285 Time series models, autumn 2018

library("itsmr") # Time series analysis using the Innovations Algorithm
# M = c("exp","season",12,"trend",1) # Not working with "exp"

M <- c("season", 12, "trend", 1) # Set data model: Subtracting trend component and seasonal components
e = Resid(dataseries,M) 
test(e) # Test residuals (adjusted series) for stationarity and randomness
a = arma(e,p=2,q=1) # Estimate ARMA model coefficients using maximum likelihood, returns ARMA model
forecast(dataseries,M,a)

# New model: Differencing the data 
M = c("diff",1)
e = Resid(dataseries,M)
a = arma(e,1,0) 

print(a)

library("TTR") # Functions to create Technical Trading Rules
library("aTSA")
library("tseries") # Time series analysis and computational finance
library("forecast")


# Import ARMA-series
setwd("~/Tidsrekker/TMA4285_Project_1")
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




# Estimate ARMA model coefficients using maximum likelihood, returns ARMA model
############################################################

armaFit = arma(dataseries, order = c(2, 1), include.intercept = TRUE, qr.tol = 1e-07)
summary(armaFit)
plot(armaFit) # makes three plots: data, ACF, PACF 

coef(armaFit)
vcov(armaFit)
residuals(armaFit) # 500 resiudals: is this W?
fitted(armaFit) # Presumeably the X-hat vaules
print(armaFit)

acf(residuals(armaFit), na.action=na.remove)
pacf(residuals(armaFit), na.action=na.remove)


fit <- auto.arima(dataseries)
LH.pred<-predict(fit,n.ahead=100) #Predict the next 100 steps of the arima series
plot(dataseries,type="o", lwd="1")
lines(LH.pred$pred,col="red",type="o",lwd="1") 
grid()


# Forecasting
#######################################################################
 # Here we can i) Forecast like on p. 101 in the book or ii) Use the forecast function in R
class(armaFit)
fcast = forecast(armaFit, h=20)


ets(dataseries) # Exponential smoothing state space model
plot(ets(dataseries, model = 'ZZZ')) #model = 'ZZZ' : error type, trend type and season type, Z = automatically selected

