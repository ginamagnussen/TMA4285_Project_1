# TMA4285 Time series models, autumn 2018
library("itsmr") # Time series analysis using the Innovations Algorithm
library("TTR") # Functions to create Technical Trading Rules
library("aTSA")
library("tseries") # Time series analysis and computational finance
library("forecast")


dataseries <- ts(read.table("DataEx3G14.txt"))

plot.ts(dataseries)

#Check stationarity
adf.test(dataseries) # Augmented Dickey-Fuller test


a = arma(e,p=2,q=1) # Estimate ARMA model coefficients using maximum likelihood, returns ARMA model
forecast(dataseries,M,a)



# SMA: Calculate moving averages (MA)  of a series, n = number of periods to average over
dsSMA2 <- SMA(dataseries, n = 2) 
dsSMA5 <- SMA(dataseries, n = 5)
dsSMA10 <- SMA(dataseries, n = 10)
dsSMA50 <- SMA(dataseries, n=50)
dsSMA100 <- SMA(dataseries, n=100)

plot.ts(dsSMA50)




fit <- auto.arima(dataseries)
LH.pred<-predict(fit,n.ahead=100) #Predict the next 100 steps of the arima series
plot(dataseries,type="o", lwd="1")
lines(LH.pred$pred,col="red",type="o",lwd="1") 
grid()

fcast <- forecast(fit)
plot(fcast)

