# TMA4285 Time series models, autumn 2018

dataseries <- ts(read.table("DataEx3G14.txt"))
plot.ts(dataseries)

# min(dataseries)
# max(dataseries)
# mean(dataseries) # + confidence interval?

library("itsmr") # Time series analysis using the Innovations Algorithm
# M = c("exp","season",12,"trend",1) # Not working with "exp"

M <- c("season", 12, "trend", 1) # Set data model
e = Resid(dataseries,M) 
test(e) # Test residuals for stationarity and randomness
a = arma(e,p=2,q=1) # Estimate ARMA model coefficients using maximum likelihood, returns ARMA model
forecast(dataseries,M,a)

# New model
M = c("diff",1)
e = Resid(dataseries,M)
a = arma(e,1,0) 

print(a)


#Looking for 
#A) a trend
#B) a seasonal component
#C) any apparent sharp changes in behavior
#D) any outlying observations


library("TTR") # Functions to create Technical Trading Rules
# SMA: Calculate moving averages (MA)  of a series, n = number of periods to average over
dsSMA2 <- SMA(dataseries, n = 2) 
dsSMA5 <- SMA(dataseries, n = 5)
dsSMA10 <- SMA(dataseries, n = 10)
dsSMA50 <- SMA(dataseries, n=50)
dsSMA100 <- SMA(dataseries, n=100)

plot.ts(dsSMA50)



# decompose(dataseries) funker hvis man deler opp dataserien? ma vite seasons på forhand? 


#Check stationarity
library("aTSA")
adf.test(dataseries) # Augmented Dickey-Fuller test


library(tseries) # Time series analysis and computational finance
adf.test(dataseries)


library("forecast")
auto.arima(dataseries) # Fit best ARIMA model to univariate time series

fit <- auto.arima(dataseries)
LH.pred<-predict(fit,n.ahead=100)
plot(dataseries,type="o", lwd="1")
lines(LH.pred$pred,col="red",type="o",lwd="1")
grid()

fcast <- forecast(fit)
plot(fcast)


ets(dataseries)
plot(ets(dataseries, model = 'ZZZ'))

