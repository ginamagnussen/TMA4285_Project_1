# TMA4285 Time series models, autumn 2018



ds <- read.table("/home/shomea/g/ginama/H2018/TMA4285 Time series models/Exercise 3 (Project 1)/DataEx3G14.txt")
x <- 1:dim(ds)[1]; y <- ds[,1]
frame <- data.frame(time = x, obs = y)
plot(x, ds[,1], "l")

min(y)
max(y)
mean(y) # + confidence interval? 

dataseries <- ts(read.table("DataEx3G14.txt"))
plot.ts(dataseries)

library("itsmr")
M = c("exp","season",12,"trend",1)
e = Resid(dataseries,M)
test(e)
a = arma(e,p=2,q=1)
forecast(dataseries,M,a)


M = c("diff",1)
e = Resid(dataseries,M)
a = arma(e,1,0)
print(a)


#Looking for 
#A) a trend
#B) a seasonal component
#C) any apparent sharp changes in behavior
#D) any outlying observations


library("TTR")
dsSMA2 <- SMA(dataseries, n = 2)
dsSMA5 <- SMA(dataseries, n = 5)
dsSMA10 <- SMA(dataseries, n = 10)
dsSMA50 <- SMA(dataseries, n=50)
dsSMA100 <- SMA(dataseries, n=100)

plot.ts(dsSMA50)



# decompose(dataseries) funker hvis man deler opp dataserien? ma vite seasons på forhand? 


#Check stationary
library("aTSA")
adf.test(dataseries)



library(tseries)
adf.test(dataseries)


library("forecast")
auto.arima(dataseries)

fit <- auto.arima(dataseries)
LH.pred<-predict(fit,n.ahead=100)
plot(dataseries,type="o", lwd="1")
lines(LH.pred$pred,col="red",type="o",lwd="1")
grid()

fcast <- forecast(fit)
plot(fcast)


ets(dataseries)
plot(ets(dataseries, model = 'ZZZ'))

