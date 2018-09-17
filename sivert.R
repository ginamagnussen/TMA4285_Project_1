
# Testing out for myself (Sivert)
library(tseries)

dataseries <- ts(read.table("DataEx3G14.txt"))
plot.ts(dataseries)

model = arma(dataseries, order = c(2, 1), lag = NULL, coef = NULL,
     include.intercept = TRUE, series = NULL, qr.tol = 1e-7)
     
summary(model)
forecast(dataseries)#,M,a)
