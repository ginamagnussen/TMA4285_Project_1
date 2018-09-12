# TMA4285 Time series models, autumn 2018



ds <- read.table("/home/shomea/g/ginama/H2018/TMA4285 Time series models/Exercise 3 (Project 1)/DataEx3G14.txt")
x <- 1:dim(ds)[1]; y <- ds[,1]
frame <- data.frame(time = x, obs = y)
plot(x, ds[,1], "l")

min(y)
max(y)
mean(y) # + confidence interval? 
