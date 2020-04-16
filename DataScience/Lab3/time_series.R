
data("mdeaths")
mdeaths
View(sunspot.month)
plot(sunspot.month)

library(ggplot2)
library(Metrics)
library(forecast)
library(reshape)
data("mdeaths")
mdeaths


#start end time frequency cycle - The deltat() function returns the fixed time interval between observations and the frequency() function returns the number of observations per unit time. Finally, the cycle() function returns the position in the cycle of each observation
start(sunspot.month)
end(sunspot.month)
time(sunspot.month)
cycle(sunspot.month)
deltat(sunspot.month)


####time series object

time_series1 <- ts(mdeaths, start=1974 ,frequency = 12)
time_series1
#check whether it is a ts object
is.ts(time_series1)
ts.plot(time_series1, main="Sunspot",ylab ="Monthly Total mean Sunspot number")
abline(reg = lm(time_series1~time(time_series)1),col="green")

#This property can stabilize variability when a series exhibits increasing variability over time. It may also be used to linearize a rapid growth pattern over time

#Q3  -  Monthly mean values

plot(aggregate(time_series1,FUN = mean))

#Q4 -  Boxplot Monthly

boxplot(time_series1~cycle(time_series1),xlab="Month",ylab = "Sunspot Number",main = "Sunspot")

#Q5 - decompose using stl and find TREND


stlts <- stl(time_series1, s.window = "periodic")
plot(stlts)


stlts$trend <- stlts$time.series[,2]
plot(stlts$trend)


# Trend is downward

# Q6  - Seasonality

stlts$seasonal <- stlts$time.series[,1]
plot(stlts$seasonal)


