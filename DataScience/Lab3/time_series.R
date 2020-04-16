
data("sunspot.month")
sunspot.month
View(sunspot.month)
plot(sunspot.month)

library(ggplot2)
library(Metrics)
library(forecast)
library(reshape)
data("sunspot.month")
sunspot.month


#start end time frequency cycle - The deltat() function returns the fixed time interval between observations and the frequency() function returns the number of observations per unit time. Finally, the cycle() function returns the position in the cycle of each observation
start(sunspot.month)
end(sunspot.month)
time(sunspot.month)
cycle(sunspot.month)
deltat(sunspot.month)


####time series object

time_series <- ts(sunspot.month, start=1749 ,frequency = 12)
#time_series
#check whether it is a ts object
is.ts(time_series)
ts.plot(time_series, main="Sunspot",ylab ="Monthly Total mean Sunspot number")
abline(reg = lm(time_series~time(time_series)),col="green")

#This property can stabilize variability when a series exhibits increasing variability over time. It may also be used to linearize a rapid growth pattern over time

#Q3  -  Monthly mean values

plot(aggregate(time_series,FUN = mean))

#Q4 -  Boxplot Monthly

boxplot(time_series~cycle(time_series),xlab="Month",ylab = "Sunspot Number",main = "Sunspot")
