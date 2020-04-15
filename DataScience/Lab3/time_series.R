
data("sunspot.month")
sunspot.month
View(sunspot.month)
plot(sunspot.month)


####time series object

time_series <- ts(sunspot.month, start=1749 ,frequency = 12)
time_series
is.ts(time_series)
ts.plot(time_series)

