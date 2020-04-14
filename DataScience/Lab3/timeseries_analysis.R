data("Nile")
Nile
data("EuStockMarkets")
View(EuStockMarkets)


length(Nile)
length(EuStockMarkets)

plot(Nile)
plot(Nile, xlab = "Year", ylab = "River Volume",main = "Annual River Nile Volume , 1871-1970",type = "b")
ts.plot(Nile)

plot(EuStockMarkets)
ts.plot(EuStockMarkets)
data("UKDriverDeaths")
UKDriverDeaths

#################################
#start end time frequency cycle - The deltat() function returns the fixed time interval between observations and the frequency() function returns the number of observations per unit time. Finally, the cycle() function returns the position in the cycle of each observation
start(Nile)
end(Nile)
time(Nile)
time(EuStockMarkets)
cycle(EuStockMarkets)
deltat(EuStockMarkets)
deltat(Nile)
frequency(Nile)
frequency(EuStockMarkets)
start(EuStockMarkets)

#plot
ts.plot(EuStockMarkets, col = 1:4, xlab = "Year", ylab = "Index Value", main = "Major European Stock Indices, 1991-1998")

# Add a legend to your ts.plot
legend("topleft", colnames(EuStockMarkets), lty = 1, col = 1:4, bty = "n")

####time series object


time_series <- ts(UKDriverDeaths, start=1969 ,frequency = 12)
time_series
is.ts(time_series)
ts.plot(time_series)

#This property can stabilize variability when a series exhibits increasing variability over time. It may also be used to linearize a rapid growth pattern over time

linear_growth <- log(time_series)
ts.plot(time_series)
ts.plot(linear_growth)
length(dts)

####decompose and trends using stl

decomposets <- decompose(time_series)
decomposets
plot(decomposets)
# we can see a increasing trend
stlts <- stl(time_series, s.window = "periodic")
plot(stlts)

frequency(time_series)

#deseonalizing and detrend

dts <- diff(time_series)
plot(dts)
length(dts)

#using holtwinters model
len75 = (length(time_series)*75)/100
leny = (len75/12) - 1
leny
hots <- ts(UKDriverDeaths, start=c(1969,1) , end = c(1969+leny,12) ,frequency = 12)
plot(hots)


hotsm <- HoltWinters(hots)
hotsm
plot(hotsm)

len25  =  length(time_series) - length(hots)
p <- predict(hotsm, len25, prediction.interval = TRUE)
plot(hotsm,p)
sqrt(mean((hotsm-p)^2))
accuracy(hotsm , p)



#ARIMA
train_ats <- ts(UKDriverDeaths[1:144], start=c(1969,1) , end = c(1969+leny,12) ,frequency = 12)
train_ats <- diff(train_ats)
plot(train_ats)
test_ats <- ts(UKDriverDeaths[145:192], start=1981,frequency = 12)
test_ats
model = auto.arima(train_ats)
p <- predict(model,48)
autoplot(p)

train <- window(UKDriverDeaths,start = c(1969,1) ,end=c(1980,12))
train
model = auto.arima(train)
p <- forecast(model, h = 48)
p
plot(UKDriverDeaths)
plot(p)
accuracy(p,UKDriverDeaths)[,'RMSE']


#########################
train
fchw <- hw(train, seasonal = "additive", h = 48)
fchw
autoplot(fchw)
accuracy(p,UKDriverDeaths)[,'RMSE']
