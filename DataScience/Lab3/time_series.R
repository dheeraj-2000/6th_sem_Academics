
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
start(mdeaths)
end(mdeaths)
time(mdeaths)
cycle(mdeaths)
deltat(mdeaths)


####time series object

time_series1 <- ts(mdeaths, start=1974 ,frequency = 12)
time_series1
#check whether it is a ts object
is.ts(time_series1)
ts.plot(time_series1, main="Deaths From lung disease",ylab ="Monthly Total mean Deaths")
abline(reg = lm(time_series1~time(time_series1)),col="green")

#This property can stabilize variability when a series exhibits increasing variability over time. It may also be used to linearize a rapid growth pattern over time

#Q3  -  Monthly mean values

plot(aggregate(time_series1,FUN = mean))

#Q4 -  Boxplot Monthly

boxplot(time_series1~cycle(time_series1),xlab="Month",ylab = "Deaths",main = "Death from lung disease")

#Q5 - decompose using stl and find TREND


stlts <- stl(time_series1, s.window = "periodic")
plot(stlts)


stlts$trend <- stlts$time.series[,2]
plot(stlts$trend)


# Trend is downward

# Q6  - Seasonality

stlts$seasonal <- stlts$time.series[,1]
plot(stlts$seasonal)

# Yearly  quite uniform patterns


#Q7 Residuals 


stlts$residue <- (time_series1 -(stlts$trend + stlts$seasona))

plot(stlts$residue,main = "Residue after removing trend and seasonality",col = "blue")



#Q8 & Q9 75% trained holtwinter model and predicting 25%

train <- window(mdeaths,start = c(1974,1) ,end=c(1978,6))
train
# here train data for 75% and test is 25% that is 48 months , fchw is prediction , summary give whole detail about model ,rms values ,traindata ,testdata ,alpha,beta,gammavalues
fchw <- hw(train, seasonal = "additive", h = 18)
summary(fchw)
autoplot(fchw)

#Q10 PLOT predicted and actual values

act_value = tail(time_series,18)



df = data.frame( fchw , tail(time_series,18))


X = time(act_value)
dfplt = as.data.frame(data.frame(df$Point.Forecast,df$tail.time_series..18.))
ggplot(dfplt,aes(X))+
  geom_line(aes(y=dfplt$df.Point.Forecast),colour = "blue")+
  geom_line(aes(y=dfplt$df.tail.time_series..18.),colour = "black") + xlab("Time") + ylab("Deaths") + 
  ggtitle("Predicted(blue) and actual (black) values graph")


#Q11 - RMS(predicted,actual)

rmse(df$Point.Forecast,df$tail.time_series..18.)


#Q12  - Tuning Model

#improved

hw_modelt <- HoltWinters(train,alpha = "0.22" ,beta = "0.32" ,gamma = "0.82" )


model.predict <- predict(hw_modelt,n.ahead = 18)
round(model.predict)
p_values= model.predict

act_value = tail(time_series1,18)

rmse(act_value,p_values)
# SINCE RMSE LESS HENCE IMPROVED


#Q13 - ARIMA MODEL - 75% train

train <- window(mdeaths,start = c(1974,1) ,end=c(1978,6))
train
model = auto.arima(train)
model



# Q14 - Predict for next 25% data 
p <- forecast(model, h = 18)
p
plot(mdeaths)
plot(p)
predicted = data.frame(p)
arima_act_values = tail(time_series1,18)

#Q15 Plotting predicted and actual
X = time(arima_act_values)
arima_df <- as.data.frame(data.frame(X,predicted$Point.Forecast,arima_act_values))

ggplot(arima_df,aes(X))+
  geom_line(aes(y=predicted$Point.Forecast),colour="green")+
  geom_line(aes(y=arima_act_values),colour = "black") + xlab("Time") + ylab("Deaths")+
  ggtitle("Predicted(green) and actual (black) values graph")

# Q16 -  RMSE
#TWO WAYS TO FIND
rmse(arima_act_values,predicted$Point.Forecast)
forecast::accuracy(p,mdeaths)[,'RMSE']

tscl <- tsclean(time_series1)
modelcl <- HoltWinters(tscl)
model_without_cleaning <- HoltWinters(time_series1)
plot(model_without_cleaning, main = "Original with Fitted time series : Raw Data")
plot(modelcl, main = "Original with Fitted time series : Cleaned Data")
modelcl$SSE
model_without_cleaning$SSE
