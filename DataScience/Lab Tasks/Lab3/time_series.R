
library(ggplot2)
library(Metrics)
library(forecast)
library(reshape)
data("sunspot.month")
sunspot.month


####time series object

time_series <- ts(sunspot.month, start=1974 ,frequency = 12)
time_series1


#This property can stabilize variability when a series exhibits increasing variability over time. It may also be used to linearize a rapid growth pattern over time

# -  Monthly mean values

plot(aggregate(time_series1,FUN = mean))

#-  Boxplot Monthly

boxplot(time_series1~cycle(time_series1),xlab="Month",ylab = "Deaths",main = "Death from lung disease")

# - decompose using stl and find TREND

stlts <- stl(time_series1, s.window = "periodic")
plot(stlts)


#  - Seasonality

stlts$seasonal <- stlts$time.series[,1]
plot(stlts$seasonal)

# Yearly  quite uniform patterns




# 75% trained holtwinter model and predicting 

train <- window(mdeaths,start = c(1974,1) ,end=c(1978,6))
train
fchw <- hw(train, seasonal = "additive", h = 18)
summary(fchw)
autoplot(fchw)


# PLOT predicted and actual values

act_value = tail(time_series1,18)

df = data.frame( fchw , tail(time_series1,18))


X = time(act_value)
dfplt = as.data.frame(data.frame(df$Point.Forecast,df$tail.time_series..18.))
ggplot(dfplt,aes(X))+
  geom_line(aes(y=dfplt$df.Point.Forecast),colour = "blue")


# RMS(predicted,actual)

rmse(df$Point.Forecast,df$tail.time_series..18.)


hw_modelt <- HoltWinters(train,alpha = "0.22" ,beta = "0.32" ,gamma = "0.82" )


model.predict <- predict(hw_modelt,n.ahead = 18)
round(model.predict)
p_values= model.predict

act_value = tail(time_series1,18)

rmse(act_value,p_values)



# Predict for next 25% data 
p <- forecast(model, h = 18)
p
plot(mdeaths)
plot(p)
predicted = data.frame(p)
arima_act_values = tail(time_series1,18)


# Plotting predicted and actual
X = time(arima_act_values)
arima_df <- as.data.frame(data.frame(X,predicted$Point.Forecast,arima_act_values))

ggplot(arima_df,aes(X))+
  geom_line(aes(y=predicted$Point.Forecast),colour="green")


#rmse error

forecast::accuracy(p,mdeaths)[,'RMSE']





# Q19 Cleaning data
tscl <- tsclean(time_series1)
modelcl <- HoltWinters(tscl)
model_without_cleaning <- HoltWinters(time_series1)
plot(model_without_cleaning, main = "Original with Fitted time series : Raw Data")
plot(modelcl, main = "Original with Fitted time series : Cleaned Data")
modelcl$SSE
model_without_cleaning$SSE
