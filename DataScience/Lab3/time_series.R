
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




