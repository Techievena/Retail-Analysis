setwd("~/R/Working Directory/Retail-Analysis")
library(data.table)
library(plyr)
library(forecast)
traindata<-read.csv("Training dataset.csv")
testdata<-read.csv("Testing dataset.csv")

acf(time_series_unit, lag.max = 20) #Positive corelation upto 14
pacf(time_series_unit, lag.max = 20) #Positive partial-corelation upto 7
#Seasonality of 7
acf(time_series_frequency, lag.max = 20) #Positive corelation upto 14
pacf(time_series_frequency, lag.max = 20) #Positive partial-corelation upto 5

acf(diff(time_series_unit), lag.max = 20, na.action = na.pass, ci.type='ma') #lag1 correlation = -0.5 differencing done
acf(diff(time_series_frequency), lag.max = 20, na.action = na.pass, ci.type='ma') #lag1 correlation = -0.5 differencing done

sd(time_series_unit) #89.2552
sd(diff(time_series_unit)['2014-04-02/']) #117.0078
sd(diff(diff(time_series_unit)['2014-04-02/'])['2014-04-03/']) #204.1791
sd(diff(diff(diff(time_series_unit)['2014-04-02/'])['2014-04-03/'])['2014-04-04/']) #374.975

#order of differencing must be 0/1

tsdiag(arima(time_series_unit, order = c(0,0,0)))
tsdiag(arima(time_series_unit, order = c(0,1,0)))
tsdiag(arima(time_series_unit, order = c(0,1,1)))

#Adding an AR term corrects for mild under-differencing, while adding an MA term corrects for mild overdifferencing.

plot(forecast(arima(time_series_unit, order = c(0,1,1)),h=7))
plot(forecast(arima(time_series_unit, order=c(0,1,1), seasonal=list(order=c(1,1,0), period=7)),h=7))
