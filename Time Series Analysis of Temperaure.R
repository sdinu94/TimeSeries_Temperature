#Resources used
#http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
#https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/

getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- '/Users/Vijitha/Downloads'
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

#Read the timeseries file from rows January 1781 (to get rid of yester year missing values)
#Modify file to reflect above before reading
data <- read.csv("Time Series.csv", header = TRUE)

#first 2 columns for analysis
chic <- data[c("dt", "AverageTemperature")] 

#frequency set to 12 since monthly data, labeling rows from January 1781 onwards
#read as time series data
chictimeseries <- ts(chic[,2], frequency=12, start=c(1781,1))
dev.off()
plot.ts(chictimeseries)
#Century wise visualization
plot.ts(chictimeseries,xlim=c(1781,1800))
plot.ts(chictimeseries,xlim=c(1801,1900))
plot.ts(chictimeseries,xlim=c(1901,2000))
plot.ts(chictimeseries,xlim=c(2001,2013))

cycle(chictimeseries)
#mean temperature increases year on year
plot(aggregate(chictimeseries,FUN = mean))
#across months seasonal effect. more fluctuation in dec, jan and less in july.
boxplot(chictimeseries~cycle(chictimeseries))

#Stationary series test? Augmented Dickey-Fuller test
adf.test(chictimeseries,alternative="stationary")
adf.test(diff(chictimeseries), alternative="stationary")
#stationary to do time series modelling without difference

acf(chictimeseries, lag.max=25)             # plot a correlogram
acf(chictimeseries, lag.max=25, plot=FALSE)
#dies off after lag 2. AR model of order p=1
#look at significance levels
pacf(chictimeseries, lag.max=25)             # plot a correlogram
pacf(chictimeseries, lag.max=25, plot=FALSE)
#look at significance levels
#dies off after about 5 to 6 lags

#Trying the simple moving average decomposition for non-seasonal data. 
#Applicable for non-seasonal model (trend + irregualar components)
#Our data is clearly seasonal?
library("TTR")
chictimeseriesSMA <- SMA(chictimeseries,n=12)
#try multiples of 12 for n
# We are trying to smooth the time series
plot(chictimeseriesSMA)
#still a lot of random fluctuations while using an order of 12.  We are expecting a somewhat linear trend
#confirms that our data is seasonal?

#decomposing seasonal data (trend + seasonal + irregular components)
chicseriescomps <- stats::decompose(chictimeseries)

chicseriescomps$seasonal
# Highest is for August and lowest is for January
#The estimated seasonal factors are given for the months January-December, and are the same for each year. 

plot(chicseriescomps)
#talk about the trend plot, how has it changed over time? coldest year yet? hottest year yet?
#rising trend? global warming?

#without seasonal component
chicseriescompsadj <- chictimeseries - chicseriescomps$seasonal
#trend and irregular component without the seasonal adjustment
plot(chicseriescompsadj)

#compare plots
par(mfrow=c(2, 1))
plot(chictimeseries)
title("original data")
plot(chicseriescompsadj)
title("without the seasonal component")
dev.off()

#simple exponential smoothing - to make short term forecasts
#can only be performed on non-seasonal additive model
chicseriesforecasts <- HoltWinters(chicseriescompsadj,beta=FALSE,gamma=FALSE)
chicseriesforecasts
# alpha that are close to 0 mean that little weight is placed on the most recent observations when making forecasts of future values
plot(chicseriesforecasts) #red line is the forecast.  It is smoother than actual.
chicseriesforecasts$SSE

library("forecast")
chicseriesforecasts2 <- forecast.HoltWinters(chicseriesforecasts, h=12)
chicseriesforecasts2
plot.forecast(chicseriesforecasts2)
#darker blue/gray is the 80% prediction interval and the ligher shade is the 95% interval

#limiting x to see it more clearly
plot.forecast(chicseriesforecasts2,xlim=c(1980,2015))

#accuracy of this predictive mode
chicseriesforecasts$SSE

#How are residuals distributed?
plot.ts(chicseriesforecasts2$residuals)

acf(chicseriesforecasts2$residuals[-1],lag.max=20)


#predict(chicseriesforecasts,n.ahead=3)
#predict(chicseriesforecasts,n.ahead=12)
#dev.off()
#plot(chictimeseries,xlim=c(1980,2020))
#lines(predict(chicseriesforecasts,n.ahead=48),col=2)

#ARIMA - taking correlations into account unlike the Holt method
#Stationarytimeseries?
plot(chictimeseries,xlim=c(1980,2000))
#with diff of 1,2,3,etc?
chicseriesdiff <- diff(chictimeseries, differences=1)
#try 1,2,3
plot.ts(chicseriesdiff,xlim=c(1980,2000))
#Looks like it is already a stationary series
acf(chicseriesdiff, lag.max=12)             # plot a correlogram
acf(chicseriesdiff, lag.max=12, plot=FALSE)
#look at significance levels
pacf(chicseriesdiff, lag.max=20)             # plot a correlogram
pacf(chicseriesdiff, lag.max=20, plot=FALSE)
#look at significance levels

#fit an arima model to do timeseries forecasting
#ARIMA(p,0,q)
result <-auto.arima(chictimeseries,stationary=TRUE,seasonal=TRUE)
#suggests 2 models
#ARIMA(1,0,5) - AR[1] model + MA[5]
#ARIMA(0,0,2) - MA[2] model 

acf(chictimeseries, lag.max=25)             # plot a correlogram
acf(chictimeseries, lag.max=25, plot=FALSE)
#look at significance levels
pacf(chictimeseries, lag.max=25)             # plot a correlogram
pacf(chictimeseries, lag.max=25, plot=FALSE)
#look at significance levels

library("forecast")
#Forecast next 5 months temperature
chictimeseriesARIMA <-arima(chictimeseries,order=c(1,0,5))
forecast.Arima(chictimeseriesARIMA, h=5, level=c(99.5))
#plot of residuals
plot(chictimeseriesARIMA$residuals)
#acf of residuals
acf(chictimeseriesARIMA$residuals[-1],lag.max=20)

#Take the one with lowest AIC/BIC

#Summary statistics of original data
quantile(chic[,2])

mean(chic[,2])

sd(chic[,2])

var(chic[,2])

library("moments")
skewness(chic[,2])

