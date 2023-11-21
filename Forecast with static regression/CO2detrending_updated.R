library(readxl)
library(tibble)
library(forecast)
CO2levels <- read_excel("C:/Users//CO2levels.xlsx")
View(CO2levels)

y <- ts(CO2levels) #specifying variable as a time series
trend <- seq(1:length(y)) #Creating the linear trend, i.e. time period counter
plot(y)

fit <- lm(y ~ trend) #estimates an OLS regression of y on a constant and trend.
summary(fit) #demonstrate results of the OLS estimation
yhat <-fit$fitted.values #obtain fitted values (in-sample prediction)
accuracy(fit$fitted.values,y) #obtain RMSE and other measure of accuracy comparing y and yhat

plot(y, type="l")
lines(yhat, col="red")

ystar <- resid(fit) #ystar is the detrended series, which is simply the residuals from the previous regression.

plot(ystar)
lines(ystar, col="red")


acf(y, 50) #can observe both trend and seasonality in the data by high values of ACF and a repeated pattern
#We can specify how many lags we want to plot. Here I just chose 50. 
acf(ystar, 50) #can see that the trend has been taken care of, but the seasonal pattern remains

#Differencing as an alternative way of detrending:
dy <- diff(y) #generate a new variable that is first differences, Y_t-Y_t-1
plot(dy)

acf(dy, 50) # similar to acf(ystar), i.e. have successfully detrended the data



#Accounting for seasonality
y <- ts(CO2levels, frequency=12) #specify monthly frequency for the data
month <- seasonaldummy(y) #generate a set of dummies representing months 


fit2 <- lm(y ~trend+month)  #estimates an OLS regression of y on a constant and trend and 11 dummies (months)
summary(fit2)
yhat2 <-ts(fit2$fitted.values, frequency =12)
accuracy(fit2$fitted.values,y)

plot.ts(y, type="l")
lines(yhat2, col="red")

ystar2 <- fit2$residuals #ystar is the detrended and deseasonalised series, which is simply the residuals from the previous regression.


#compare the residuals from two models, i.e. comparing detrended data with detrended and deseasonalised data.
plot(ystar)
lines(ystar2, col="red")

acf(ystar2, 50)
