library(forecast)
library(readxl)
################################################################################
# HW: Case1p413-414 (q1-3): Restaurant sales, weekly data
################################################################################
data <-read_excel("C:/Users//Case1p413-414.xls")
y<-ts(data$`Sales (all data)`)
#Old data
y_old<-y[1:104] #the covered time period is 1/4/81-12/26/82
#New data
y_new<- y[105:148] #1/2/83-10/30/83

#For the first two questions imagine, that the old data is the only one you have at hand. 
#Q 1#
#Use the old data and split it into in-sample and out-of-sample in order to proceed with estimation and evaluation
#I'll use roughly 20% of data for the test set
insamp <-y_old[1:84]
outsamp<- y_old[85:104]

#Plot the data and the correlograms
tsdisplay(insamp)
#ACF sort of dies out, cut off after lag 1 for PACF => 
# so could try an AR(1)

# Fitting an ARIMA model to the in-sample window 
fit <- auto.arima(insamp) #automatically select p,d and q for ARIMA
summary(fit) #Did get ARIMA(1,0,0) = AR(1) as predicted
tsdisplay(residuals(fit), main='Model Residuals') # Checking the model diagnostics

# Evaluating the performance based on the out-of-sample window
fcast <- forecast(fit, h=length(outsamp))
plot(fcast)
lines(y)
accuracy(outsamp,fcast$mean)

# Q2 #
f1<-Arima(y, model=fit)
summary(f1) #preserving the estimates obtained in previous step 
#and using them on the remaining data
# Obtaining forecast for first four weeks of January 1983
fcast1<-forecast(f1,h = 4)

# Q3 #
#Comparing it with the true value (from the new data set)
View(data.frame(fcast1,y_new[1:4]))

# Q5 #
# Now repeat the steps above, but change the in-sample to be the old data 
# and the out-of-sample - the new data
# You can also pick a smaller out-of-sample window and see how the results change.


###########################
#clear the environment
rm(list = ls())
###########################


################################################################################
# HW: Case4p417-419: The Lydia E.Pinkham Medicine Company
################################################################################
data <- read_excel("C:/Users/Case4p417-419.xls")
y<-ts(data$Sales)

tsdisplay(y, main='Data')

insamp<-y[1:42]
outsamp <-y[43:54]

tsdisplay(insamp)

#Replicating the statistical results, described in the case. 
#Is the optimal model selected an AR(2)?

fit1<-Arima(insamp, order=c(2,0,0), include.mean = TRUE)
summary(fit1)
tsdisplay(residuals(fit1))

f1<-Arima(y, model=fit1)
summary(f1) #preserving the estimates obtained in previous step and using them on the remaining data
fcast1<-f1$fitted[43:54]
View(fcast1)
accuracy(fcast1, outsamp)

# Q1 #
# Model forecast for 1961
forecast(f1, h=1) #the forecast is lower than the actual value.

# Q2 #
#Differencing the data
dy<-diff(y)
tsdisplay(dy)

#Estimating ARIMA(1,1,0)
fit2<-Arima(insamp, order=c(1,1,0))
summary(fit2)
tsdisplay(residuals(fit2))

f2<-Arima(y, model=fit2)
summary(f2) #preserving the estimates obtained in previous step and using them on the remaining data
#Comparing the forecasting equation for the ARIMA(1,1,0) model 
#with the forecasting equation for the AR(2) model, we see the two equations are very 
#similar and, consequently, would expect the one step ahead forecasts and forecast
#errors to be similar.

fcast2<-f2$fitted[43:54]
View(fcast2)
accuracy(fcast2, outsamp)
#The RMSE of model 1 and 2 are similar, therefore, the choice of one model 
#over the other depends upon whether one believes 
#the sales series in non-stationary or "nearly" non-stationary.  

