library(forecast)
library(readxl)
################################################################################
##### Sales data seasonal ######
################################################################################
data<- read_excel("C:/Users/au672494/OneDrive - Aarhus Universitet/Teaching/Courses/Business Forecasting/Business Forecasting 2023/Week 45/Sales data.xlsx")
y<-ts(data$Sales,start = 1)

T<-floor(0.85*length(y))# counts number of time period available in the data 
#and then takes 85% to determine the size of the training set. The integer number.


#Generate a training and test sub-samples
insamp<-y[1:T]
outsamp<- y[T+1:length(y)]

tsdisplay(insamp)#ACF doesn't die out => non-stationary, we can also notice seasonality
#=> let's allow for ARIMA with seasonality
fit0 <- auto.arima(insamp, seasonal=TRUE) #automatically select p,d and q for ARIMA
summary(fit0) #pick's a non-seasonal model ARIMA(2,1,2) with drift 
#as we have not ts() the insamp data, so R doesn't know the frequency :/


##In order for auto.arima to consider the seasonal component, need to make sure 
##the data is ts-ed, which is insamp and outsamp in this case

#Generate a training and test sub-samples and ts() them
insamp<-ts(y[1:T],end=T, frequency=12)
outsamp<- ts(na.omit(y[T+1:length(y)]),end=length(y),frequency=12)
#make sure to specify start and/or end for each of the sub-samples. Otherwise plotting will not work later on.

tsdisplay(insamp)#ACF doesn't die out => non-stationary, we can also notice seasonality
#=> let's allow for ARIMA with seasonality
fit <- auto.arima(insamp, seasonal=TRUE) #automatically select p,d and q for ARIMA
#"seasonal=TRUE" allows R to pick seasonality if necessary

summary(fit) 
tsdisplay(residuals(fit),lag.max=30, main='Model Residuals') # Checking the model diagnostics

#If you want to enforce seasonal ARIMA, you can do ARIMA(p,d,q)(P,D,Q)_12, where the second bracket reffers 
fit2<- Arima(insamp, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
#but then you have to decide yourself on p and q of ARIMA for both the main and the seasonal component

summary(fit2) 
tsdisplay(residuals(fit2),lag.max=30, main='Model Residuals') # Checking the model diagnostics

# Obtaining forecast for first four weeks of January 1983
#and evaluating the performance based on the out-of-sample window
#for auto.arima chosen model
fcast1 <- forecast(fit, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast1)

#plot data together with the original pseudo out-of-sample
f1<-ts(fcast1$mean, end=length(y), frequency = 12)
plot(f1, col ='blue')
lines(outsamp)

#estimate the out-of-sample accuracy of the forecast
accuracy(fcast1$mean, outsamp)
View(data.frame(fcast1$mean,outsamp)) 


#Can repeat the same for the arima with fixed parameters choice
fcast2 <- forecast(fit2, h=length(outsamp))

accuracy(fcast2$mean, outsamp)








