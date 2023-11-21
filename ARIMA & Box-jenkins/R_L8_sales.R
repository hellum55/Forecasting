library(forecast)
library(readxl)
################################################################################
##### Sales data seasonal ######
################################################################################
data<- read_excel("C:/Users/Sales data.xlsx")
y<-ts(data$Sales, frequency = 12)

T<-0.85*length(y)# counts number of time period available in the data and then takes 85% to determine the size of the training set

#Generate a training and test subsamples
insamp<-y[1:T] 
outsamp<- y[120:length(y)]

tsdisplay(insamp)#ACF doesn't die out => non-stationary, we can also notice seasonality
#=> let's allow for ARIMA with seasonality
fit <- auto.arima(insamp, seasonal=TRUE) #automatically select p,d and q for ARIMA
#"seasonal=TRUE" allows R to pick seasonality if necessary
#If you want to enforce seasonal ARIMA, you can do ARIMA(p,d,q)×(P,D,Q)_12, where the second bracket reffers 
#fit<- Arima(insamp, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
#but then you have to decide yourself on p and q of ARIMA for both the main and the seasonal component

summary(fit) 
tsdisplay(residuals(fit),lag.max=30, main='Model Residuals') # Checking the model diagnostics

# Obtaining forecast for first four weeks of January 1983
#and evaluating the performance based on the out-of-sample window
fcast <- forecast(fit, h=length(outsamp))
plot(fcast)

View(data.frame(fcast,outsamp)) 
accuracy(fcast, outsamp)





