###########################
#clear the environment
rm(list = ls())
###########################
library(readxl)
library(forecast)
library(tseries)
##########################
#######NordPoolCase#######
data <- read_excel("~/Business Forecasting/Old exams/Data/NordPoolCase (1).xlsx")
View(data)


#Determine the key variable of interest, i.e. the one you want to forecast
y<- ts(data$`System price`, frequency=7)
tsdisplay(y)
adf.test(y)
dy<-diff(y)
adf.test(dy)
plot(decompose(y, type = "multiplicative"))
plot(decompose(y, type = "additive"))


#What could be your explanatory variable?
buy<-ts(data$`DK1 Buy`, frequency=7)
plot(buy)
sell<-ts(data$`DK1 Sell`, frequency=7)
plot(sell)

x<- buy-sell
plot(x)
adf.test(x)
X<-x[2:305] #to match the length of the differenced y series


#What is the relationship between system price and buy/sell side?
#could consider the spread
lm1 <-lm(dy ~X) #in case we want to explain the relationship
summary(lm1)

library(Hmisc)
lm2<-lm(dy~Lag(X,-1)) #predictive regression would be more useful for forecasting
summary(lm2)

#or consider each of the variables separately (remember they are non-stationary)
lm3 <-lm(dy ~diff(buy)+diff(sell))
summary(lm3)


#Construct a model for predicting system price
#splitting the sample
insamp<-ts(y[1:274], frequency=7) #90% of the data
outsamp <-y[275:length(y)]

#ARIMA
f1<-auto.arima(insamp) 
summary(f1)
tsdisplay(residuals(f1))

fcast1<-forecast.Arima(f1, h = length(outsamp))
plot(fcast1)
lines(ts(y, frequency = 7))
accuracy(ts(fcast1,frequency=7), outsamp)

#Forecasting with a simple MA of order 7
k<-7
l<-length(insamp) #number of available data points
#adding extra rows, as many as periods ahead want to forecast
h <-length(outsamp)
MA <- insamp 
########################################
#generating space to save the forecasts
for (i in 1:h){
  MA <- c(y, 0)
}
#calculating the forecast values
for (j in (l+1):(l+h) ){
  a<-j-k
  b<-j-1
  Xm <-MA[a:b]
  MA[j] <- mean(Xm)
}
#########################################
accuracy(MA[275:305],outsamp)

# HoltWinters
fit2 <- HoltWinters(insamp)
fcast2 <- forecast(fit2, h=length(outsamp))
plot(fcast2)
lines(ts(y, frequency = 7))
accuracy(fcast2, outsamp)



#Comparing the 3 different forecasting methods
accuracy(fcast1, outsamp) #ARIMA
accuracy(MA[275:305],outsamp) #MA of order 7
accuracy(fcast2, outsamp) #HoltWinters

dm.test(residuals(fcast1), residuals(fcast2), h=length(outsamp))

# Considering a forecast combination
#Equal weights
combfit_eq<-0.5*fcast1$mean + 0.5*fcast2$mean
accuracy(combfit_eq, outsamp)

#Calculate the weights (GR method)
combfit <- lm(outsamp ~ fcast1$mean + fcast2$mean)
summary(combfit) #the coefficients in the regression will give you the weights
accuracy(combfit$fitted, outsamp)

combf <- ts(combfit$fitted, frequency = 7)
combfe<- ts(combfit_eq, frequency=7)
outsamp <-ts(y[275:length(y)], frequency = 7)
ts.plot(outsamp, combf, combfe, gpars=list(xlab="Date", lty=c(1:2), col=c("black", "green","blue")))



#Producing forecast for 25 days using a combination of ARIMA and HoltWinters
#Use the full data set and the ARIMA of choice to produce a forecast
fit1<-Arima(y, model=f1)#preserving the estimates obtained in previous step and using them on the remaining data
forecast1<-forecast(fit1,h=25)$mean

#Use the full data set and the HoltWinters model of choice to produce a forecast
fit2w<-HoltWinters(y, alpha=fit2$alpha,beta=fit2$beta,gamma=fit2$gamma)#preserving the estimates obtained in previous step and using them on the remaining data
forecast2<-forecast(fit2w,h=25)$mean

#Final forecast combination
combfcast <-combfit$coefficients[1]+combfit$coefficients[2]*forecast1+combfit$coefficients[3]*forecast2


#Consider the relationship between price and volume over time
library(ARDL)
model<-auto_ardl(dy ~X,data=cbind(X, dy), max_order = 7)
adl<-model$best_model #saves the best model
adl$order #shows the order of the ADL selected
summary(adl)

library(vars)
z <- ts(cbind(dy,X), frequency = 7)
#determine the order of VAR
VARselect(z, lag.max =14, type="const")[["selection"]]
var1 <- VAR(z, p=12, type="const") #estimate VAR(1)
summary(var1)
#group data for further analysis

#Can do model diagnostics as in AR models
#Checking for serial correlation in the errors
serial.test(var1, lags.pt=14, type="PT.asymptotic")
# The null is off no serial correlation

#Obtaining impulse response functions
plot(irf(var1,boot = TRUE, ci=0.95))






