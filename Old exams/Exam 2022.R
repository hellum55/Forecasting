library(readxl)
library(tibble)
library(forecast)
data <- read_excel("~/Business Forecasting/Old exams/Data/Exam_Jan2022_Data.xlsx")

#Question1 ####
y <- ts(data$`Passenger arrival`,start = 1, end = c(86, 4), frequency = 7)
#Chech the properties:
tsdisplay(y)

#Check for stationairty:
library(tseries)
adf.test(y)

#Question 2 ####
# Additive Decomposition with trend and seasonal components (no cyclical)
decomp_y <- decompose(y, type = "additive") #for multiplicative decomposition
plot(decomp_y)

#Question 3####
trend <- seq(1:length(y))
dummy <- seasonaldummy(y)#Creating the linear trend, i.e. time period counter
plot(y)

fit <- lm(data$`Passenger arrival` ~ 0+trend+dummy) #estimates an OLS regression of y on a constant and trend.
summary(fit) #demonstrate results of the OLS estimation
yhat2 <-ts(fit$fitted.values, frequency =7)
accuracy(fit2fitted.values,y)

#create separate variables for each of the estimated components
Tr<-decomp_y$trend
S<-decomp_y$seasonal
I<-decomp_y$random

# Generate the fitted values (in-sample fit) using the decomposition
decomp_lm<-Tr+S+1

#compare the residuals from two models, i.e. comparing detrended data with detrended and deseasonalised data.
acf(resid(fit))

#Question 4 ####
plot.ts(y, type="l")
lines(yhat2, col="green")
lines(decomp_lm, col="red")


accuracy(decomp_lm,y) 
accuracy(fit$fitted.values,y)

#Question 5 ####
T<-floor(0.85*length(y))
#Generate a training and test sub-samples
insamp<-ts(y[1:509], start = 1, end = c(73, 5), frequency=7)
outsamp<- ts(y[510:599], start = c(73,6), end = c(86,4),frequency=7)
tsdisplay(insamp)

fit <- auto.arima(insamp, seasonal=TRUE)
summary(fit)
tsdisplay(residuals(fit),lag.max=30, main='Model Residuals')

fcast1 <- forecast(fit, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast1)
lines(ts(y[510:599], start = c(73,6), frequency = 7))
#plot data together with the original pseudo out-of-sample
f1<-ts(fcast1$mean, end=length(y), frequency = 7)
plot(f1, col ='blue')
lines(outsamp)

accuracy(fcast1$mean, outsamp)

#Lets try another model:

fit2<-Arima(insamp, order=c(0,1,0))
summary(fit2)
tsdisplay(residuals(fit),lag.max=30, main='Model Residuals')

fcast2 <- forecast(fit2, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast2)
lines(ts(y[510:599], start = c(73,6), frequency = 7))
#plot data together with the original pseudo out-of-sample
f1<-ts(fcast2$mean, end=length(y), frequency = 7)
plot(f1, col ='blue')
lines(outsamp)

accuracy(fcast2$mean, outsamp)

#Question 6 ####
# triple exponential - models level, trend, and seasonal components (all parameters will be automatically estimated)
fit3 <- HoltWinters(insamp, seasonal = "additive")
fit3$alpha
fit3$beta
fit3$gamma

fcast3 <- forecast(fit3, h=length(outsamp))
plot(fcast3)
lines(ts(y[510:599], start = c(73,6), frequency = 7))
accuracy(fcast3, outsamp)

#Question 8: ####
y2 <- ts(data$`Stringency Index`,start = 1, end = c(86, 4), frequency = 7)
#Chech the properties:
tsdisplay(y2)
adf.test(y2)

Y <- data$`Passenger arrival`
X <- data$`Stringency Index`

scatter.smooth(x=X, y=Y, main="Y ~ X") 
linMod <- lm(Y ~ X) #To omit intercept when necessary, the formula can be written for example as lm(cost ~ age - 1)
summary(linMod)

library(lmtest)
bptest(linMod) # Breusch-Pagan test H_0: variance is constant.

# Checking the residual diagnostic
plot(resid(linMod))
acf(resid(linMod))

#Question 9 ####
library(ARDL)
# We will use the "vars" package for VAR analysis: install.packages("vars")
library(vars)
x<-ts(data$`Stringency Index`)
y<-ts(data$`Passenger arrival`)

#Estimate an ADL
model<-auto_ardl(y ~x,data=cbind(x, y), max_order = 10)
fit<-model$best_model #saves the best model
fit$order #shows the order of the ADL selected
summary(fit)
