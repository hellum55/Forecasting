library(readxl)
library(tibble)
library(forecast)
library(tseries)
library(lmtest)
library(vars)
library(dLagM)
library(ARDL)
library(car)
#read data:
data <- read_excel("~/Business Forecasting/Old exams/Data/Re-Exam_Feb2023 (1).xlsx")
#Remove NA's
data <- na.omit(data)
sum(is.na(data))

#Question 1:
y <- ts(data$`Service satisfaction`, frequency = 4)
x <- ts(data$`Talk Duration (AVG)`, frequency = 4)
tsdisplay(y)
tsdisplay(x)
dy <- diff(y)
tsdisplay(dy)
adf.test(y)
adf.test(x)

#Question 2
mult <- decompose(y, type = "multiplicative")
add <- decompose(y, type = "additive")
plot(mult)
plot(add)

week <- seasonaldummy(y) #creates seasonal dummies
trend <- seq(1:length(y))

fit <- lm(y ~ week) #fits a linear model (OLS)
summary(fit)

#Question 3:
plot(y)
fit1 <- lm(y ~ log(trend) + week) #fits a linear model (OLS)
summary(fit1)

plot(fit1$fitted.values)
lines(fit1$fitted.values, col = "red")

plot(resid(fit1))
lines(resid(fit1), col = "red")

res<-fit1$residuals
jarque.bera.test(res) 
acf(res)
dwtest(fit1)

#Question 4:
fit2 <- auto.arima(y)
summary(fit2)

#Question 5:
T <- 182*0.75
insamp<-ts(y[1:T], frequency=4) #90% of the data
outsamp <-y[(T+1):length(y)]

tsdisplay(insamp)

fit3 <- auto.arima(insamp)
summary(fit3)
tsdisplay(residuals(fit3), main='Model Residuals')

fcast <- forecast(fit3, h=length(outsamp))
plot(fcast)
lines(ts(outsamp, start = c(34, 4), frequency = 4))
accuracy(fcast, outsamp)

fit4 <- Arima(y, model=fit3)
summary(fit4) #preserving the estimates obtained in previous step 
#and using them on the remaining data
# Obtaining forecast for first four weeks of January 1983
fcast1<-forecast(fit4,h = 4)
plot(fcast1)
fcast1

#Question 6:
dy <- diff(y)
dx <- diff(x)
tsdisplay(dy)
tsdisplay(dx)
adf.test(dy)
adf.test(dx)

# Combining the two vectors x and y 
z <- ts(cbind(x,y))

#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z) 

library(Hmisc)
fit5 <- lm(dy~dx+Lag(trend,-1)
summary(fit5)
adf.test(resid(fit5))
bptest(fit5)
acf(resid(fit5))







