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
x <- ts(data$`Waiting Time (AVG)`, frequency = 4)
tsdisplay(y)
tsdisplay(x)
adf.test(y)
adf.test(x)

dy <- diff(y)
dx <- diff(x)
tsdisplay(dy)
tsdisplay(dx)
adf.test(dy)
adf.test(dx)
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
hist(x)
fit1 <- lm((y) ~ (-1/(trend)+week)) #fits a linear model (OLS)
summary(fit1)

plot((y), type="l")
lines(fit1$fitted.values, col="red")

plot(resid(fit1))
lines(resid(fit1), col = "red")
accuracy(ts(fit1$fitted.values, frequency = 4),y)

res<-fit1$residuals
jarque.bera.test(res) #Null = data is normally distributed
acf(res)
dwtest(fit1)

multi <- decompose(y, type = "multiplicative")
plot(multi)
S<-multi$seasonal

SI <- scale(S[1:4]) #the indicies are the same for repeated season, so only need 12 of them as have 12 different months
plot(SI, type="h") 

CMA <- ma(y, order=52, centre=TRUE) #desesonalized data
#seasonal facto
SF<-y/CMA
plot(SF, type="l")
abline(h=1, col = "red")

linmod <- lm(CMA ~ trend, na.action = "na.exclude")
CMAT <- linmod$fitted.values
#cycle factor
Cycle <- na.exclude(CMA) / CMAT
ts.plot(Cycle)
abline(h=1, col = "red") 

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
insamp_y<-ts(y[1:137], frequency=4) #90% of the data
insamp_x<-ts(x[1:137], frequency = 4)
outsamp_y <-y[(T+1):length(y)]

dy <- diff(insamp_y)
dx <- diff(insamp_x)
tsdisplay(dy)
tsdisplay(dx)
adf.test(dy)
adf.test(dx)

# Combining the two vectors x and y 
z <- ts(cbind(insamp_x,insamp_y))

#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z) 

library(Hmisc)
fit5 <- lm(insamp_y~insamp_x)
summary(fit5)
adf.test(resid(fit5))
bptest(fit5)
acf(resid(fit5))

fit6 <- lm(insamp_y~insamp_x+log(trend)[1:137])
summary(fit6)
acf(resid(fit6))
adf.test(resid(fit6))

pres1<-auto.arima(insamp_x)
#pres1<-Arima(pres, order=c(2,1,1), seasonal=c(0,0,2))
pres2<-forecast(pres1,h=length(outsamp_y))
presf<-pres2$mean

fcast4<-fit6$coefficients[1]+fit6$coefficients[2]*presf
accuracy(fcast4, outsamp_y)

#Question 8:
#group data for further analysis
dy <- diff(y)
dx <- diff(x)
z <- ts(cbind(dy,dx), frequency = 4)
#dz<- diff(z)

#determine the order of VAR
VARselect(z, lag.max = 8, type="const")[["selection"]] #optimal p =1
var1 <- VAR(z, p=4, type="const") #estimate VAR(1)
summary(var1)

#Can do model diagnostics as in AR models
#Plot the autocorrelation functions for the VAR residuals
acf(residuals(var1))
#Checking for serial correlation in the errors
serial.test(var1, lags.pt=8, type="PT.asymptotic")

#Checking stability of VAR
var1.stable <- stability(var1, type = "OLS-CUSUM")
plot(var1.stable)
roots(var1)

#Obtaining impulse response functions
plot(irf(var1,boot = TRUE, ci=0.95))

#split the sample into training and test
insampdz <- ts(z[1:136, 1:2], frequency = 4)
outsampdz <- ts(z[137:181, 1:2], frequency = 4)

#select the order of VAR
VARselect(insampdz, lag.max = 8, type="const")[["selection"]]

#Fit the model on training data
fit <- VAR(insampdz, p=4, type="const")
summary(fit)
#check the residuals of the model
serial.test(fit, lags.pt=4, type="PT.asymptotic")

#Obtain the forecast for the test period
length(outsampdz)
fcast3 <- forecast(fit, h=45)
plot(fcast3) #provides a plot of the forecasts

#calculate the accuracy of the forecast for the ltn variable
accuracy(fcast3$forecast$dy, outsampdz[1:45, 1])

#Aggregate data to levels
fcast_y<-y[137]+cumsum(fcast3$forecast$dy$mean)
library(ggplot2)
Date <- seq(1:182)
ggplot(data, aes(x=Date)) +
  geom_line(aes(y = append(y[1:137],fcast_y)), color = "red")+    
  geom_line(aes(y = y)) +
  scale_y_continuous(name = "")

#Question 9:
accuracy(fcast3$forecast$dy, outsampdz[1:45, 1])
accuracy(fcast4, outsamp_y)
accuracy(fcast, outsamp)

z1 <- ts(cbind(x,y), frequency = 4)
dz1 <- (z)
fit4 <- VAR(z, p=4, type="const")
fcast8 <- forecast(fit4, h=4)
plot(fcast8) #provides a plot of the forecasts

fcast_full <- y[182]+cumsum(fcast8$forecast$dy$mean)
Y<-ts(append(y, fcast_full), frequency = 4)
# Now create the plot with the actual data and the forecast appended
plot(y, main="Actual Data and Forecast", xlab="Time", ylab="Values", col="blue", xlim=c(start(y)[1], end(Y)[1]))
lines(Y, col="red", lty=2)
Y_subset <- window(Y, start=182, end = 186)
lines(Y_subset, col="red", lty=2)
legend("topleft", legend=c("Actual Data", "Forecast"), col=c("blue", "red"), lty=1:2, bty="n")




fit4 <- Arima(y, model=fit3)
summary(fit4) #preserving the estimates obtained in previous step 
#and using them on the remaining data
# Obtaining forecast for first four weeks of January 1983
fcast1<-forecast(fit4,h = 4)
plot(fcast1)


