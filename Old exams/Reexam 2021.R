library(readxl)
library(tibble)
library(forecast)
library(tseries)
library(lmtest)
library(vars)
library(ARDL)
data <- read_excel("~/Business Forecasting/Old exams/Data/OliveOil.xlsx")
data <- na.omit(data)
sum(is.na(data))

#Question 1:
y <- ts(data$Price, frequency=52)
plot(y)
tsdisplay(y)
#Signs of non-stationarity. The lags of ACF do not die off quickly. A potential downward trend.
adf.test(y)
#The test says it is clearly non-stationary.
mult <- decompose(y, type = "multiplicative")
add <- decompose(y, type = "additive")
plot(mult)
plot(add)
#Clear downward trend together with clear seasonality. Might be in the winter periods that the sales are down
#No sign that the variance is decreasing or increasing. Additive might be the best in this case.
#The frequency should be set to 52, because it is weekly data.

#Question 2: ####
#create separate variables for each of the estimated components
month <- seasonaldummy(y) #creates seasonal dummies
trend <- seq(1:length(y))

## Fit a linear trend model
fit <- lm(y ~ trend+month)
fit1 <- lm(y ~ trend*month)
#if we want to extract the seasonal components can add dummies for months
summary(fit)
summary(fit1)
accuracy(fit$fitted.values,y)
accuracy(fit1$fitted.values,y)

plot(fit$fitted.values)
lines(fit$fitted.values, col = "red")
plot(fit1$fitted.values)
lines(fit1$fitted.values, col = "red")
#The multiplicative actually works better. it catches the uprise in the end, and the additive does not catch that

#Question 3: ####
T <- round(length(y)*0.8)
insamp <- ts(y[1:T], frequency = 52)
outsamp <- y[(T+1):length(y)]

#Question 4: ####
#Lets consider a forecast of ARIMA and HoltWinters
#We start with ARIMA:
fit2 <- auto.arima(insamp, seasonal=TRUE)
summary(fit2)
tsdisplay(residuals(fit2),lag.max=40, main='Model Residuals') # Checking the model diagnostics
#Seems quite good now.

fcast <- forecast(fit2, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast)
lines(y)
#plot data together with the original pseudo out-of-sample
f<-ts(fcast$mean, end=length(y), frequency = 52)
plot(f, col ='blue')
lines(outsamp)
accuracy(fcast$mean, outsamp)
#Overall a good test RMSE of 10.64

#HoltWinters
fit3 <- HoltWinters(insamp)
fcast1 <- forecast(fit3, h=length(outsamp))
plot(fcast1, col= "blue")
lines(y)
accuracy(fcast1$mean, outsamp)
#A bit worse than ARIMA. It predicts downward, but the reality is upward.

#Question 5: ####
#Lets see if the models are different from eachother:
dm.test(residuals(fcast), residuals(fcast1), h=length(outsamp))
#We can reject the H0. the models are different
dm.test(fcast$mean-outsamp,fcast1$mean-outsamp, h=length(outsamp))

#Lets combain the two:
# Granger-Ramanathan combination method (GR)
combfit <- lm(outsamp ~ fcast$mean + fcast1$mean)
summary(combfit) #the coefficients in the regression will give you the weights
combfcast <- ts(combfit$fitted.values, frequency = 52)
accuracy(combfcast, outsamp)

#plotting the initial data together with the forecast
yf2<-ts(append(insamp,combfcast), frequency = 52)
ts.plot(y, yf2, gpars=list(xlab="Year", lty=c(1:2)))
#It is better though

#Question 6: ####
fit4<-Arima(y, model=fit2)#preserving the estimates obtained in previous step and using them on the remaining data
forecast3<-forecast(fit4,h=52)$mean
#Use the full data set and the HoltWinters model of choice to produce a forecast
fit5<-HoltWinters(y, alpha=fit3$alpha,beta=fit3$beta,gamma=fit3$gamma)#preserving the estimates obtained in previous step and using them on the remaining data
fcast2<-forecast(fit5,h=52)$mean

#Final forecast combination
combfcast <-combfit$coefficients[1]+combfit$coefficients[2]*forecast3+combfit$coefficients[3]*fcast2
plot(combfcast)
combfcast

####################### CASE 2 #############################
data <- read_excel("~/Business Forecasting/Old exams/Data/HouseInc.xlsx")
data <- na.omit(data)
sum(is.na(data))

#Question 1: ####
y <- ts(data$REPI, start = 1975)
x <- ts(data$Income, start = 1975)
plot(y)
plot(x)
tsdisplay(y)
tsdisplay(x)
#First we check for stationairity.From the ACF it looks like there is a clear trend because is does not die
#off quickly. This is for both variables
#Lets check with a test:
adf.test(y, k=trunc((length(y)-2)^(1/3)))
adf.test(x, k=trunc((length(x)-2)^(1/3)))
#Both non-stationary. We might have to take the difference between the lags
#We cannot reject the null-hypo so lets check the differencs of y and x
dy <- diff(y, differences = 2)
dx <- diff(x, differences = 2)
adf.test(dy)
adf.test(dx)
#Lets see the components:
#No Seasonality just a clear trend
#We will check for co-integration:
# Combining the two vectors x and y 
z <- ts(cbind(y,x))
#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z) 
#There is no co-integration between the variables. So no long term relationship

#Question 4####
fit <- lm(dy~dx)
summary(fit)

accuracy(fit$fitted.values,dy)
plot(fit$fitted.values)
lines(fit$fitted.values, col = "red")
#They do not look constant. The variance increases with time:
acf(resid(fit))
#Looks rather perfect when looking at the ACF. There is no significant spikes.
bptest(fit) 
#The Brausch-Pagan tells that the variance is not constant. We can reject H0
#The OLS does not meet the assumptions.
dwtest(fit)
#Do not have any autocorrelation problems.
jarque.bera.test(fit$residuals) #Check if residuals show normality
#Again we reject the H0 - signs a non normality.
#All looks quite good.

#We need future values of X prices so we need to forecast these with ARIMA
ARIMA <- auto.arima(x)
ARIMA
fcast2 <- forecast(ARIMA, h=6)
fcast <- fcast2$mean

dfcast <- diff(fcast)
d<-fit$coefficients[1]+fit$coefficients[2]*dfcast
forecast1<-y[45]+cumsum(d)
plot(forecast1)

benchmark <- Arima(dx, order = c(1, 0, 0))
fcast2 <- forecast(benchmark, h=5)
fcast_y<-y[45]+cumsum(fcast2$)
fcast_y
