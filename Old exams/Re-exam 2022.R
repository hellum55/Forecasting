library(readxl)
library(tibble)
library(forecast)
library(tseries)
library(lmtest)
library(vars)
library(ARDL)
data <- read_excel("~/Business Forecasting/Old exams/Data/Reexam_Feb2022_Data.xlsx")
data <- na.omit(data)
sum(is.na(data))
#Question 1: ####
#Are the data stationary/non stationary?
y <- ts(data$PCE, frequency=12)
x <- ts(data$ICS, frequency=12)
z <- ts(data$TB3MS, frequency = 12)
tsdisplay(y)
tsdisplay(x)
tsdisplay(z)
#A clear upward trend for the consumption variable, with a little dip in 2008 (ofcourse). 
#The treasury bond also has a downward spike maybe with some cyclical movements because the data
#is over such a long time. All ACF's shows that the variables might be non-stationary.
#Lets test it with a statistical tool:
adf.test(y)
adf.test(x)
adf.test(z)
#All tests shows that the variables are non-stationary, so we have to difference the variables and hope they
#become stationary.
z_1 <- ts(cbind(x, y, z))
#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z_1)
#there is no co-integration to be seen when performing on all variables. We the need to difference the variables
dy <- diff(y)
dx <- diff(x)
dz <- diff(z)

adf.test(dy)
adf.test(dx)
adf.test(dz)
#We can reject the H0 for all variables meaning they are all stationary now.
tsdisplay(dy)
tsdisplay(dx)
tsdisplay(dz)
#All the ACF's look kinda nice now.
cor(y, x)
cor(y, z)
#The correlation between y and x are fairly small. maybe the x variable are not that good to predict
#the consumption after all.

#Question 3: ####
fit <- lm(dy~dx)
summary(fit)
#When performing the lm on both variables only the personal income have a siginificant influence when
#predicting consumption. 
#Lets plot the residuals:
plot(resid(fit))
#Not quite staple. We see that in the start that is around 0, but in the end we see a couple of extremes.
#Lets test it:
acf(resid(fit))
#The ACF looks quite good. only a couple of spikes.
bptest(fit) 
#The BP tells us that the residuals are not constant which we also could see from the plot
dwtest(fit)
#Do not have any autocorrelation problems, as we could see from the ACF as well.
jarque.bera.test(fit$residuals) #Check if residuals show normality
#Again we reject the H0 - signs of non normality. 
#All in all the lm does not really meet the requirements for OLS. we can not trust the LM results.
#lets forecast with the model 1-year ahead:
#We need future values of Oil prices so we need to forecast these with ARIMA
ARIMA <- auto.arima(x)
ARIMA
fcast <- forecast(ARIMA, h=13)
fcast1 <- fcast$mean

dfcast <- diff(fcast1)
d <- fit$coefficients[1]+fit$coefficients[2]*dfcast
forecast1 <- y[526]+cumsum(d)
plot(forecast1)
plot(y, col="red")
lines(ts(forecast1, start = c(44,11), frequency = 12))
#Now we have forecasted values for personal income

#Question 4: ####
#Estimate an ADL
fit1<-auto_ardl(dy ~ dx+dz, data=cbind(dx, dy, dz), max_order = 12)
fit2<-fit1$best_model #saves the best model
fit2$order #shows the order of the ADL selected
summary(fit2)
#The ADL model chooses a model with 12 lags back for both the consumption and personal income. But only one lag
#for the 3 year bond. later lacks do not have a impact. of course not it is todays lack that decide the consumption.
#We see that almost every lag of the consumption has a impact of the consumption today.
#makes great sense. if the consumption is higher one period back the consumption might be high again
#the next period. But the a less significant impact on 
#It is better than just a regular LM.
acf(resid(fit2))
#Checking for serial correlation in the errors
#Lets try to forecast:
d1 <- fit2$coefficients[1]+fit2$coefficients[2]*dfcast
forecast2 <- y[526]+cumsum(d1)
plot(forecast2)
plot(y, col="red")
lines(ts(forecast2, start = c(44,11), frequency = 12))
#Question 5: ####
# # # # # VAR: Estimation # # # # #
z <- ts(cbind(y,x,z), frequency = 12)
# Taking the first difference to make the series stationary for VAR analysis.
dz<- diff(z)
#determine the order of VAR
VARselect(dz, lag.max = 12, type="const")[["selection"]] #optimal p =1
var1 <- VAR(dz, p=2, type="const") #estimate VAR(1)
summary(var1)

#Can do model diagnostics as in AR models
#Plot the autocorrelation functions for the VAR residuals
acf(residuals(var1))
#Checking for serial correlation in the errors
serial.test(var1, lags.pt=2, type="PT.asymptotic")
### The null is off no serial correlation

#Checking stability of VAR
var1.stable <- stability(var1, type = "OLS-CUSUM")
plot(var1.stable)
roots(var1)

#Obtaining impulse response functions
plot(irf(var1,boot = TRUE, ci=0.95))
#We can not really say much abaout the impulses. We can see that income have an influence on consumption
#but the confidence interval is around the 0 point which means that there is a possibilty that is has
#zero effect.

# # # # # VAR: Forecasting Exercise # # # # # 
#split the sample into training and test
insampdz <- ts(dz[1:446, 1:3], frequency = 12)
outsampdz <- ts(dz[447:525, 1:3], frequency = 12)

#select the order of VAR
VARselect(insampdz, lag.max = 12, type="const")[["selection"]]

#Fit the model on training data
fit3 <- VAR(insampdz, p=2, type="const")
summary(fit3)
#check the residuals of the model
serial.test(fit3, lags.pt=2, type="PT.asymptotic")
length(outsampdz)
#Obtain the forecast for the test period
fcast3 <- forecast(fit3, h=79)
plot(fcast3) #provides a plot of the forecasts

#calculate the accuracy of the forecast for the ltn variable
accuracy(fcast3$forecast$y, outsampdz[1:79, 1])

#Aggregate data to levels
fcast_y<-y[447]+cumsum(fcast3$forecast$y$mean)
library(ggplot2)
Date <- seq(1:length(y))
ggplot(data, aes(x=Date)) +
  geom_line(aes(y = append(y[1:447],fcast_y)), color = "red")+    
  geom_line(aes(y = y)) +
  scale_y_continuous(name = "")

#Not a really good test RMSE. underestimate the whole period.

#Question 6:
y <- ts(data$PCE, frequency = 12)
plot(y)
mult <- plot(decompose(y, type = "multiplicative"))
add <- (decompose(y, type = "additive"))
#It looks like the additive is best because the random component are more stationary. there is also no sign of
#increasing variance based in the time. there is definitely a trend.
#create separate variables for each of the estimated components
Tr<-add$trend
S<-add$seasonal
I<-add$random

yhat<-Tr+S+month+1 #use the components to reverse the multiplicative decomposition and obtain a forecast
plot(y, type="l")
lines(Tr,col="green")
lines(yhat,col="red")
accuracy(yhat,y) 

yf<-forecast(yhat, h = 12) #calculate forecasts 12 periods ahead, based on the carried out decomposition method
#given that estimated components are lagging behind, the end forecast is produced 12 periods ahead from the last estimated component time point, and not the end of the data. 
summary(yf)

#Question 7: ####
trend <- seq(1:length(y))
fit2 <- lm(data$PCE ~ 0+trend+month) #fits a linear model (OLS)
summary(fit2)
#A clear trend of course are significant. when taking sesonality dummies into acount it has no significance
#We can just delete the month
plot.ts(y, type="l")
lines(yhat2, col="green")
lines(yhat, col="red")

yhat2 <-ts(fit2$fitted.values, frequency =12)
accuracy(fit2$fitted.values,y)
#A great straight line because of the steady trend in the beginning. but it can cot capture the fluctations in the end
#but it is captured by the decomp regression. The fit is much better from the decomp

#Obtaining the cycle series based on the slides
trend <- seq(1:length(z))
CMA <- ma(z, order=12, centre=TRUE)
linmod <- lm(CMA ~ trend, na.action = "na.exclude")
CMAT <- linmod$fitted.values
Cycle <- na.exclude(CMA) / CMAT
ts.plot(Cycle)
abline(h=1, col = "red")
cycle <- CMA/CMAT

#Question 8: ####
y <- ts(data$PCE, frequency=12)
insampdz <- ts(y[1:446], frequency = 12)
outsampdz <- ts(y[447:526])

#HoltWinter
fit4 <- HoltWinters(insampdz)
fit4
#Very much weight on the trend from the past lag. not so much on the seasonal part.
fcast4 <- forecast(fit4, h=length(outsampdz))
plot(fcast4)
lines(y)
accuracy(fcast4, outsampdz) 
#Worse than decomp.
#lets forecast 1-year ahead
fit5 <- HoltWinters(y)
fcast5 <- forecast(fit5, h=12)
plot(fcast5)
lines(y)
#Still a upward trend

#Question 9:
fit5 <- auto.arima(insampdz) #automatically select p,d and q for ARIMA
#"seasonal=TRUE" allows R to pick seasonality if necessary
summary(fit5) 
tsdisplay(residuals(fit5),lag.max=30, main='Model Residuals') # Checking the model diagnostics
#Looks fine
fcast1 <- forecast(fit5, h=length(outsampdz))
#plot the forecast together with confidence intervals
plot(fcast1)
lines(y)
accuracy(fcast1, outsampdz)
#Worse than HoltWinters - underestimates again.
fit7<- Arima(insampdz, order=c(1,1,1), seasonal=list(order=c(1,1,0),period=12))
fcast9 <- forecast(fit7, h=length(outsampdz))
#plot the forecast together with confidence intervals
plot(fcast9)
lines(y)
accuracy(fcast9, outsampdz)
#It is better with a simpler model.

fit6 <- Arima(y, order=c(1,1,1), seasonal=list(order=c(1,1,1),period=12))
summary(fit6)
tsdisplay(residuals(fit6),lag.max=30, main='Model Residuals') # Checking the model diagnostics
fcast8 <- forecast(fit6, h=12)
plot(fcast8)
