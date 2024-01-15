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
data <- read_excel("~/Business Forecasting/Old exams/Data/Exam_Jan2023.xlsx")
#Remove NA's
data <- na.omit(data)
sum(is.na(data))

#Question 1:
freq <- 7
y <- ts(data$total_revenue, frequency=freq)
x <- ts(data$n_screenings, frequency=freq)
tsdisplay(y)
tsdisplay(x)

adf.test(y)
adf.test(x)

#Question 2:
mult <- decompose(y, type = "multiplicative")
add <- decompose(y, type = "additive")
plot(mult)
plot(add)
#Check the random variable. Which one is centered the most?
#Extract the components:
Tr_m<-mult$trend
S_m<-mult$seasonal
I_m<-mult$random
#Additive                 
Tr_a<-add$trend
S_a<-add$seasonal
I_a<-add$random
#Check which of the decompositions is the best:
# Generate the fitted values (in-sample fit) using the decomposition
yhat_m<-Tr_m*S_m*1 #use the components to reverse the multiplicative decomposition and obtain a forecast
plot(y, type="l")
lines(Tr_m,col="green")
lines(yhat_m,col="red")
accuracy(yhat_m,y) 

yf<-forecast(yhat_m, h = l)
plot(yf)                 
#The additive:
yhat_a<-Tr_a+S_a+1 #use the components to reverse the multiplicative decomposition and obtain a forecast
plot(y, type="l")
lines(Tr_a,col="green")
lines(yhat_a,col="red")
accuracy(yhat_a,y) 

yf1<-forecast(yhat_a, h = l)
plot(yf1) 
#Decide which one is best

#Question 3:
dy <- diff(y)
dx <- diff(x)
#Now check for stationary:
adf.test(dy)
adf.test(dx)
acf(dy)
acf(dx)
data <- data[130:235,]
#Question 4:
y <- ts(data$total_revenue, frequency = freq)
insamp<-ts(y[1:76], frequency=freq) #90% of the data
outsamp <-y[77:106]

#First check the ACF
acf(diff(insamp))#ACF doesn't die out => non-stationary. If there is sign of seasonality do ARIMA with seasonality
pacf(diff(insamp))
adf.test(insamp)
#=> let's allow for ARIMA with seasonality
fit6 <- auto.arima(insamp) #automatically select p,d and q for ARIMA
summary(fit6)
#Check properties of the residuals
tsdisplay(residuals(fit6),lag.max=30, main='Model Residuals') # Checking the model diagnostics
#Typically looks kinda nice.

# Obtaining forecast and evaluating the performance based on the out-of-sample window
#for auto.arima chosen model
fcast5 <- forecast(fit6, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast5)
insamp
lines(ts(outsamp, start = c(11,7), frequency = freq))
plot(y)
accuracy(fcast5$mean, outsamp)

#We can try to do a simpler model:
fit2<- Arima(insamp, order=c(4,1,1), seasonal=list(order=c(1,1,0),period=freq))
summary(fit2)
#Check properties of the residuals
tsdisplay(residuals(fit2),lag.max=30, main='Model Residuals') # Checking the model diagnostics
#Typically looks kinda nice.

# Obtaining forecast and evaluating the performance based on the out-of-sample window
#for auto.arima chosen model
fcast6 <- forecast(fit2, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast6)
lines(ts(insamp, frequency = freq))
plot(y)
accuracy(fcast6$mean, outsamp)

#Question 5:
#Winters' with multiplicative seasonal component
fit4 <- HoltWinters(insamp)
fcast3 <- forecast(fit4, h=length(outsamp))
plot(fcast3)
lines(ts(y, frequency = freq))
accuracy(fcast3, outsamp)
#Check if the residuals look normal or maybe not constant:
checkresiduals(fcast3)

#Question 6:
data <- data[130:235, ]
y <- ts(data$total_revenue, frequency = freq)
x <- ts(data$n_screenings, frequency = freq)
plot(y)
plot(x)
Sz <- ts(cbind(x,y))
#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z) 
insamp<-ts(y[1:76], frequency=freq) #90% of the data
outsamp <-ts(y[77:106], start = c(16,2), frequency = freq)

linMod <- lm(y ~ x)
summary(linMod)

# Checking the residual diagnostic
plot(resid(linMod))
acf(resid(linMod)) # white noise residuals?

bptest(linMod) # Breusch-Pagan test H_0: variance is constant.
# If there are several models, to compare them we can use AIC, BIC criteria (choosing the model with min AIC or BIC)

#Forecast with linear regression:
x <- ts(data$n_screenings)
ARIMA <- auto.arima(x)
ARIMA #Notice which one
fcast <- forecast(ARIMA, h=length(outsamp))
fcast1 <- fcast$mean


lm_fcast <- linMod$coefficients[1]+linMod$coefficients[2]*fcast1
plot(lm_fcast)
lines(ts(y))
plot(y, col="blue")
lines(ts(forecast1, start = c(11,7), frequency = freq))
lines(ts(outsamp, start = c(11,7), frequency = freq), col = "red")

accuracy(forecast1, outsamp)
forecast1
outsamp

#Check if the assumptions are met:
vif(linMod)
library(lmtest)
bptest(linMod) # Breusch-Pagan test H_0: variance is constant.
dwtest(linMod, alternative = c("two.sided")) #Durbin-Watson test statistic for autocorrelation
plot(linMod$residuals)
acf(linMod$residuals)
pacf(linMod$residuals)
library(tseries)
jarque.bera.test(linMod$residuals) 

#Question 7:
#### ADL estimation ####
#Estimate an ADL
dy <- diff(y)
dx <- diff(x)
fit1<-auto_ardl(dy ~ dx, data=cbind(dx, dy), max_order = 1)
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

#Question 8:
#Check if Holtwinter and ARIMA are equally good at predicting (same predicting ability)
#Granger-Ramanathan combination method
combfit <- lm(y[1:106] ~ fit1$fitted[1:106] + fit2$fitted[,1])
combfit <- lm(outsamp ~ linMod$fitted[77:106] + fcast3$mean)
summary(combfit) #the coefficients in the regression will give you the weights
combfcast <- ts(combfit$fitted.values, frequency = freq)
accuracy(combfcast, outsamp)

out<-ts(outsamp,frequency=freq)
#plotting the initial data together with the forecast
yf2<-ts(append(insamp,combfcast), frequency = freq)
ts.plot(insamp, yf2, gpars=list(xlab="Month", lty=c(1:2)))
ts.plot(combfcast)
combfcast
insamp


scatterplot(x,y)
