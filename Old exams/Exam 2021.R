library(readxl)
library(tibble)
library(forecast)
library(tseries)
library(lmtest)
library(vars)

data <- read_excel("~/Business Forecasting/Old exams/Data/RUBUSD.xlsx")

#Question 1: #### What components are present in the data?
plot(decompose(y, type = "multiplicative"))
plot(decompose(y, type = "additive"))
#We can see a decreasing trend in the data, together with seasonality. The random part of the plot
#shows more or less same variance throughout the whole period. It looks slightly better with additive
#decomposition, and there is no sign of increasing variance from the original plot.

#What frequency should you assign to the data?
#It is set to 5 because it does not take weekends, only weekdays

#Are the data stationary/non stationary?
y<- ts(data$Rate, frequency=5)
x<- ts(data$Oil, frequency=5)
tsdisplay(y)
tsdisplay(x)
#Clearly a decreasing trending pattern for both variables. Clear signs of non-stationarity
#because the ACF does not die out but declines slowly - shows a clear trend in the data.
#But lets do a statistical test to be sure:
adf.test(y)
adf.test(x)
#Both variables do not have a p-value below 5%. They are both non stationary which is the same conclusion
#as we saw from the ACF.

#Question 2: #### You want to consider a regression framework to obtain a prediction. What is your
#dependent variable? Given the properties of the data, what do you need to check for
#prior to runing a regression?

#The dependent variable in this case is the Rate between RUB/USD, and we will find out if the oil prices have a
#influence on the rate variable.

#When doing linear regression we want to check for co-integration. 
#Lets check if the differenced variables also are non-stationary:
z <- ts(cbind(x,y))
#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z)
#We can not reject the null-hypotheses meaning that the variables show no clear sign of co-integration
#We have to proceed the regression with differenced variables -1 lag.
dy <- diff(y)
dx <- diff(x)
tsdisplay(dy)
tsdisplay(dx) 
adf.test(dy)
adf.test(dx)
#When differencing on the first lag both variables become stationary.

#Question 3: Based on the results you have obtained in the previous question, transform the data
#if necessary to obtain a valid regression. Run a simple linear regression, check if the
#residuals satisfiy the OLS assumptions, then interpret the results.
lm1 <-tslm(dy~dx)
summary(lm1)

plot(resid(lm1))
#There is no clear pattern from the scatter plot. Looks like there is heteroscesdcasisty
acf(resid(lm1))
#Looks rather perfect when looking at the ACF. There is no significant spikes.
bptest(lm1) 
#The Brausch-Pagan tells that the variance is not constant. We can reject H0
#The OLS does not meet the assumptions.
dwtest(lm1)
#Do not have any autocorrelation problems.
jarque.bera.test(lm1$residuals) #Check if residuals show normality
#Again we reject the H0 - signs a non normality.

#Question 4: ####
#Using the regression, produce a one month ahead forecast for the variable of interest,
#assuming the estimated coefficients remain the same.

#We need future values of Oil prices so we need to forecast these with ARIMA
ARIMA_oil1 <- auto.arima(x)
ARIMA_oil1
fcast_oil2 <- forecast(ARIMA_oil1, h=21)
fcast_oil <- fcast_oil2$mean

dfcast_oil <- diff(fcast_oil)
d_oil<-lm1$coefficients[1]+lm1$coefficients[2]*dfcast_oil
forecast1<-y[1046]+cumsum(d_oil)
plot(forecast1)

#Question 5: ####
y1<- ts(data$Rate)
f1<-auto.arima(y1) 
f2<-auto.arima(y)
tsdisplay(residuals(f2))
#The software chooses a model model with two AR-components, and two MA components
#One difference with one lag behind, and one seasonal.
#Or just a random walk.
summary(f1)
summary(f2)

fcast2<-forecast(f2, h = 20)
fcast2$mean
plot(fcast2$mean)
#The regression predicted that the rate would drop a bit, but ARIMA
#predicts its rather linear the next 20 periods. 

#Question 6: ####
z <- ts(cbind(dy,dx), frequency = 5)
#determine the order of VAR
VARselect(z, lag.max =10, type="const")[["selection"]]
var1 <- VAR(z, p=1, type="const") #estimate VAR(1)
summary(var1)
#It looks like that the lags that explains rate the most is oil prices 1, 3 and 4 periods
#back. Non of the lags explains the oil prices.

#Can do model diagnostics as in AR models
#Checking for serial correlation in the errors
serial.test(var1, lags.pt=5, type="PT.asymptotic")
# The null is off no serial correlation

#Obtaining impulse response functions
plot(irf(var1,boot = TRUE, ci=0.95))
#We can not really say anything

fcast3 <- forecast(var1, h=20)
plot(fcast3)

#Aggregate data to levels
fcast_y<-y[1046]+cumsum(fcast3$forecast$y$mean)
fcast_y
library(ggplot2)
Date <- seq(1:length(y))
ggplot(data, aes(x=Date)) +
  geom_line(aes(y = append(y[1:1046],fcast_y)), color = "red")+    
  geom_line(aes(y = y)) +
  scale_y_continuous(name = "")

############################### Second Case #####################################
data <- read_excel("~/Business Forecasting/Old exams/Data/Cattle.xlsx")
#Question 1: #### Examine the data set and describe its dynamic properties.
y<- ts(data$Sales, frequency=12)
tsdisplay(y)
#From the ACF we can see a clear trend. The spikes does not die. From the original plot we can se
#a drecreasing trend. From the ACF we also observe seasonality every 5th lag. Looks like a steady variance
#We will test for stationarity:
adf.test(y)
#The DF test says it is a staionary data set

#Question 2: #### Decompose the series. Which type of decomposition would you use?
#I would recommend a additive one. The variance does not increase by time
plot(y)
mult <- plot(decompose(y, type = "multiplicative"))
add <- plot(decompose(y, type = "additive"))

#Question 3: #### Split the data into two subsets, let the training set be 85% of the data.
insamp<-ts(y[1:263], frequency=12) #85% of the data
outsamp <- y[264:length(y)]

#Question 4: ####
#ARIMA
f1<-auto.arima(insamp) 
summary(f1)
#the ARIMA software chooses a AR 3 component.
tsdisplay(residuals(f1))
#The residuls look pretty good. ACF has only a few spikes, but nothing significant. 
fcast1<-forecast(f1, h = length(outsamp))
plot(fcast1)
lines(ts(y, frequency = 12))
accuracy(ts(fcast1,frequency=12), outsamp)
#Pretty nice RMSE on training and test data. The forecast tends to underestimate the downward spikes

#Forecasting with a simple MA of order 7
k<-12
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
accuracy(MA[264:309],outsamp)
#A slightly worse test RMSE than ARIMA, it just predicts that the sales will be steady
plot(MA)

# HoltWinters
fit2 <- HoltWinters(insamp)
fcast2 <- forecast(fit2, h=length(outsamp))
plot(fcast2)
lines(ts(y, frequency = 12))
accuracy(fcast2, outsamp)
#Also looks quite good. predicts the spikes better.

accuracy(fcast1, outsamp) #ARIMA
accuracy(MA[264:309],outsamp) #MA of order 7
accuracy(fcast2, outsamp) #HoltWinters

#Question 5: #### After comparing the results, choose the best model. Then use the full data set to obtain
#a year ahead prediction for the demand.
# HoltWinters
fit3 <- HoltWinters(y)
fcast3 <- forecast(fit3, h=12)
fcast3
plot(fcast3)
lines(ts(y, frequency = 12))
accuracy(fcast3, outsam)

#Question 6: #### Next you want to see if you can improve on the quality of the forecast by using a forecast
#combination method.
combfit_eq<-0.5*fcast1$mean + 0.5*fcast2$mean
#Calculate the weights (GR method)
combfit <- lm(outsamp ~ fcast1$mean + fcast2$mean)
summary(combfit) #the coefficients in the regression will give you the weights
accuracy(combfit$fitted, outsamp)

combf <- ts(combfit$fitted, frequency = 12)
combfe<- ts(combfit_eq, frequency=12)
outsamp <-ts(y[264:length(y)], frequency = 12)
ts.plot(outsamp, combf, combfe, gpars=list(xlab="Date", lty=c(1:2), col=c("black", "green","blue")))
