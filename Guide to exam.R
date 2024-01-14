library(readxl)
library(tibble)
library(forecast)
library(tseries)
library(lmtest)
library(vars)
library(dLagM)
library(ARDL)
#read data:
data <- read_excel("~/Business Forecasting/Old exams/Data/RUBUSD.xlsx")
#Remove NA's
data <- na.omit(data)
sum(is.na(data))
#forecast period:
l <- 7
#### Properties of the data ####
#Decide on which frequency to be applied
freq <- 7
y <- ts(data$Rate, frequency=freq)
x <- ts(data$Oil, frequency=freq)
z <- ts(data$Date, frequency = freq)
#Check for properties in the data:
tsdisplay(y)
tsdisplay(x)
#Check if the variables are stationary or not:
adf.test(y)
adf.test(x)
#If we cannot reject the null-hypotheses check if the differenced y and x are:
dy <- diff(y)
dx <- diff(x)
dz <- diff(z)
#Now check for stationary:
adf.test(dy)
adf.test(dx)

#Check the correlations between the y and x
Y <- data$Rate
X <- data$Oil
# Creating a scatter plot with a smooth-line fitting
scatter.smooth(x=X, y=Y, main="Y ~ X^2") 
# Calculate the correlation between the series
cor(Y, X)
cor.test(Y,X)

#### Decomposition ####
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

#### Check for co-integration i linear relationship:
adf.test(y) #The null is that series contain a unit root, i,e, non-stationary
adf.test(x)
dy <- diff(y)
dx <- diff(x)
adf.test(dy) #The null is that series contain a unit root, i,e, non-stationary
adf.test(dx)
#They should be stationary by now
#Combining the two vectors x and y 
z <- ts(cbind(x,y))
#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z) 
#If the H0 is rejected. There is a long term relationship. If H0 not rejected continue with dy/dx or just one of them.

#### Linear Regression ####
#Regular linear regression:
# Building a linear model using lm() function, Replicating the table
linMod <- lm(Y ~ X) #To omit intercept when necessary, the formula can be written for example as lm(cost ~ age - 1)
summary(linMod)

# Checking the residual diagnostic
plot(resid(linMod))
acf(resid(linMod)) # white noise residuals?

bptest(linMod) # Breusch-Pagan test H_0: variance is constant.
# If there are several models, to compare them we can use AIC, BIC criteria (choosing the model with min AIC or BIC)
AIC(linMod)  
BIC(linMod)

#Linear regression on the trend and the seasonal dummy:
trend <- seq(1:length(y))
dummy <- seasonaldummy(y)
fit <- lm(data$Rate ~ trend)
summary(fit)
accuracy(fit$fitted.values,y)

# Plot the data together with the linear trend
yfit=ts(fit$fitted.values, frequency=freq) #
plot.ts(y, type="l")
lines(yfit, col="green")
lines(yhat_m, col="red")

#compare the residuals from two models, i.e. comparing detrended data with detrended and deseasonalised data.
plot(fit$residuals)
lines(fit$residuals, col="red")
acf(fit$residuals, 50)

#### Linear regression with dy/dx ####
dy <- diff(y)
dx <- diff(x)
fit2 <- lm(dy~dx)
summary(fit2)
accuracy(fit2$fitted.values,dy)
#Plot the residuals to check if they are constant
plot(fit2$residuals)
lines(fit2$residuals, col="red")
acf(fit2$residuals, 50)
#Check with a Braush-Pagan test: H0 variance is constant
bptest(fit2)
jarque.bera.test(fit2$residuals) #Check if residuals show normality
#Again if we reject the H0 - signs of non normality.
dwtest(fit2) #null hypothesis that the autocorrelation of the disturbances is 0

#Forecast with linear regression:
ARIMA <- auto.arima(x)
ARIMA #Notice which one
fcast <- forecast(ARIMA, h=(l+1))
fcast1 <- fcast$mean

dfcast <- diff(fcast1)
d <- fit2$coefficients[1]+fit2$coefficients[2]*dfcast
forecast1 <- y[length(y)]+cumsum(d)
plot(forecast1)
plot(y, col="red")
lines(ts(forecast1, start = 151, frequency = freq))

#### Split the data ####
T <- round(length(y)*0.8)
insamp<-ts(y[1:T], frequency=freq) #90% of the data
outsamp <-y[(T+1):length(y)]

#### Simple forecasts MA/ES ####
#12-period moving average smoothing with "order" specified
yt <- ma(y, order=freq, centre=TRUE)
lines(yt, col="red") #lines() function will add to the existing graph 

#Forecasting with a simple MA of order 7
k<-freq
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
accuracy(MA[(T+1):length(y)],outsamp)
#############################################
#Forecasting 10 periods ahead with MA of order 3
yt <- data$Rate
k<-freq #length of your MA window
#starting point is your actual data, replace yt with your variable of interest name
l<-length(yt) #number of available data points
h <-10 #number of periods ahead you want to forecast
########################################CODE STARTS
y <- yt 
#generating space to save the forecasts
for (i in 1:h){
  y <- c(y, 0)
}
#calculating the fitted values (MA for the periods already available, will need it to calculate the RMSE)
yfit <- rep(NA,l)
for (j in (k+1):(l) ){
  a<-j-k
  b<-j-1
  x <-y[a:b]
  yfit[j] <- mean(x)
}
#calculating RMSE
e<-yt-yfit
RMSE<-sqrt(sum(e^2,na.rm = TRUE)/length(yfit))
#calculating the forecast values 
##(extends the data variable with the forecasted values, so the y series is ready for plotting)
for (j in (l+1):(l+h) ){
  a<-j-k
  b<-j-1
  x <-y[a:b]
  y[j] <- mean(x)
}
#########################################CODE ENDS
plot(y)

# HoltWinters
fit3 <- HoltWinters(insamp)
fcast2 <- forecast(fit3, h=length(outsamp))
plot(fcast2)
lines(ts(y, frequency = freq))
accuracy(fcast2$mean, outsamp)


#Winters' with additive seasonal component
fit5 <- HoltWinters(insamp, seasonal =c("additive"))
fcast4 <- forecast(fit5, h=length(outsamp))
plot(fcast4)
lines(ts(y, frequency = freq))
accuracy(fcast4, outsamp)

#Winters' with multiplicative seasonal component
fit4 <- HoltWinters(insamp, seasonal =c("multiplicative"))
fcast3 <- forecast(fit4, h=length(outsamp))
plot(fcast3)
lines(ts(y, frequency = freq))
accuracy(fcast3, outsamp)
#Check if the residuals look normal or maybe not constant:
checkresiduals(fcast2)

ContactsF<-fcast2$mean*fcast4$mean
ContactsF

#### ARIMA-models ####
#Generate a training and test sub-samples and ts() them
insamp<-ts(y[1:T], frequency=freq)
outsamp<- ts(y[(T+1):length(y)], start = c(120,5), end = c(150,3), frequency=freq)

#First check the ACF
tsdisplay(insamp)#ACF doesn't die out => non-stationary. If there is sign of seasonality do ARIMA with seasonality
#=> let's allow for ARIMA with seasonality
fit6 <- auto.arima(insamp, seasonal=TRUE) #automatically select p,d and q for ARIMA
summary(fit6)
#Check properties of the residuals
tsdisplay(residuals(fit6),lag.max=50, main='Model Residuals') # Checking the model diagnostics
#Typically looks kinda nice.

# Obtaining forecast and evaluating the performance based on the out-of-sample window
#for auto.arima chosen model
fcast5 <- forecast(fit6, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast5)
lines(ts(y, frequency = freq))
plot(y)
accuracy(fcast5$mean, outsamp)

#Lets look at a simpler model:
fit7<- Arima(insamp, order=c(1,1,1))
summary(fit7) 
tsdisplay(residuals(fit7),lag.max=50, main='Model Residuals') 
#Very good
fcast6 <- forecast(fit7, h=length(outsamp))
plot(fcast6)
lines(ts(y, frequency = freq))
accuracy(fcast6$mean, outsamp)

#estimate the out-of-sample accuracy of the forecast
View(data.frame(fcast6$mean,outsamp)) 

#Check if Holtwinter and ARIMA are equally good at predicting (same predicting ability)
dm.test(residuals(fcast2), residuals(fcast5), h=length(outsamp)) #The null hypothesis is that the two methods have the same forecast accuracy.
#In this case equally good.
dm.test(fcast2$mean-outsamp,fcast5$mean-outsamp, h=length(outsamp)) #WE can compare the performance of the models out-of-sample
#Now they are not equal.

#### Combining forecast models ####
# Nelson combination method
combfitN <- lm(outsamp ~ fcast2$mean + fcast5$mean)
summary(combfitN)
#the intercept is significant => there is a bias, we need to correct the data for it
outsampcor<-outsamp-combfitN$coefficients[1] #where combfitN$coefficients[1] picks out the intercept value from the estimated regression
# Now want to run an OLS without an intercept on the corrected (debiased data)
#with respect to a restriction on the weights:  w1 + w2 = 1
fitW <- lm(outsampcor ~ 0+ offset(fcast2$mean) + I(fcast5$mean-fcast2$mean))
coef_2 <- coef(fitW)
beta_1 <- 1 - coef_2 #the weight is negative, would prefer a different combination method in this case
beta_2 <- coef_2
#beta_1 and beta_2 will give you the weights. 
# Now can use those weights to obtain a combination forecast
combfcastN <-beta_1*fcast2$mean+beta_2*fcast5$mean
accuracy(combfcastN, outsamp) #can see that in this case the forecast combination performes worse than the individual forecasts
combfcastN

#plotting the initial data together with the forecast
yf1<-ts(append(insamp,combfcastN), frequency = freq)
ts.plot(y, yf1, gpars=list(xlab="Month", lty=c(1:2)))


# Granger-Ramanathan combination method (GR)
combfit <- lm(outsamp ~ fcast2$mean + fcast5$mean)
summary(combfit) #the coefficients in the regression will give you the weights
combfcast <- ts(combfit$fitted.values, frequency = freq)
accuracy(combfcast, ts(outsamp, start = c(1,1), end = c(30,6), frequency = freq))
combfcast

#plotting the initial data together with the forecast
yf2<-ts(append(insamp,combfcast), frequency = freq)
ts.plot(y, yf2, gpars=list(xlab="Month", lty=c(1:2)))

# Equal weights (EW)
combfcastE <- 0.5*fcast2$mean + 0.5*fcast5$mean
accuracy(combfcastE,outsamp)

#plotting the initial data together with the forecast combinations: GR and EW
out<-ts(outsamp,frequency=freq)
yf3<-ts(combfcastE,frequency = freq)
ts.plot(out,combfcast, yf3, gpars=list(xlab="Month", lty=c(1:3)), col=c(rep("black",1),rep("blue",1),rep("red",1)))

#### Dynamic forecasting ####
y <- ts(data$Rate, frequency=freq)
x <- ts(data$Oil, frequency=freq)
dy<-diff(y)
dx<-diff(x)

#Estimate an ADL
model<-auto_ardl(dy~dx,data=cbind(dx, dy), max_order = 12)
fit6<-model$best_model #saves the best model
fit6$order #shows the order of the ADL selected
summary(fit6)

#alternatively (if want to decide on lag order ourselves)
model1<-ardlDlm(x=diff(data$Oil),y=diff(data$Rate),p=1,q=1) #where p is lags of x and q is lags of y

###################### VAR ###############################
y <- ts(data$Rate, frequency=freq)
x <- ts(data$Oil, frequency=freq)
#group data for further analysis
z <- ts(cbind(y,x), frequency = freq)

# Taking the first difference to make the series stationary for VAR analysis.
dz<- diff(z)

##### VAR: Estimation #####
#determine the order of VAR
VARselect(dz, lag.max = 10, type="const")[["selection"]] #optimal p =1
var1 <- VAR(dz, p=1, type="const") #estimate VAR(1)
summary(var1)

#Can do model diagnostics as in AR models
#Plot the autocorrelation functions for the VAR residuals
acf(residuals(var1))
#Checking for serial correlation in the errors
serial.test(var1, lags.pt=10, type="PT.asymptotic")
### The null is off no serial correlation

#Checking stability of VAR
var1.stable <- stability(var1, type = "OLS-CUSUM")
plot(var1.stable)
roots(var1)

#Obtaining impulse response functions
plot(irf(var1,boot = TRUE, ci=0.95))

##### VAR: Forecasting Exercise ####  
#split the sample into training and test
insampdz <- ts(dz[1:837, 1:2], frequency = freq)
outsampdz <- ts(dz[838:1045, 1:2], frequency = freq)

#select the order of VAR
VARselect(insampdz, lag.max = 10, type="const")[["selection"]]

#Fit the model on training data
fit7 <- VAR(insampdz, p=1, type="const")
summary(fit7)
#check the residuals of the model
serial.test(fit7, lags.pt=10, type="PT.asymptotic")

#Obtain the forecast for the test period
length(outsampdz)
fcast8 <- forecast(fit7, h=416)
plot(fcast) #provides a plot of the forecasts

#calculate the accuracy of the forecast for the ltn variable
accuracy(fcast$forecast$ltn, outsampdz[1:25, 2])

#Aggregate data to levels
fcast_ltn<-ltn[91]+cumsum(fcast$forecast$ltn$mean)
library(ggplot2)
Date <- seq(1:length(ltn))
ggplot(EuroMacroData, aes(x=Date)) +
  geom_line(aes(y = append(ltn[1:91],fcast_ltn)), color = "red")+    
  geom_line(aes(y = ltn)) +
  scale_y_continuous(name = "")

#### ADL estimation ####
#Estimate an ADL
dy <- diff(y)
dx <- diff(x)
fit1<-auto_ardl(dy ~ dx, data=cbind(dx, dy), max_order = 12)
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
ARIMA <- auto.arima(x)
ARIMA
fcast <- forecast(ARIMA, h=3)
fcast1 <- fcast$mean

dfcast <- diff(fcast1)

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











