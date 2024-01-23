library(readxl)
library(forecast)
library(tseries)
###########################
#clear the environment
rm(list = ls())
###########################
##################
#####Exam 2018####

##Case 1##
data <- read_excel("C:/Users/Business Forecasting 2021/Week 49/Exam2018Case1.xlsx")
View(data)


##1
#Determine the key variable of interest, i.e. the one you want to forecast
lr<-ts(data$LR, start=1960, frequency=1)
tsdisplay(lr)
tsdisplay(diff(lr))

#Define the secondary variable, the one you can use as an explanatory variable in your regression for example. 
sr<-ts(data$SR, start=1960)
tsdisplay(sr)
tsdisplay(diff(sr))

mr<-ts(data$MR, start=1960)
tsdisplay(mr)
tsdisplay(diff(mr))

#group data for further analysis
z <- ts(cbind(lr,sr,mr), frequency = 1)




##2
insamp<-ts(lr[1:31], start=1960)
outsamp <-lr[32:length(lr)]

#Estimating an Arima model
model1<-auto.arima(insamp) #estimated the model on 1960-1990 data
summary(model)
tsdisplay(residuals(model1))
fcast1<-forecast(model1,h=length(outsamp))$mean #obtain a forecast for 1991-2000 using model f1 and assuming no data for that period is available
accuracy(fcast1, outsamp) #compare this accuracy for the MA based on 1960-1990 data forecast


#Forecasting with a simple MA of order 3
k<-3
l<-length(insamp) #number of available data points
#adding extra rows, as many as periods ahead want to forecast
h <-10
y <- insamp 
########################################
#generating space to save the forecasts
for (i in 1:h){
  y <- c(y, 0)
}
#calculating the forecast values
for (j in (l+1):(l+h) ){
  a<-j-k
  b<-j-1
  x <-y[a:b]
  y[j] <- mean(x)
}
#########################################
fcast2<-y[32:length(lr)]
accuracy(fcast2,outsamp)

#plotting the forecasts on the graph
f2<-ts(fcast2, start='1991', frequency =1) #Moving average
f1<-ts(fcast1, start='1991', frequency =1) #ARIMA
ts.plot(lr, f2, f1, type="l", col=c("black", "green","red"))


fit1<-Arima(lr, model=model1)#preserve the ARIMA model obtained on the insamp, but use all the available data (1960-2000)
forecast1<-forecast(fit1,h=18)$mean #Producing forecast for 2001-2018


#Forecasting with a simple MA of order 3
k<-3
l<-length(lr) #number of available data points
#adding extra rows, as many as periods ahead want to forecast
h <-18
y <- lr
########################################
#generating space to save the forecasts
for (i in 1:h){
  y <- c(y, 0)
}
#calculating the forecast values
for (j in (l+1):(l+h) ){
  a<-j-k
  b<-j-1
  x <-y[a:b]
  y[j] <- mean(x)
}
#########################################
forecast2<-ts(y[(length(y)-18+1):length(y)],start=2001, frequency=1)

ts.plot(lr, forecast1, forecast2, type="l", col=c("black", "red","green"))




##3
fit<-lm(lr~sr+mr)
summary(fit)
library(car)
vif(fit)
library(lmtest)
bptest(fit) # Breusch-Pagan test H_0: variance is constant.
dwtest(fit, alternative = c("two.sided")) #Durbin-Watson test statistic for autocorrelation
acf(fit$residuals)
pacf(fit$residuals)
library(tseries)
jarque.bera.test(fit$residuals) #Jarque-Bera test the null of normality 




##4
dlr<-diff(lr)
dmr<-diff(mr)
fit2<-lm(dlr~dmr)
summary(fit2)
dwtest(fit2, alternative = c("two.sided")) #Durbin-Watson test statistic for autocorrelation
acf(fit2$residuals)




##5
#In order to use a LM for prediction, need to have new data for the explanatory variable, or a prediction for it
mrf1<-auto.arima(mr)
mrf2<-forecast(mrf1,h=19) #need 19 periods ahead, because will take differences for running the regression, so will loose one observation
mrf<-mrf2$mean

dmrf<-diff(mrf)
dlrf<-fit2$coefficients[1]+fit2$coefficients[2]*dmrf
forecast2<-lr[41]+cumsum(dlrf)
View(forecast2)




##6
library(vars)
# Taking the first difference to make the series stationary for VAR analysis.
dz<- diff(z)
VARselect(dz, lag.max = 5, type="const")[["selection"]] #determine the order of VAR
var1 <- VAR(dz, p=1, type="const") #estimate VAR(1)
summary(var1)
#Obtaining impulse response functions
plot(irf(var1,boot = TRUE, ci=0.95))




##7
#Testing each of the series for unit root
adf.test(lr)
adf.test(sr)
adf.test(mr)
#Performing a cointegration test
po.test(z) #Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration





###########################
#clear the environment
rm(list = ls())
###########################
##################
##Case 2##
data <- read_excel("C:/Users/Business Forecasting 2021/Week 49/Exam2018Case2.xlsx")
View(data)


##1
bc<-ts(data$`Bad calls`, frequency=24)
tsdisplay(bc)




##2
decompSalesMult <- decompose(bc, type = "multiplicative") 
plot(decompSalesMult)




##3
pres<-ts(data$Pressure, frequency=24)
wind<-ts(data$`Wind speed`,frequency=24)
adf.test(bc)
adf.test(wind)
adf.test(pres)

fit1<-lm(bc~pres+wind)
summary(fit1)
plot(fit1$residuals)
accuracy(fit1)




H<-24*7 #forecasting horizon
##4
#Forecast based on smoothing
fit2 <- HoltWinters(bc)
fcast2 <- forecast(fit2, h=H)$mean
accuracy(fit2$fitted[,1],bc)




##5
#In order to use a LM for prediction, need to have new data for the explanatory variable, or a prediction for it
pres1<-auto.arima(pres)
#pres1<-Arima(pres, order=c(2,1,1), seasonal=c(0,0,2))
pres2<-forecast(pres1,h=H)
presf<-pres2$mean

wind1<-auto.arima(wind)
#wind1<-Arima(wind, order=c(4,0,0), seasonal=c(2,0,0))
wind2<-forecast(wind1,h=H)
windf<-wind2$mean

fcast1<-fit1$coefficients[1]+fit1$coefficients[2]*presf+fit1$coefficients[3]*windf


##6
#Granger-Ramanathan combination method
combfit <- lm(bc[25:2016] ~ fit1$fitted[25:2016] + fit2$fitted[,1])
summary(combfit) #the coefficients in the regression will give you the weights
combfcast <-combfit$coefficients[1]+combfit$coefficients[2]*fcast1+combfit$coefficients[3]*fcast2
combfcast<-ts(combfcast, frequency=24)
ts.plot(combfcast)


