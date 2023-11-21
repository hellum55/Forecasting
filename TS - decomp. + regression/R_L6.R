library(forecast)
library(readxl)
################################################################################
######## Alomega Sales ##########
################################################################################
Alomegasales<- read_excel("C:/Users//Business Forecasting 2021/Week 46/Alomegafoodstores.xlsx")
View(Alomegasales)

## Fetch the dependent variable, Sales, from the data
y <-ts(Alomegasales$Sales,end = c(2006, 12), frequency=12) # the data is monthly, so we choose frequency 12
plot(y)
trend <- seq(1:length(y)) #creates a linear trend variable (time counter)

## Fit a linear trend model
fit <- lm(y ~ trend)
#if we want to extract the seasonal components can add dummies for months
summary(fit)
accuracy(fit$fitted.values,y)

# Plot the data together with the linear trend
yfit=ts(fit$fitted.values,end = c(2006, 12), frequency=12) #specify the ts type of data for the fitted values to make sure the plotting works
plot(y)
lines(yfit, col="red",type="l")

### One can project the trend line into future months for sales forecasts, 
### although, as the case suggests, accurate forecasts will not result:  
### The MAPE using only the trend line is 28%.



# Multiplicative Decomposition with trend and seasonal components (no cyclical)
decompSalesMult <- decompose(y, type = "multiplicative") #for multiplicative decomposition
plot(decompSalesMult)

#create separate variables for each of the estimated components
Tr<-decompSalesMult$trend
S<-decompSalesMult$seasonal
I<-decompSalesMult$random # These are really the residuals!

# Using the seasonal factor create a seasonal index by standardizing
SI <- scale(S[1:12]) #the indicies are the same for repeated season, so only need 12 of them as have 12 different months
plot(SI, type="h") 

# Generate the fitted values (in-sample fit) using the decomposition
yhat<-Tr*S*1 #use the components to reverse the multiplicative decomposition and obtain a forecast
plot(y, type="l")
lines(Tr,col="green")
lines(yhat,col="red")

accuracy(yhat,y) 

yf<-forecast(yhat, h = 12) #calculate forecasts 12 periods ahead, based on the carried out decomposition method
#given that estimated components are lagging behind, the end forecast is produced 12 periods ahead from the last estimated component time point, and not the end of the data. 
summary(yf)



#Multiplicative Decomposition with a cyclical component
  #Obtaining the cycle series based on the slides
  #centered moving average
CMA <- ma(y, order=12, centre=TRUE) #desesonalized data
  #seasonal facto
SF<-y/CMA
plot(SF, type="l")
abline(h=1, col = "red")
  #finding the trend
linmod <- lm(CMA ~ trend, na.action = "na.exclude")
CMAT <- linmod$fitted.values
  #cycle factor
Cycle <- na.exclude(CMA) / CMAT
ts.plot(Cycle)
abline(h=1, col = "red") 



#Time series regression
#fetch explanatory variables
paper<-ts(Alomegasales$Paper, frequency=12)
tv<-ts(Alomegasales$TV, frequency=12)
month <- seasonaldummy(y) #creates seasonal dummies, if such not available in the data
#Regression model for sales
reg <-lm(y~paper+tv+month)
summary(reg)

#testing for multicollinearity
library(car)
vif(reg)

#Durbin-Watson serial correlation test
library(lmtest)
dwtest(reg)

#obtain residuals from the model to perform model quality checks
res<-reg$residuals
plot(res)
#check for normality
hist(res)
library(tseries)
jarque.bera.test(res) #Jarque-Bera test the null of normality 
#serial correlation
acf(res)
plot(acf(res,plot=F)[1:20])#alternative command if you want to skip the 0 lag


##Case7_Q1
#Julie appears to have a good regression equation with a high adjusted R2.  
#Additional significant explanatory variables may be available but there is not much
#variation left to explain. 
# One can still check the full range of available variables.
# Along with the adj R2 value, one should check the t values for the variables in their final equation,
# the F value for the regression and the residual autocorrelations. 





###########################
#clear the environment
rm(list = ls())
###########################

################################################################################
######## Sales data, seasonal ##########
################################################################################
Salesdata <- read_excel("C:/Users//Business Forecasting 2021/Week 46/Sales data.xlsx")
View(Salesdata)

y <- ts( Salesdata$Sales, frequency = 12, start = c(1979, 1))
plot(y)

#1
#Using regression with trend and seasonal dummies to create additive decomposition:

month <- seasonaldummy(y) #creates seasonal dummies
trend <- seq(1:length(y)) #creates a linear trend

fit <- lm(y ~ month + trend) #fits a linear model (OLS)
summary(fit)

plot(fit$fitted.values)
lines(fit$fitted.values, col = "red")

plot(resid(fit))
lines(resid(fit), col = "red")

#2
#Alternative way to decompose time series (probably much easier to use :))
decompSalesAdd <- decompose(y, type = "additive") #for additive decomposition
decompSalesMult <- decompose(y, type = "multiplicative") #for multiplicative decomposition

plot(decompSalesAdd)
plot(decompSalesMult)
#Note that these plots do not show the cycle series...

#3
#Obtaining the cycle series based on the slides
CMA <- ma(y, order=12, centre=TRUE)
linmod <- lm(CMA ~ trend, na.action = "na.exclude")
CMAT <- linmod$fitted.values
Cycle <- na.exclude(CMA) / CMAT
ts.plot(Cycle)
abline(h=1, col = "red")




