library(readxl)
library(tibble)
library(forecast)
###########################
#Example of using generalised differencing to account for autocorrelation#
###########################
data <- read_excel("C:/Users//Business Forecasting 2021/Week 46/Autocorrelation.xlsx")

#fetching the dependent and explanatory variables, specifying them as time series
y<-ts(data$Y)
x<-ts(data$X)

#Estimating an OLS of Y on X
fit<- lm(y~x) #let's call this equation (1)
summary(fit)

#Durbin-Watson serial correlation test
library(lmtest)
dwtest(fit) #rejecting the null => have autocorrelation problems in model (1)

#Assume the errors follow an AR(1) process, with a coefficient rho. 
#In order to obtain the generalized difference, need to estimate rho. 
#Use residuals from eq (1) as an estimator for the error term.

#obtain residuals from eq (1)
e<- fit$residuals

#generate a variable the is the e_t-1 (lagged e)
library(Hmisc)
le<-Lag(e, shift = 1)

#estimate and AR(1) process for the residuals, without a constant)
fit2<- lm(e~0+le) #0 implies there is no constant in the estimated model
summary(fit2) # estimated rho is 0.79 and significant (confirms presence of autocorrelation)

rho<-fit2$coefficients #fetch the estimate of rho for further usage

#generate generalised differences
ygd<- y - rho*Lag(y,shift=1)
xgd<- x - rho*Lag(x,shift=1)

#estimate an OLS regression for the transformed variables
fit3<-lm(ygd~xgd)
summary(fit3)

#perform a DW test to check that there is no autocorrelation problem left in the model with transformed variables. 
dwtest(fit3) #cannot reject the null => no autocorrelation.
#mission completed :) 
