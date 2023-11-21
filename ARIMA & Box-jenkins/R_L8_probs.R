library(forecast)
library(readxl)
################################################################################
######## Demand_prob7p403 ##########
################################################################################
data <- read_excel("C:/Users/Demand_prob7p403.xls")
view(data)
y<-ts(data$Demand)

## Q1 ##
  acf(y)
  # Autocorrelations fail to die out, suggesting that demand is non-stationary.
  # We can see from the data a clear upwards trending pattern
  
  dy<-diff(y) # generate the first difference of the variable 
  tsdisplay(dy)
  # Autocorrelations for first differences of demand, do die out (cut off relative to standard error limits)
  # suggesting series of first differences is stationary.   
  
  # Plotting the series and the autocorrelation function
  plot(y)
  # A plot of the demand series shows the series is increasing linearly in 
  # time with almost a perfect (deterministic) straight line pattern.  

  
 # If an ARIMA model is fit to the demand data, the autocorrelations and  
 # plots of the original series and the series of first differences, suggest an 
 # ARIMA(0,1,1) model could be good starting point. The first order moving average 
 # term is suggested by the significant autocorrelation at lag 1 for the first differenced series.  
  

# Let us create in-sample (training) and out-of-sample (test) portions of the data.
insamp <- y[1:40]
outsamp <- y[41:52]

tsdisplay(insamp, main = 'In-sample data')#plot in-sample data together with acf and pacf


## Q2 ##
# Fitting an ARIMA model to the in-sample window
fit <- auto.arima(insamp) #automatically select p,d and q for ARIMA
summary(fit)
 # The automatic choice by R is ARIMA(2,1,1) with drift 
tsdisplay(residuals(fit), main='Model Residuals') # Checking the model diagnostics


# Obtaining forecast and evaluating the performance based on the out-of-sample window
fcast <- forecast(fit, h=12)
plot(fcast)
lines(y)

accuracy(fcast, outsamp)

# # Now, let us compare it to the ARIMA specification ourselves.
fit2 <- Arima(insamp, order = c(0,1,1), include.drift = TRUE) #allow drift includes a constant in the model in cases when d=1
tsdisplay(residuals(fit2), main='Simpler Model Residuals') 

fcast2 <- forecast(fit2, h=12)
plot(fcast2)
lines(y)

accuracy(fcast2, outsamp)

# The more complex model performs better according to RMSE, 
# but not according to the other types of forecast errors

## Q3 ##
# Let's use the simpler model to write down the equation and then produce the forecast
summary(fit2)
#Yhat_53 = Y_52 (57.54) -1+epsilon_hat_52 +0.71

## Q4 ##
# Now use the parameters of the simpler model to fit it to the whole data set, 
# so that we can then obtain a forecast value for the future
f2<-Arima(y, model=fit2)
summary(f2) #preserving the estimates obtained in previous step and using them on the remaining data
forecast(f2, h=4) # obtain forecast for period 53-56


###########################
#clear the environment
rm(list = ls())
###########################


################################################################################
######## IBM stock_prob12p405 ##########
################################################################################
y <- ts(read_excel("C:/Users/IBMstock_prob12p405.xls"))

tsdisplay(y)
## a-b ##
#The autocorrelation coefficient plot indicates that the data are non-stationary.  
#Therefore, the data should be first differenced.  

dy<- diff(y)
acf(dy)
pacf(dy)
#There is not much going on in either the autocorrelations or partial autocorrelations for the differenced series. 
#Could make a case for a first order AR term in a model for the differenced data.  

## c ##
# Let us create in-sample (training) and out-of-sample (test) portions of the data.
insamp <- y[1:40, 1]
outsamp <- y[41:52, 1]

###########################
# Fitting an ARIMA model to the in-sample window
fit <- auto.arima(insamp) #automatically select p,d and q for ARIMA
summary(fit)
#It selects an ARIMA(1,1,0), exactly the case we were making earlier: first order AR term in a model for the differenced data

## d ##
#Diagnostic checks
tsdisplay(residuals(fit), main='Model Residuals') 
#The residual plots look good and there are no significant residual autocorrelations.
#There is no reason to doubt the adequacy of the model

## e ##
# Obtaining forecast and evaluating the performance based on the out-of-sample window
fcast <- forecast(fit, h=12)
plot(fcast)
lines(y)

accuracy(fcast, outsamp)


# Now use the parameters of the model to fit it to the whole data set, 
# so that we can then obtain a forecast value for the future
f<-Arima(y, model=fit)
summary(f) #preserving the estimates obtained in previous step and using them on the remaining data
forecast(f, h=1) # obtain forecast for period 53

#Naive forecast Y_t+1=Y_t
y[52,1] #our ARIMA forecast is higher than the one of the naive method

#What is the automatic forecast if no test sample assigned?
auto.arima(y)
forecast(auto.arima(y),h=1) #got even a higher value, so without testing might overestimate the upwards tendency in the data. 


###########################
#clear the environment
rm(list = ls())
###########################


################################################################################
######## Closing stock quotations_prob13p409 ##########
################################################################################
# Closing stock quotations (prob13p403)
data<- read_excel("C:/Users/ClosingStockQuatations_prob13p409.xls")
y<-ts(data$DEF)
#Splitting the sample
insamp <- y[1:145]
outsamp <- y[146:150]

tsdisplay(insamp, main = 'In-sample data') #plot in-sample data together with acf and pacf

# Fitting an ARIMA model to the in-sample window
fit <- auto.arima(insamp) #automatically select p,d and q for ARIMA
summary(fit)
tsdisplay(residuals(fit), main='Model Residuals') # Checking the model diagnostics

# Obtaining forecast and evaluating the performance based on the out-of-sample window
fcast <- forecast(fit, h=5)
plot(fcast)
lines(y)

accuracy(fcast, outsamp)


