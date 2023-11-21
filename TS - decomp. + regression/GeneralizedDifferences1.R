library(readxl)
library(tibble)
library(forecast)
###########################
#Example of using generalised differencing to account for autocorrelation#
###########################
data <- read_excel("C:/Autocorrelation.xlsx")


View(data)

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



###If you now want to use the model for forecasting, given it's a classical regression, 
#then you would need future values of X in order to obtain a forecast for Y. 

# Assuming you have the value of x for additional 3 periods, let's call these values x_future.
x_future <- c( tail(x, n = 1), 12.5, 12.8, 13) # start by putting in the last observed value of X 

# Calculate generalized differences for the future values of x
xgd_future <- x_future - rho * Lag(x_future, shift = 1)

# Forecast generalized differences for y
ygd_forecast <- predict(fit3, newdata = data.frame(xgd = xgd_future))

# Calculate forecasts for y
# Initialize a vector to store the forecasts for y
y_forecast <- c( tail(y, n = 1),numeric(3))
# Forecast for the next 3 periods
for (i in 2:4) {
  y_forecast[i] <- ygd_forecast[i] + rho * y_forecast[i-1]
}
# y_forecast now contains the forecasted values of y for the 3 additional periods.

#Next we can add the predicted values after the available data and plot it
y_with_forecasts <- c(y, y_forecast) # Now, you have a time series that includes the original values of y and the forecasted values.

# Create a time index for the extended time series
time_index <- 1:length(y_with_forecasts)

# Plot the extended time series with forecasts
plot(time_index, y_with_forecasts, type = "l", xlab = "Time", ylab = "Y", main = "Original and Forecasted Y", col = "blue")

# Add the original series to the plot
lines(time_index[1:length(y)], y, col = "black", lty = 1)

# Add vertical lines to indicate the separation between the original data and forecasts
abline(v = length(y), col = "red")

# Optionally, you can add a legend to the plot
legend("topleft", legend = c("Original Y", "Forecasted Y"), col = c("black", "blue"), lty = 1)


