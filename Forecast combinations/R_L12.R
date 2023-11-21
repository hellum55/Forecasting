library(readxl)
library(forecast)
library(ggplot2)
library(tseries)
#########################
data <- read_excel("C:/Users/AirPassengers.xlsx")

# Define the variable and pick the frequency
y <- ts(data$`#Passengers`,start=1949, frequency = 12)
tsdisplay(y)

#Split the sample prior to analysis
# In-sample (75%) and out-of-sample (25%) split
insamp <- ts(y[1:108], frequency = 12)
outsamp <- y[109:144]
l<-length(outsamp) #generate a number called "l" equal to the length of the test set


# # # FORECAST 1 # # #
# # Generating the first forecast based on ARIMA models
fit <- auto.arima(insamp, seasonal=TRUE) #We have a clear seasonal component, so should ask R to allow for it
summary(fit) #model of choice is ARIMA(1,1,0)(0,1,0)[12]
tsdisplay(residuals(fit), main='Model Residuals')
fcast <- forecast(fit, h=l)
plot(fcast)
lines(y)

accuracy(fcast, outsamp)


# # # FORECAST 2 # # #
# # Now let us get a second forecast. Holt-Winters method could be a good choice (we have both trend and seasonal component)
fit2 <- HoltWinters(insamp)
fcast2 <- forecast(fit2, h=l)
plot(fcast2)
lines(y)

accuracy(fcast2, outsamp)


# # # Compare forecast 1 and 2 # # #
# # We can use a Diebold-Mariano test to see if these forecasts are significantly different from each other. 
dm.test(residuals(fcast), residuals(fcast2), h=l) #the null hypothesis is that the two methods have the same forecast accuracy. 
dm.test(fcast$mean-outsamp,fcast2$mean-outsamp, h=l) #WE can compare the performance of the models out-of-sample
# We are not able to reject the null, so the forecasts are of equal predictive ability

# # # Combining forecast 1 and 2 # # # 
# Finally let us check if combining these two forecasts will lead to an improvement in terms of RMSE. 

# Nelson combination method
combfitN <- lm(outsamp ~ fcast$mean + fcast2$mean)
summary(combfitN)
#the intercept is significant => there is a bias, we need to correct the data for it
outsampcor<-outsamp-combfitN$coefficients[1] #where combfitN$coefficients[1] picks out the intercept value from the estimated regression
# Now want to run an OLS without an intercept on the corrected (debiased data)
#with respect to a restriction on the weights:  w1 + w2 = 1
fitW <- lm(outsampcor ~ 0+ offset(fcast$mean) + I(fcast2$mean-fcast$mean))
coef_2 <- coef(fitW)
beta_1 <- 1 - coef_2 #the weight is negative, would prefer a different combination method in this case
beta_2 <- coef_2
#beta_1 and beta_2 will give you the weights. 
# Now can use those weights to obtain a combination forecast
combfcastN <-beta_1*fcast$mean+beta_2*fcast2$mean
accuracy(combfcastN, outsamp) #can see that in this case the forecast combination performes worse than the individual forecasts

#plotting the initial data together with the forecast
yf1<-ts(append(insamp,combfcastN), start=1949, frequency = 12)
ts.plot(y, yf1, gpars=list(xlab="Month", lty=c(1:2)))


# Granger-Ramanathan combination method (GR)
combfit <- lm(outsamp ~ fcast$mean + fcast2$mean)
summary(combfit) #the coefficients in the regression will give you the weights
combfcast <- ts(combfit$fitted.values, frequency = 12)
accuracy(combfcast, outsamp)

#plotting the initial data together with the forecast
yf2<-ts(append(insamp,combfcast), start=1949, frequency = 12)
ts.plot(y, yf2, gpars=list(xlab="Month", lty=c(1:2)))


# Equal weights (EW)
combfcastE <- 0.5*fcast$mean + 0.5*fcast2$mean
accuracy(combfcastE,outsamp)

#plotting the initial data together with the forecast combinations: GR and EW
out<-ts(outsamp,frequency=12)
yf3<-ts(combfcastE,frequency = 12)
ts.plot(out,combfcast, yf3, gpars=list(xlab="Month", lty=c(1:3)), col=c(rep("black",1),rep("blue",1),rep("red",1)))
