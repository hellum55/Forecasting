library(readxl)
library(forecast)
library(tseries)
####################
#### Diary data ####
dairydata  <- read_excel("C:/Users/dairydata.xls")

# # Unit root testing example
y <- ts(log(dairydata$emp)) #Logarithm of employment series. 
#We take log mostly because we want to smooth out the effect of extraordinary observations
tsdisplay(y)

#ADF test for unit root
adf.test(y) #The null is that series contain a unit root, i,e, non-stationary
#Cannot reject the null.So let's look at the first differences
dy <- diff(y)
tsdisplay(dy)
# Let us conduct an ADF test in first differences now.
adf.test(dy) #Rejecting the null, so can claim stationarity of the differenced series


# # Cointegration testing example.
#Consider an explanatory variable
x <- ts(log(dairydata$prodh)) #production worker hours
#We have checked the stationarity properties of y, now need to do the same for the x variable
adf.test(x)
adf.test(diff(x)) #x is also non-stationary in levels, but stationary in first differences.
#Now can proceed to checking for cointegration between the two.

# Combining the two vectors x and y 
z <- ts(cbind(x,y))

#Phillips-Ouliaris test (2-step Engle-Granger test), null hypothesis: no cointegration
po.test(z) 
# we reject the null, so can say that the series are cointegrated, meaning that there is a long-term relationship between the variables. 
# So now we can run a regression on the level variables (don't need to take the first differences) and use the regression for forecasting. 

#Manually performing the cointegration test. 
fit <- lm(y~x) #Running an OLS of y on x
adf.test(resid(fit)) #Testing if the residuals from the estimated model contain a unit root.
# Keep in mind that in this case we would need different critical values (ones for the EG test).
