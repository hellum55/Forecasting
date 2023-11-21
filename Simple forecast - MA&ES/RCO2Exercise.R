#CO2 Exercise (again make sure to change your working directory)
#you are provided with monthly CO2 levels data and asked to analyse it using the techniques covered in class
library(readxl)
library(tibble)
library(forecast)
#importing data
CO2 <- read_excel("C:/Users//Week 45/CO2levels.xlsx")

#Renaming the series and defining the numerical vector as a time-series object using ts() function
yt <- ts(CO2, end=c(2018, 8), frequency=12)  #specifying 12 as frequence, as have monthly data and expect that type of seasonality maybe

plot(yt, type="l", col="black")

#12-period moving average smoothing with "order" specified
yt12 <- ma(yt, order=12, centre=TRUE)
lines(yt12, col="red") #lines() function will add to the existing graph 

#Simple and Holt-Winters exponential smoothing

# simple exponential - models level (alpha will be automatically estimated)
fit1 <- HoltWinters(yt, beta=FALSE, gamma=FALSE)
view(alpha.fit1)
plot(forecast(fit1))
accuracy(forecast(fit1))
fit1[["alpha"]] #will display the alpha value that has been selected and used

# double exponential - models level and trend (alpha and beta will be automatically estimated)
fit2 <- HoltWinters(yt, gamma=FALSE)
plot(forecast(fit2))
accuracy(forecast(fit2))


# triple exponential - models level, trend, and seasonal components (all parameters will be automatically estimated)
fit3 <- HoltWinters(yt)
plot(forecast(fit3))
accuracy(forecast(fit3)) #this model wins in terms of RMSE :)
 





