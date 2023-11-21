#Case 6 p108-111 HW
library(readxl)
library(forecast)
data <- read_excel("C:/Users//Week 45/Orders and CPO.xlsx")
#separating series from the imported data          
orders <- data$Orders
cpo <- data$CPO 

#Renaming the series and defining the numerical vector as a time-series object using ts() function
orders <- ts(orders, end=c(2003, 6), frequency=12)  #specifying 12 as frequency, as have monthly data and expect that type of seasonality maybe
cpo <- ts(cpo, end=c(2003, 6), frequency=12)  #specifying 12 as frequency, as have monthly data and expect that type of seasonality maybe

#Q1
plot(orders, type="l", col="black")
plot(cpo, type="l", col="black")

acf(orders) #the lagsare stated as parts of the year, given the data is monthly
acf(cpo)

#alternatively if you want to change the scaling and labling of the x axis to be in months
mx=6 #max lags to consider in ACF
acf(orders, lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)

#Q2
# simple exponential - models level (alpha will be automatically estimated)
fit1 <- HoltWinters(orders, beta=FALSE, gamma=FALSE)
plot(forecast(fit1))
accuracy(forecast(fit1))

#Holt's
fit2 <- HoltWinters(orders, gamma=FALSE)
plot(forecast(fit2))
accuracy(forecast(fit2))

#Winters' with additive seasonal component
fit3 <- HoltWinters(orders, seasonal =c("additive"))
plot(forecast(fit3))
accuracy(forecast(fit3))

#Winters' with multiplicative seasonal component
fit4 <- HoltWinters(orders, seasonal =c("multiplicative"))
plot(forecast(fit4))
accuracy(forecast(fit4))

#Model 4 shows best fit, so we use it for forecasting
orderF<-forecast(fit4,h =4)
#can then look at the ACF of the residuals
checkresiduals(forecast(fit4))


#Q3
#There is no aparent seasonal component, but maybe a trend
# simple exponential - models level (alpha will be automatically estimated)
fit21 <- HoltWinters(cpo, beta=FALSE, gamma=FALSE)
plot(forecast(fit21))
accuracy(forecast(fit21))
fit21["alpha"] #shows the optimal smoothig parameter value chosen by R

#Holt's
fit22 <- HoltWinters(cpo, gamma=FALSE)
plot(forecast(fit22))
accuracy(forecast(fit22))

cpoF<-forecast(fit21,h =4)
#can then look at the ACF of the residuals
checkresiduals(forecast(fit21))

#Q4
ContactsF<-orderF$mean*cpoF$mean

#Q5
#see pdf

#Q6
#see pdf
