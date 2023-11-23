library(forecast)
library(readxl)

# Q1:####
data<- read_excel("~/Business Forecasting/ARIMA & Box-jenkins/ARSales.xlsx")
y <-ts(data$Sales,end = c(2006, 12), frequency=12)

plot(y)

# Q2:####
decompSalesAdd <- decompose(y, type = "additive") #for multiplicative decomposition
plot(decompSalesAdd)

decompSalesMult <- decompose(y, type = "multiplicative") #for multiplicative decomposition
plot(decompSalesMult)
#create separate variables for each of the estimated components
Tr<-decompSalesMult$trend
S<-decompSalesMult$seasonal
I<-decompSalesMult$random # These are really the residuals!

# Q3:####
acf(y)
pacf(y)

# Q4:####
T<-length(y[1:76])
insamp<-y[1:T]
outsamp<- y[77:length(y)]
#Make it a TS:
insamp<-ts(y[1:T],end=T, frequency=12)
outsamp<- ts(na.omit(y[T+1:length(y)]),end=length(y),frequency=12)

tsdisplay(insamp)

fit0 <- auto.arima(insamp, seasonal=TRUE) #automatically select p,d and q for ARIMA
summary(fit0)
tsdisplay(residuals(fit0), main='Model Residuals')

#Q5: ####
fcast1 <- forecast(fit0, h=length(outsamp))
#plot the forecast together with confidence intervals
plot(fcast1)

#plot data together with the original pseudo out-of-sample
f1<-ts(fcast1$mean, end=length(y), frequency = 12)
plot(f1, col ='blue')
lines(outsamp)
#Testing the accuracy insamp/outsamp
accuracy(fcast1$mean, outsamp)
View(data.frame(fcast1$mean,outsamp)) 


