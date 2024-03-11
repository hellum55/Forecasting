library(forecast)
library(readxl)

# Q1:####
data<- read_excel("~/Business Forecasting/ARIMA & Box-jenkins/ARSales.xlsx")
y <-ts(data$Sales,start = 1, frequency=12)

plot(y)
#The plot is multiplicative because the variance is increasing. Make a linear reg to see it
# Q2:####
decompSalesAdd <- decompose(y, type = "additive") #for multiplicative decomposition
plot(decompSalesAdd)

decompSalesMult <- decompose(y, type = "multiplicative") #for multiplicative decomposition
plot(decompSalesMult)
#A clear upward trend. Clear seasonal spikes. Random is the unexplained part. Also the residual. 
#Quite constant so the decomp captures the most of the explonation. Constant noise.

#create separate variables for each of the estimated components
Tr<-decompSalesMult$trend
S<-decompSalesMult$seasonal
I<-decompSalesMult$random # These are really the residuals!

# Q3:####
acf(y)
pacf(y)
#A strong autocorrelation (ACF). Seasonality in the acf between each year. 
#Lag 13 is way out of the conf. levels. It seems odd that lag 13 is way out. 
#Its a quick and a fast die out, but no clear model to pick.
#

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
lines(ts(y[77:96], start = 1, frequency = 12))

#plot data together with the original pseudo out-of-sample
f1<-ts(fcast1$mean, end=length(y), frequency = 12)
plot(f1, col ='blue')
lines(outsamp)fghcghfhgfhg
#Testing the accuracy insamp/outsamp
accuracy(fcast1$mean, outsamp)
View(data.frame(fcast1$mean,outsamp)) 

#With trend and seasonality is would fit nicely with Holters:


