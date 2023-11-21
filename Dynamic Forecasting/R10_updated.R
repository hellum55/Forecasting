library(readxl)
library(forecast)
library(tseries)
# We will use the "dynlm" package for ADL analysis: install.packages("dynlm")
library(ARDL)
# We will use the "vars" package for VAR analysis: install.packages("vars")
library(vars)
#########################
###########ADL###########
data <- read_excel("C:/Users/au672494/OneDrive - Aarhus Universitet/Courses/Business Forecasting/Business Forecasting 2022/Week 48/GDP_CO2.xls")
View(data)
x<-ts(data$gdp)
y<-ts(data$co2)

tsdisplay(x)
tsdisplay(y)

dy<-diff(y)
dx<-diff(x)

#Estimate an ADL
model<-auto_ardl(dy ~dx,data=cbind(dx, dy), max_order = 5)
fit<-model$best_model #saves the best model
fit$order #shows the order of the ADL selected
summary(fit)

#alternatively (if want to decide on lag order ourselves)
library(dLagM)
model<-ardlDlm(x=diff(data$gdp),y=diff(data$co2),p=5,q=1) #where p is lags of x and q is lags of y



#########################
#clear the environment
rm(list = ls())
#########################

###########VAR###########
#########################
EuroMacroData <- read_excel("C:/Users/au672494/OneDrive - Aarhus Universitet/Courses/Business Forecasting/Business Forecasting 2022/Week 48/EuroMacroData.xlsx")
View(EuroMacroData)
# Let us take a look at nominal short-, long-term interest rates and exchange rates.

#short-term interest rates
stn <- ts(log(EuroMacroData$STN), frequency = 4)
tsdisplay(stn)

#long-term interest rates
ltn <- ts(log(EuroMacroData$LTN), frequency = 4)
tsdisplay(ltn)

#exchange rates
een <- ts(log(EuroMacroData$EEN), frequency = 4)
tsdisplay(een)

#group data for further analysis
z <- ts(cbind(stn,ltn,een), frequency = 4)

# Taking the first difference to make the series stationary for VAR analysis.
dz<- diff(z)

# # # # # VAR: Estimation # # # # #
#determine the order of VAR
VARselect(dz, lag.max = 10, type="const")[["selection"]] #optimal p =1
var1 <- VAR(dz, p=1, type="const") #estimate VAR(1)
summary(var1)

#Can do model diagnostics as in AR models
#Plot the autocorrelation functions for the VAR residuals
acf(residuals(var1))
#Checking for serial correlation in the errors
serial.test(var1, lags.pt=10, type="PT.asymptotic")
### The null is off no serial correlation

#Checking stability of VAR
var1.stable <- stability(var1, type = "OLS-CUSUM")
plot(var1.stable)
roots(var1)

#Obtaining impulse response functions
plot(irf(var1,boot = TRUE, ci=0.95))


# # # # # VAR: Forecasting Exercise # # # # # 
#split the sample into training and test
insampdz <- ts(dz[1:90, 1:3], frequency = 4)
outsampdz <- dz[91:115, 1:3]

#select the order of VAR
VARselect(insampdz, lag.max = 20, type="const")[["selection"]]

#Fit the model on training data
fit <- VAR(insampdz, p=20, type="const")
summary(fit)
#check the residuals of the model
serial.test(fit, lags.pt=10, type="PT.asymptotic")

#Obtain the forecast for the test period
fcast <- forecast(fit, h=25)
plot(fcast) #provides a plot of the forecasts

#calculate the accuracy of the forecast for the ltn variable
accuracy(fcast$forecast$ltn, outsampdz[1:25, 2])

#Aggregate data to levels
fcast_ltn<-ltn[91]+cumsum(fcast$forecast$ltn$mean)
library(ggplot2)
Date <- seq(1:length(ltn))
ggplot(EuroMacroData, aes(x=Date)) +
  geom_line(aes(y = append(ltn[1:91],fcast_ltn)), color = "red")+    
  geom_line(aes(y = ltn)) +
  scale_y_continuous(name = "")


