#The packages we will use frequently need to be called before using functions
library(forecast)
library(stats)
library(datasets)
library(readxl)

################################################################################
##### Problem 8 on page 92 of HW, prob8p92HW.xlsx #####
################################################################################
# Importing data from the directory (notice that your directories will be different!)
data <- read_excel("C:/Users//Week 45/prob8p92HW.xlsx")

#viewing the imported data
View(data)

#Renaming the series and defining the numerical vector as a time-series object using ts() function
yt <- ts(data)

#plotting the series
ts.plot(yt)

#5-period moving average smoothing with "order" specified
    #ma command calculates the centered average, have a look at the yt5c variable. 
    #If you want to use it for forecasting keep in mind that the last available 
    #observation of yt5c is the one step ahead forecast value
  yt5c <- ma(yt, order=5, centre=TRUE)

#
k <- 5 #specify the order of the moving average
c <- rep (1/k,k) #remember that simple average is a weighted average with equal weights, 
                #you need to specify weights for the filter command to work
yt5<- filter(yt, c, sides = 1) #sides=1 implies that you are taking average of the data prior to the point, 
                              #if you specify sides = 2 you'll get the same result as with ma command, i.e. centered average.
ts.plot(yt5)
View(yt5)

#In terms of simple moving averages it might be quicker to actually do it in excel :) I've attached a file with that solution too


#Simple and Holt-Winters exponential smoothing
  #alpha is the smoothing parameter, 
  #beta tells you if you should account for a trend or not, 
  #gamma is responsible for the presence of a seasonal component in the model

# simple exponential - models level (alpha=0.4), no trend, no seasonal
fit <- HoltWinters(yt, alpha=0.4, beta=FALSE, gamma=FALSE)
#plot the forecast
plot(forecast(fit))
#produce forecast accuracy measures
accuracy(forecast(fit))
#display the fitted values
fitted(fit)


# simple exponential - models level (alpha=0.6)
fit2 <- HoltWinters(yt, alpha=0.6, beta=FALSE, gamma=FALSE)
plot(forecast(fit2))
accuracy(forecast(fit2)) # in this case the MSE is lower than for the "fit" model, 
                        #so we would prefer a higher smoothing parameter in this case


# Holt's exponential - models level (alpha=0.6)
  #exponential smoothing when a trend component is present: beta= TRUE
fit3 <- HoltWinters(yt, alpha=0.6, beta=TRUE, gamma=FALSE)
plot(forecast(fit3))
accuracy(forecast(fit3)) #the MSE is even lower :)


# Winters' exponential - models level (alpha=0.6), trend: beta=TRUE, seasonal component: gamma=TRUE
    #in order to make it work you need to define the frequency of your seasonal 
    #component, when specifying the ts data
yt <- ts(data, frequency = 4) # let's assume we suspect a quarterly pattern, 
                              # could do 12 if suspect monthly
fit4 <- HoltWinters(yt, alpha=0.6, beta=TRUE, gamma=TRUE)
plot(forecast(fit4))
accuracy(forecast(fit4)) # experiment with seasonality frequency to see if you can get any lower in MSE





################################################################################
##### Problem 9-10 on page 93 of HW #####
################################################################################
# Importing data from the directory (notice that your directories will be different!)
data<-read_excel("C:/Users//Week 45/prob9-10p93HW.xlsx")
View(data)

#Renaming the series and defining the numerical vector as a time-series object using ts() function
yt <- ts(data)
ts.plot(yt)

#3-period moving average smoothing 
k <- 3 #order of the moving average
c <- rep (1/k,k) #equal weights
yt3<- filter(yt, c, sides = 1)
ts.plot(yt3)

variables <- data.frame(yt, yt3)
View(variables)  #the last estimated value of yt3 is your January forecast

library(data.table)
y3<- shift(yt3,n=-1, type="lead") #shifting the estimated averages to match the forecasting variable

variables <- data.frame(variables, y3) #grouping variables to view them together
View(variables)

# getting accuracy measures: comparing the forecasted variable (correctly shifted) and the actual data
accuracy(y3,yt)

#5-period moving average smoothing 
k <- 5 #order of the moving average
c <- rep (1/k,k) #equal weights
yt5<- filter(yt, c, sides = 1)
y5<- shift(yt5,n=-1, type="lead") #shifting the estimated averages to match the forecasting variable
View(y5)
# getting accuracy measures: comparing the forecasted variable (correctly shifted) and the actual data
accuracy(y5,yt)

## MA-3 has a lower RMSE



# simple exponential - models level (alpha=0.2)
fit <- HoltWinters(yt, alpha=0.2, beta=FALSE, gamma=FALSE, l.start=9.29) 
#l.start is used to specify the starting value if not to be chosen automatically
plot(forecast(fit))
accuracy(forecast(fit)) #MA-3 still seems to perform better, 
#the forecasted values are very similar to MA-5
fitted(fit)



################################################################################
#Additional code
#Forecasting 10 periods ahead with MA of order 3
k<-3
#starting point is your actual data, replace yt with your variable of interest name
l<-length(yt) #number of available data points
#adding extra rows, as many as periods ahead want to forecast
h <-10
y <- yt 

########################################
#generating space to save the forecasts
for (i in 1:h){
  y <- c(y, 0)
}
#calculating the forecast values
for (j in (l+1):(l+h) ){
  a<-j-k
  b<-j-1
  x <-y[a:b]
  y[j] <- mean(x)
}
#########################################
View(y)

