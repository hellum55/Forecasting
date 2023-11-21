#The packages we will use frequently need to be called before using functions
#BLABLABALBALA
library(forecast)
library(stats)
library(datasets)
library(readxl)
library(ggplot2)
library(tseries)
################################################################################
##### Stock market returns #####
################################################################################
# Importing data from the directory (notice that your directories will be different!)
data <- read_excel("C:/Users//Returns.xls")

#viewing the imported data
View(data)

#Renaming the series, defining the numerical vector as a time-series object using ts() function
r <- ts(data$RETURN,start=1950:1, frequency = 12)

#plotting the series
ts.plot(r)


#Split the sample prior to analysis
# In-sample (75%) and out-of-sample (25%) split
insamp <- ts(r[1:198], frequency = 12)
outsamp <- ts(r[199:264], end=2015:4, frequency=12)
l<-length(outsamp) #generate a number called "l" equal to the length of the test set


# # # FORECAST 1 # # #
# # Generating the first forecast based on the historical mean
fcst1 <- rep(mean(insamp),l)
accuracy(fcst1, outsamp)
res1<-outsamp-fcst1

# # In-sample fit
fit1 <- rep(mean(insamp),198)
accuracy(fit1,insamp)

# # # FORECAST 2 # # #
# #  Generating the second forecast based on the last observed value
fcst2<-ts(r[198:263], end=2015:4, frequency=12)
accuracy(fcst2, outsamp)
res2<-outsamp-fcst2


# # # Compare forecast 1 and 2 # # #
# # We can use a Diebold-Mariano test to see if these forecasts are significantly different from each other. 
#the null hypothesis is that the two methods have the same forecast accuracy. 
dm.test(res1, res2)
# We are able to reject the null at the 5% level of significance,
# so the forecasts are of different predictive ability
