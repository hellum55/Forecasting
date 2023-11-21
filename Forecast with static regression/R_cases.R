library(readxl)
library(tibble)
library(forecast)

################################################################################
##### Case2p222 #####
################################################################################
data <- read_excel("C:/Users//Business Forecasting 2021/Week 46/Case2p222.xlsx")
View(data)

Y <- data$Y
X <- data$X

# Creating a scatter plot with a smooth-line fitting

scatter.smooth(x=X, y=Y, main="Y ~ X") 

# Calculate the correlation between the series

cor(Y, X)

# Building a linear model using lm() function, Replicating the table
linMod <- lm(Y ~ X) #To omit intercept when necessary, the formula can be written for example as lm(cost ~ age - 1)
summary(linMod)

# Checking the residual diagnostic
plot(resid(linMod))
acf(resid(linMod)) # white noise residuals?

library(lmtest)
bptest(linMod) # Breusch-Pagan test H_0: variance is constant.

# If there are several models, to compare them we can use AIC, BIC criteria (choosing the model with min AIC or BIC)
AIC(linMod)  
BIC(linMod)

#Q1
#The 89 degree temperature is 24 degrees off ideal (89 - 65 = 24).  
a <- data.frame(X=24)
result <- predict(linMod, a) 
print(result)

#Q2
#The 41 degree temperature is 24 degrees off ideal (41 - 65 = -24). 
#So the forecast result is the same as in Q1

#Q3
cor(X,Y)
#Since there is a fairly strong relationship between output and deviation from ideal                     
#temperature (r = -.80), higher output may well result from efforts to control the   
#temperature in the work area so that it is close to 65 degrees.   
#Gene should consider ways to do this.


#Q4
#Gene has made a decent start towards finding an effective forecasting tool.  
#However, since about 36% of the variation in output is unexplained, 
#he should look for additional important predictor variables. 





################################################################################
##### Case3p233 #####
################################################################################
data <- read_excel("C:/Users//Week 46/Case3p223.xlsx")
View(data)

Y <- data$Y
X <- data$X
plot(X,Y)

#Q1
# Calculate the correlation between the series
cor(Y, X)
#The correlation coefficient is:  r = .927.  
cor.test(Y,X)
#The corresponding  t = 8.9 for testing has a p value of .000.  
#We reject H0 and conclude the correlation between days absent and employee age holds for the population. 

#Q2
# Building a linear model using lm() function, Replicating the table
linMod <- lm(Y ~ X) #To omit intercept when necessary, the formula can be written for example as lm(cost ~ age - 1)
summary(linMod)

#Q3
#R^2 = 0.8592 =>About 86% of Y's (absent days) variability can be explained through 
# knowledge of X (employee age).

#Q4
#The null hypothesis  is rejected using t = 8.9 (p value = .000).
#There is a significant relation between absent days and employee age.  

#Q5
a <- data.frame(X=24)
result <- predict(linMod, a) 
print(result)

# Checking the residual diagnostic
plot(resid(linMod))
acf(resid(linMod)) # white noise residuals?

library(lmtest)
bptest(linMod) # Breusch-Pagan test H_0: variance is constant.

#Q6
#If time and cost are not factors, it might be helpful to take a larger sample 
#to see if these small sample results hold.  If results hold, a larger sample will 
#very likely produce more precise interval forecasts.  

#Q7
#The fitted function is likely to produce useful forecasts, although 95% 
#prediction intervals can be fairly wide because of the small sample size.    


