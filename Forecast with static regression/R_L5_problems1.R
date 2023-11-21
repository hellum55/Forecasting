library(readxl)
library(tibble)

################################################################################
##### prob5p209 #####
################################################################################
data <- read_excel("C:/Users//Business Forecasting 2021/Week 46/prob5p209.xlsx")
View(data)

mc <- data$`Maintenance cost`
age <- data$Age

#Q.a
plot(age, mc)

#Q.b
# Creating a scatter plot with a smooth-line fitting
abline(lm(mc~age)) #Positive linear relationship

#Q.c
# Calculate the correlation between the series
cor(mc, age)

#Q.d
# Building a linear model using lm() function
linMod <- lm(mc~age)#lm(Y ~ X) #To omit intercept when necessary, the formula can be written for example as lm(mc ~ age - 1)
plot(linMod)
summary(linMod)

# Checking the residual diagnostic
plot(resid(linMod))
acf(resid(linMod)) # white noise residuals?

library(lmtest)
bptest(linMod) # Breusch-Pagan test H_0: variance is constant.


#Q.e
#From summary table: the t value associated with the slope coefficient is t = 7.14 (p-value = .000) 
#=> Reject at the 5% level.  The correlation coefficient is significantly different from 0 since
# the slope coefficient is significantly different from 0.  

#Q.f
# Determining the annual maintenance cost for a 5-year-old bus
a <- data.frame(age=5)
result <- predict(linMod, a) 
print(result)


################################################################################
##### prob11p212 #####
################################################################################
data <- read_excel("C:/Users//Business Forecasting 2021/Week 46/prob11p212.xlsx")
View(data)

Y <- data$`Building permits`
X <- data$`Interest rate`

#Q.a
plot(X, Y)

#Q.b
# Building a linear model using lm() function
linMod <- lm(Y~X) #To omit intercept when necessary, the formula can be written for example as lm(mc ~ age - 1)
#plot(linMod)
summary(linMod)

#Q.c
#Reject at the 5% level since t = -5.18 and it's p value = .001 < .05.

#Q.d
#If interest rate increases by 1%, on average the number of building permits 
#will decrease by 145. 

#Q.e  
#From summary output above, r2 = .793.  

#Q.f
#Interest rate explains about 79% of the variation in number of building permits issued.


