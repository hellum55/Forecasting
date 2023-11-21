#The packages we will use frequently need to be called before using functions
library(forecast)
library(stats)
library(datasets)

########################################################################


# Importing data from the directory (notice that your directories will be different!)
library(readxl)
data <- read_excel("C:/Users//shampoo_sales.xlsx")
View(data)

#Renaming a variable 

x <- data$Sales


#Plotting the series (this can be done in so many ways, type ??plot in the console to read the help file)
ts.plot(x)

#Correlogram 
acf(x)
pacf(x)

#Plotting all three components together 
library(ggplot2)
tsdisplay(x)


