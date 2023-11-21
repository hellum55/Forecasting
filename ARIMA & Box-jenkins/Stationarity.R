# # Stationary series examples
library(forecast)

set.seed(1)

St1 <- ts(replicate(n = 1, 
                 arima.sim(model = list(order = c(1, 0, 0), ar=0.3), n = 100)))

matplot(St1, 
        type = "l", 
        col = c("steelblue"), 
        lty = 1, 
        lwd = 2,
        main = "AR(1)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St1)


St2 <- ts(replicate(n = 1, 
                    arima.sim(model = list(order = c(0, 0, 1), ma=0.9), n = 100)))

matplot(St2, 
        type = "l", 
        col = c("steelblue"), 
        lty = 1, 
        lwd = 2,
        main = "MA(1)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St2)


St3 <- ts(replicate(n = 1, 
                    arima.sim(model = list(order = c(1, 0, 1), ar=0.5, ma=0.5), n = 100)))

matplot(St3, 
        type = "l", 
        col = c("steelblue"), 
        lty = 1, 
        lwd = 2,
        main = "ARMA(1,1)",
        xlab = "Time",
        ylab = "Value")

tsdisplay(St3)

# Nonstationary series examples
set.seed(1)

RWs1 <- ts(replicate(n = 1, 
                    arima.sim(model = list(order = c(0, 1 ,0)), n = 100)))

matplot(RWs1, 
        type = "l", 
        col = c("steelblue", "darkred"), 
        lty = 1, 
        lwd = 2,
        main = "Random Walk",
        xlab = "Time",
        ylab = "Value")

tsdisplay(RWs1)


RWs2 <- ts(replicate(n = 1, 
                     arima.sim(model = list(order = c(1, 1 , 1), ar=0.5, ma=0.5), n = 100)))

matplot(RWs2, 
        type = "l", 
        col = c("steelblue", "darkred"), 
        lty = 1, 
        lwd = 2,
        main = "Random Walk",
        xlab = "Time",
        ylab = "Value")

tsdisplay(RWs2)
