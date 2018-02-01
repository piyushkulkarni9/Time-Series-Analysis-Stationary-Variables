# -----------------------------
#-------- Q 9.2 ---------------
#------------------------------
qt(0.975,101)
qt(0.95,101)
qt(0.90,101)

sqrt(1.3946+2.1606-(2*1.0406))

5.644-(1.984*1.2140)
sqrt(1.3946+2.1606+1.4214-(2*1.0406)+(2*.0984)-(2*1.0367))
7.909-(1.984*1.009)

# *******************
# ***** Q 9.4 *******
# *******************

(-.31*.28)+(.09*.31)-(.03*.09)-(.37*.03)+(.17*.37)+(.39*.17)+(.03*.39)-(.03*.03)+(1.02*.03)

c <- c(0.28, -0.31,-0.09, 0.03, -0.37, -0.17, -0.39, -0.03, 0.03, 1.02)
View(c)
d <- c^2
View(d)
sum(d)
0.0979/1.5436

(-.09*.28)-(.03*.31)+(.37*.09)-(.17*.03)+(.39*.37)+(.03*.17)-(.03*.39)-(1.02*.03)

.1008/1.5436

qnorm(.025)
qnorm(1-.025)

sqrt(10)*0.0653

1.959/sqrt(10)

# ****************************
# ********* Q 9.6*************
#******************************

qt(.975, 216)
qt(.975,215)

-53.51 - (1.971*16.98)
.3306/.0649

-58.61 - (1.971*14.10)

# ******************************
#************ Q 9.22 ***********
# ******************************
library(haven)
consumptn <- read_dta("D:/Class Notes/Fall 17 Classes/ECON/Data_sets/consumptn.dta")
View(consumptn)

incgwth<- ts((consumptn$incgwth))

congwth <- ts((consumptn$congwth))

consumptionts <- cbind(congwth, incgwth)


plot(consumptionts)
ts.plot(congwth, incgwth, col = c("blue", "red"), lty = c(1:2))

ts.plot(congwth, col = "blue")
abline(h = mean(congwth), col = "red", lty = 2)

ts.plot(incgwth, col = "violet")
abline(h = mean(incgwth), col = "red", lty = 2)



lag1congwth <- lag(congwth, -1)
lag1incgwth <- lag(incgwth, -1)
lag2congwth <- lag(congwth, -2)
lag2incgwth <- lag(incgwth, -2)

data <- cbind(consumptionts, lag1congwth, lag2congwth, lag1incgwth, lag2congwth)
View(data)

data2 <- data[-c(1,2,3), ]

data3 <- as.data.frame(data2)

consumption2 <- consumptn[-c(1,2,3), ]

nrow(consumption2)

incgwth2<- ts((consumption2$incgwth))

congwth2 <- ts((consumption2$congwth))

consumptionTS2 <- cbind(congwth2, incgwth2)

# **************************************
# ** Model 1 ***********************
#*************************************
model1 <- lm(congwth2 ~ incgwth2)
summary(model1)
acf(resid(model))
anova(model1)
log(81.94/197) + (4/197)

log(81.94/197) + 2*(log(197)/197)

library(spdep)
res <- lm.LMtests(model, nb2listw(a) ,test = "LMlag")
View(model)

#library(lmtest)
#library(orcutt)
#dwtest(model$residuals)
#bptest(model)
#dwtest(model)

#dwtest()

#model <- lm(congwth2 ~ diff(congwth) + incgwth2)


# ***************************************************
# ********* Adding Lag 1 gor congwth *****************
# *****************************************************
fit <- lm(consumptionts.congwth ~ lag1congwth + consumptionts.incgwth, data = data3)
summary(fit)
anova(fit)

log(84.316/197) +(6/197)

log(84.316/197) + (3 * log(197)/ 197)

acf(resid(fit))


# ***************************************************
# ********* Adding Lag 1 and lag 2 for congwth *****************
# *****************************************************

fit2 <- lm(consumptionts.congwth ~ lag1congwth + lag2congwth  +  consumptionts.incgwth, data = data3)
summary(fit2)
anova(fit2)

log(76.098/197) +(8/197)

log(76.098/197) + (4 * log(197)/ 197)

acf(resid(fit2))


# ***************************************************
# Adding Lag 1 and lag 2 for congwth  and lag 1 for inc gwth
#*****************************************************

fit3 <- lm(consumptionts.congwth ~ lag1congwth + lag2congwth  + consumptionts.incgwth + lag1incgwth, data = data3)
summary(fit3)
anova(fit3)

log(69.334/197) +(10/197)

log(69.334/197) + (5 * log(197)/ 197)
sum((residuals(fit3)^2))

acf(resid(fit3))

# ***********************************
# Adding t-3 and t-2 ***************
# ********************************

lag3congwth <- lag(congwth, -3)

data5 <- cbind(consumptionts, lag1congwth, lag2congwth, lag3congwth, lag1incgwth, lag2incgwth)
View(data5)

data5 <- data5[-c(1,2,3,4), ]

data5 <- as.data.frame(data5)
data5 <- data5[-c(197,198,199),] 

fit4 <- lm(consumptionts.congwth ~ lag1congwth + lag2congwth + lag3congwth +
             consumptionts.incgwth + lag1incgwth + lag2incgwth, data = data5)
summary(fit4)
acf(resid(fit4))
anova(fit4)

# ***************************************************
# Droppping  Lag 1  for congwth  and lag 1 for inc gwth
#*****************************************************

fit5 <- lm(consumptionts.congwth ~ lag2congwth  + consumptionts.incgwth + lag1incgwth, data = data3)
summary(fit5)
anova(fit5)

log(69.374/197) +(8/197)

log(69.334/197) + (4 * log(197)/ 197)
sum((residuals(fit3)^2))

acf(resid(fit5))

