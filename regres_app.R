rm(list = ls())
library(MASS)
library(ISLR)
library(readxl)
library(robustbase)
library(pls)

# Advertising  Data (James et al.)
Advertising.data <- read.csv("~/Desktop/MASAÜSTÜ - ORTAK/ÇALIŞMA ALANI/DERSLER/05 BÖLÜM LİSANS DERSLERİ/İST333 REGRESSION/DATA SETS/Advertising.csv")
Advertising <- Advertising.data[,-1]


str(Advertising)
attach(Advertising)
plot(Advertising)
cor(Advertising)

# Full Model
res.fullmodel <- lm (sales~., data = Advertising)
summary(res.fullmodel)


#Model Selection

# Model 0
model0 <- lm(sales~1, data = Advertising)
summary(model0)

# Model 1
model1 <- lm(sales~TV, data = Advertising)
summary(model1)

# Model 2
model2 <- lm(sales~TV+radio, data = Advertising)
summary(model2)

# Model 3
model3 <- lm(sales~., data = Advertising)
summary(model3)

anova(model3, model2, model1, model0)

#Model Selection

# step() function
model3.lmstep.backward <- step(model3)
summary(model3.lmstep.backward)
model3.lmstep.backward$anova

#######################
#######################
#######################

# Confidence and Prediction Intervals
# Rocket Propellant  Data (Montgomery et al.)
y <- c(2158.70, 1678.15, 2316.00, 2061.30, 2207.50, 1708.30, 1784.70, 2575.00, 2357.90, 2256.70, 2165.20, 2399.55, 1779.80, 2336.75, 1765.30, 2053.50, 2414.40, 2200.50, 2654.20, 1753.70)
x <- c(15.50, 23.75, 8.00, 17.00, 5.50, 19.00, 24.00, 2.50, 7.50, 11.00, 13.00, 3.75, 25.00, 9.75, 22.00, 18.00, 6.00, 12.50, 2.00, 21.50)

df <- data.frame(x,y)
plot(x,y)
res.lm <- lm(y~x, data = df)
new <- data.frame(x = seq(3,24, 3))
predict(res.lm, new, se.fit = TRUE)
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence") # Ortalama Yanıtiçin GA
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction") # Yeni Gözlem için GA

matplot(new, cbind(pred.w.clim, pred.w.plim[,-1]),col = c(1,2,2,4,4),lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
legend("topright", c("confidence","prediction"), lty=c(2,4), col = c(2,4), cex =0.5)

#######################
#######################
#######################

# Outliers
data("starsCYG")
plot(starsCYG)
identify(starsCYG)

# LS
res.stars <- lm(log.light ~ log.Te, data = starsCYG)
abline(res.stars, col = 2)

# LS-
res.stars.r0 <- update(lm(log.light ~ log.Te, data = starsCYG[-c(11,20,30,34),]))
abline(res.stars.r0, col = 3)

# MM
res.stars.rob <- lmrob(log.light ~ log.Te, data = starsCYG)
abline(res.stars.rob, col = 4)
legend("bottomleft", c("LS", "LS-", "MM"), lty=c(1,1), col = c(2,3,4), cex =0.5)

summary(res.stars)
summary(res.stars.rob)


#######################
#######################
#######################

# High-Dimensional Data
data("gasoline")
attach(gasoline)
str(gasoline)


