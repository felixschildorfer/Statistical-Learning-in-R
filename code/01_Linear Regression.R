# Simple Regression

# Load Libraries
library(MASS)
library(ISLR)

# Attach and Explore Data
attach(Boston)
names(Boston)
plot(medv ~ lstat)

# Run OLS Linear Regresion
fit_1 <- lm(medv ~ lstat)
summary(fit_1)
abline(fit_1, col = "red")

# Look at Confidence Interval 
confint(fit_1)
predict(fit_1, data.frame(lstat= c(5, 10, 13)), interval = "confidence")

# Add Variables
fit_2 <- lm(medv ~ lstat + age)
summary(fit_2)
abline(fit_2, col = "blue")

# Use all Variables
fit_3 <- lm(medv ~.)
summary(fit_3)
par(mfrow= c(2, 2))
plot(fit_3)

# Remove insignificant Variables
fit_4 <- update(fit_3,~.-age-indus)
summary(fit_4)

# Combine lstat and age Variable
fit_5 <- lm(medv~lstat*age)
summary(fit_5)

# Non-Linear Regression and Fit
fit_6 <- lm(medv ~ lstat + I(lstat^2))
summary(fit_6)
par(mfrow= c(1, 1))
plot(medv~lstat)
points(lstat,fitted(fit_6),col = "red", pch= 20)

# Faster Method for Non-Linear Regression
fit_7 = lm(medv~poly(lstat, 4))
points(lstat,fitted(fit_7), col = "blue", pch = 20)

# Define function for faster deployment
regplot <- function(x,y,...){
  fit = lm(x~y)
  plot(x,y,...)
  abline(fit,col="red")
}

# Try out new function
regplot(medv,lstat,col="blue",pch=20)
