# Linear Discriminant Analysis
library(ISLR)
library(MASS)

# Attach and Explore Data
attach(Smarket)
names(Smarket)
summary(Smarket)

# Run Linear Discriminant Analysis
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = Year <2005)
plot(lda.fit)

# Subset and Test Data
Smarket.2005 = subset(Smarket, Year == 2005)
lda.pred = predict(lda.fit, Smarket.2005)
lda.pred[1:5]

# Look At resulst and measure accuracy
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)
