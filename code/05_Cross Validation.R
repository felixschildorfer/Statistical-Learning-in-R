# Call Libraries
library(ISLR)
library(boot)

# Attach and Plot Data
attach(Auto)
plot(mpg~horsepower)

# Run Linear Regression and Plot it
glm.fit <- glm(mpg ~ horsepower)
abline(glm.fit, col = "red")

# Cross Validate
cv.glm(Auto, glm.fit)$delta

# Compare different Polynomial Models
cv.error=rep(0, 5)
degree=1:10
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d))
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type = "b")

# 10-Fold CV
cv.error10=rep(0,10)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d))
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta
}
lines(degree,cv.error10,type = "b",col = "red")

# Watch Cross validation again

