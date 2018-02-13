# Call Libraries
library(ISLR)
library(boot)

# Attach Data
attach(Portfolio)

# Create Alpha Function and Calcualte Alpha of Portfolio
alpha <- function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy - cxy)/(vx+vy - 2*cxy)
}

alpha(X, Y)

alpha.fn <- function(data,index){
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:100)

# Set Seed
set.seed(579)

# Use Bootstrap and Plot Results
boot.out = boot(Portfolio, alpha.fn, R = 1000)
boot.out
plot(boot.out)

