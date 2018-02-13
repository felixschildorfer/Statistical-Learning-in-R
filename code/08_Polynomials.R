# Call library and attach Data
library(ISLR)
attach(Wage)

# Run Non-Linear Regression
fit <- lm(wage ~ poly(age,4))
summary(fit) 

# Plot Data and compare different Polynomial Regressions
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds=predict(fit, newdata <- list(age=age.grid),se = T)
se.bands <- cbind(preds$fit + 2*preds$se.fit,preds$fit -2*preds$se.fit)
plot(age,wage, col = "darkgrey")
lines(age.grid,preds$fit, lwd = 2, col = "blue")
matlines(age.grid,se.bands,col = "blue",lty=2)

fita <- lm(wage~age+I(age^2)+I(age^3)+I(age^4))
summary(fita)
plot(fitted(fit), fitted(fita))

fita <- lm(wage~education)
fitb <- lm(wage~education + age)
fitc <- lm(wage~education + poly(age,2))
fitd <- lm(wage~education + poly(age,3))
anova(fita,fitb,fitc,fitd)

fit <- glm(I(wage>250) ~ poly(age,3),family = binomial)
summary(fit)
preds=predict(fit,list(age=age.grid),se=T)
se.bands=preds$fit + cbind(fit=0,lower=-2*preds$se.fit,upper=2*preds$se.fit)
se.bands[1:5,]


prob.bands <- exp((se.bands)/(1+exp(se.bands)))
matplot(age.grid,prob.bands,col="blue",lwd=c(2,1,1),lty=c(1,2,2),type="l",ylim=c(0,1))               

