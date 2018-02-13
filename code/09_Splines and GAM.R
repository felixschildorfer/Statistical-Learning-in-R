# Call Libraries and attach Data
require(splines2)
require(gam)
require(ISLR)
attach(Wage)

# Plot data and visualize different Sections
fit <- lm(wage~bs(age, knots = c(25, 40, 60)))
plot(age,wage, col = "darkgrey")
lines(age.grid,predict(fit, list(age = age.grid)), col ="darkblue", lwd = 2)
abline(v = c(25,40,60), lty = 2, col = "lightblue")

# Fit Spline Model
fit1 <- smooth.spline(age, wage, df = 16)
lines(fit,col = "red", lwd = 2)

# With Cross Validation
fit2 <- smooth.spline(age, wage,cv = T)
lines(fit, col = "green", lwd = 2)
fit

# Fit GAM
gam1  <- gam(wage~s(age,df=4)+s(year,df=4)+education)
par(mfrow=c(1, 3))
plot(gam1,se=T)
gam2 <- gam(I(wage>250)~ s(age,df=4) + s(year,df=4) + education)
anova(gam1,gam2,test="Chisq")
lm1 <- lm(wage~ns(age,df=4) + ns(year,df=4) + education)
plot.gam(lm1,se=T)
 