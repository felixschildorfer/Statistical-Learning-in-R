# Call Libraries
library(ISLR)
library(leaps)
library(glmnet)


# Attach and look at Data
attach(Hitters)
summary(Hitters)

# Get rid of NA values
Hitters <- na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))

# Best Subset Regression
regfit.full <- regsubsets(Salary~., data = Hitters,nvmax = 19)
reg.summary <- summary(regfit.full)
plot(regfit.full,scale = "Cp")
coef(regfit.full, 10)

# Do Forward Selection
regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
plot(regfit.fwd, scale = "Cp")

dim(Hitters)
set.seed(1)
train=sample(seq(263),180,replace = F)
train
regfit.fwd=regsubsets(Salary~.,data=Hitters[train,],nvmax = 19, method = "forward")

val.errors=rep(NA,19)
x.test = model.matrix(Salary~.,data = Hitters[-train,])
for (i  in 1:19) {
  coefi= coef(regfit.fwd,id=i)
  pred=x.test[,names(coefi)]%*%coefi
  val.errors[i]=mean(Hitters$Salary[-train]-pred^2)
}
plot(sqrt(val.errors),ylab = "Root MSE", ylim = c(300,400), pch = 19, type = "b")
points(sqrt(regfit.fwd$rss[-1]/180),col="red",pch=19,type = "b")
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}


