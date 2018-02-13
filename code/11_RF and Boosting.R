# Call Libraries
require(randomForest)
require(gbm)
require(MASS)

# Split Data in train and test
train <- sample(1:nrow(Boston), 300)

# Run and Plot Random Forest
rf.boston <- randomForest(medv~., data = Boston, subset = train)
rf.boston
plot(rf.boston)

# test different Mtry values and plot results
oob.err <- double(13)
test.err <- double(13)
for(mtry in 1:13){
  fit <- randomForest(medv~., data = Boston,subset = train, mtry = mtry, ntree = 500)
  oob.err[mtry] <- fit$mse[500]
  pred <- predict(fit,Boston[-train,])
  test.err[mtry] <- with(Boston[-train,],mean((medv-pred)^2))
  cat(mtry,"  ")
  }
matplot(1:mtry,cbind(test.err,oob.err),pch = 19,col=c("red","blue"),type = "b")

# Gradient Boosting
boost.boston = gbm(medv~., data = Boston[train,], distribution = "gaussian",
                   n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.boston)
plot(boost.boston,i = "lstat")
plot(boost.boston,i = "rm")

# See the effect of number of trees
n.trees <- seq(from = 100, to = 1000, by = 100)
predmat <- predict(boost.boston,newdata = Boston[-train,],n.trees=n.trees)
dim(predmat)
perr <- with(Boston[-train,], apply((predmat-medv)^2, 2, mean))
plot(n.trees,perr, pch = 19)
abline(h = min(test.err), col = "red")
 