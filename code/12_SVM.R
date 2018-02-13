# Call Libraries
require(e1071)

# Create Random Data
set.seed(2)
x <- matrix(rnorm(40), 20, 2)
y <- rep(c(-1, 1), c(10, 10))
x[y == 1,] <- x[y == 1,] +1
plot(x,col=y+3,pch=19)

# Create Data Frame and Run SVM
dat <- data.frame(x,y=as.factor(y))
svmfit <- svm(y~.,data = dat,kernel="linear",cost=10,scale=F)
print(svmfit)
plot(svmfit,dat) #This is so unbelievably ugly

# Create Grid
make.grid <- function(x,n=75){
  grange=apply(x,2,range)
  x1=seq(from=grange[1,1],to=grange[2,1],length.out =n)
  x2=seq(from=grange[1,2],to=grange[2,2],length.out =n)
  expand.grid(X1=x1,X2=x2)
}
xgrid <- make.grid(x)

# Plot data on new Grid
ygrid <- predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)
         
