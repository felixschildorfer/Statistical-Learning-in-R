# Call Libraries
library(class)

# Attach Data
attach(Smarket)

# Bind and Subset Data
Xlag <- cbind(Lag1, Lag2)
train <- Year < 2005

# Apply K-Nearest Neighbor
knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train] ,k = 1)

# Measure Accuracy
table(knn.pred,Direction[!train])
mean(knn.pred == Direction[!train])
