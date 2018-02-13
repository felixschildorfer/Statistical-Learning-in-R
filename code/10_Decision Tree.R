# Call Libraries and Attach Data
require(ISLR)
require(tree)
attach(Carseats)

# Visualize Sales
hist(Sales)

# Create New Dataframe with Custom Variable
High <- ifelse(Sales <= 8, "Yes", "No")
Carseats <- data.frame(Carseats, High)             


# Create and examining tree-model
tree.carseats <- tree(High~.-Sales, data = Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats)


# Train and Predict New Variable using Treemodel
set.seed(3)
train <- sample(1:nrow(Carseats), 300)
tree.carseats <- tree(High~.-Sales,Carseats,subset = train)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.pre <- predict(tree.carseats, Carseats[-train], type = "class")
with(Carseats[-train], table(tree.pre,High))

# Cross Validate Model
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats
plot(cv.carseats)

# Prune Tree and Check Results
prune.carseats = prune.misclass(tree.carseats, best = 13)
plot(prune.carseats)
text(prune.carseats)
prune.pre <- predict(prune.carseats, Carseats[-train], type = "class")
with(Carseats[-train], table(prune.pre, High))
