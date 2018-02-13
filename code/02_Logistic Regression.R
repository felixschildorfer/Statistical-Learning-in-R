# Logistic Regression

# Call Libraries
library(ISLR)

# Attach and Explore Data
attach(Smarket)
names(Smarket)
summary(Smarket)

# Visualize Data
pairs(Smarket, col = Direction)

# Run logistic Regression und Summarize
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 +Lag4 + Lag5 + Volume, family = binomial)
summary(glm.fit)

# Predict Market responses
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

# Look at Results
table(glm.pred, Direction)
mean(glm.pred == Direction)

# Train Data and Predict Responses
train = Year < 2005
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 +Lag4 + Lag5 + Volume, family = binomial, subset = train)
glm.probs <- predict(glm.fit, newdata=Smarket[!train,], type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
Direction.2005 <- Direction[!train]

# Look at Results
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

# Train another Model and Predict Responses
glm.fit = glm(Direction ~ Lag1 + Lag2, family = binomial, subset = train)
glm.probs =predict(glm.fit, newdata=Smarket[!train,],type = "response")
glm.pred = ifelse(glm.probs > 0.5,"Up","Down")
Direction.2005 = Direction[!train]

# Look at Results
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)
