## Exploring and preparing the data
wine <- read.csv("whitewines.csv")
str(wine)
hist(wine$quality)
# Splitting the data 
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Training a model on the data
#install.packages("rpart")
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)
# Visualizing decision trees
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
# Visualizing decision trees with more details
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)
## Evaluating model performance
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)
#Measuring performance with the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(p.rpart, wine_test$quality)
mean(wine_train$quality)
MAE(mean(wine_train$quality), wine_test$quality)