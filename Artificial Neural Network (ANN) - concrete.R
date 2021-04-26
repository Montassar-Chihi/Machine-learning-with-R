## Exploring and preparing the data
concrete <- read.csv("concrete.csv")
str(concrete)
# Neural networks work best when the input data are scaled to a narrow range around zero
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
# Splitting data into train/test 
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Training a model on the data
#install.packages("neuralnet")
library(neuralnet)
# Train a model with a single hidden layer
concrete_model <- neuralnet(strength ~ cement + slag
                            + ash + water + superplastic + coarseagg + fineagg + age,
                            data = concrete_train)
# Visualize the topology
plot(concrete_model)

## Evaluating model performance
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
# We'll use correlation to measure accuracy
cor(predicted_strength, concrete_test$strength)

## Improving model performance
# Train a model with 5 hidden layers
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)
# Visualize the topology
plot(concrete_model2)
# Evaluate the new model
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
