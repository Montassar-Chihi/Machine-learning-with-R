## Exploring and preparing the data
letters <- read.csv("letterdata.csv")
str(letters)
# SVM is for continuous and categorical variable analysis,  
# so we'll have to turn our target variable "letter" to factor
letters$letter<- as.factor(letters$letter)
# Splitting data
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]

## Training a model on the data
#install.packages("kernlab") 
library(kernlab)
# Train the model
letter_classifier <- ksvm(letter ~ . , data = letters_train,
                          kernel = "vanilladot")
letter_classifier

## Evaluating model performance
letter_predictions <- predict(letter_classifier, letters_test)
# confusion matrix
table(letter_predictions, letters_test$letter)
# We'll build a table to identify the correct and false predicitions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## Improving model performance
# We'll try using other kernels
#1- Train Model
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                              kernel = "rbfdot")
#2- Make predicitons
letter_predictions_rbf <- predict(letter_classifier_rbf,
                                  letters_test)
#3- We'll evaluate our model and compare accuracy
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
