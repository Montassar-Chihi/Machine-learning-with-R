# Importing our data
credit <- read.csv("credit.csv")
## Exploring and preparing data
str(credit)
#exploring categorical variables
table(credit$checking_balance)
table(credit$savings_balance)
table(credit$default)
#exploring numerical variables
summary(credit$months_loan_duration)
summary(credit$amount)
## Creating random training and test datasets
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
#making sure the proportions are right and data is well devided between train and test
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
## Training a model on the data
# install.packages("C50")
library(C50)
#The 17th column in credit_train is the default class variable, 
# so we need to exclude it from the training data frame, 
# but supply it as the target FACTOR vector for classification
credit_train$default<-as.factor(credit_train$default)
#creating our decision tree ("trials=" idicates the number of itterations we want it to do
#in order to boost its accuracy)
credit_model <- C5.0(x=credit_train[-17],y= credit_train$default , trials=20)
#take a look at it
credit_model # you can use plot() function to actually see the tree
summary(credit_model) 
## Evaluating model performance
credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

