## Exploring and preparing the data
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
# Target variable
summary(insurance$expenses)
hist(insurance$expenses)
# Exploring relationships among features – the correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])
# Visualizing relationships among features – the scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])
# A better visualization 
#install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

##  Training a model on the data
ins_model <- lm(expenses ~ ., data = insurance)
ins_model 

## Evaluating model performance
summary(ins_model)

## Improving model performance
# adding non-linear relationships
insurance$age2 <- insurance$age^2
# converting a numeric variable to a binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
# Putting it all together – an improved regression model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
summary(ins_model2)



