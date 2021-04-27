## Exploring and preparing the data
teens <- read.csv("snsdata.csv")
str(teens)
# Check the number of male and female ( and missing values )
table(teens$gender)
table(teens$gender, useNA = "ifany")
# We'll do some cleaning due to some odd observations in data
summary(teens$age)
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)
summary(teens$age)
# Data preparation – dummy coding missing values
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
# To confirm that we did the work correctly, 
# let's compare our constructed dummy variables to the original gender variable
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")
# Data preparation – imputing the missing values
mean(teens$age)
mean(teens$age, na.rm = TRUE) # remove missing values
# We actually need to calculate average for each graduation year
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(teens$age, teens$gradyear, FUN =
                 function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age) # No more missing values

## Training a model on the data
#install.packages("stats")
library(stats)
# We consider 36 features that represent number of times interests appeared on the SNS
interests <- teens[5:40]
# We standardize data
interests_z <- as.data.frame(lapply(interests, scale))
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5) 

## Evaluating model performance
# Check size of each cluster
teen_clusters$size
# We can examine the coordinates of the cluster centroids
teen_clusters$centers
# Given this subset of the interest data, we can already infer some characteristics of the clusters

## Improving model performance
# After adding the cluster column , we can do some analysis and compare each cluster 
# and see how much it relates to the features
teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)





