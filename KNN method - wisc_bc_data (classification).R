##importing the dataset
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
##take a look on the data
str(wbcd)
##remove the id column
wbcd <- wbcd[-1]
##manipulate the feature we want to predict ( the diagnosis result )
#see how many "Benign" and "Malignant" we have
table(wbcd$diagnosis)
#make diagnosis a factor for algorithm purposes
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),
                        labels = c("Benign", "Malignant"))
#take a look on the result
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
##take a look on other features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
##normalize the numeric data 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)
##split into train set and test set
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
#include diagnosis column into the train and test sets
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

library(class)
##perform the KNN algorithm on the data
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)
wbcd_test_pred

library(gmodels)
##Evaluating the result
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
##In order to improve the performance of our model , we'll use Z-score standarisation
wbcd_z <- as.data.frame(scale(wbcd[-1]))
#check the result
summary(wbcd_z$area_mean)
#split into train and test set again
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
#perform the KNN algorithm on the data
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)
#Evaluating the result
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
######### WE GOT WORSE RESULTS AS WE HAVE 95% ACCURACY WHEN WE PREVIOUSLY HAD 98%
