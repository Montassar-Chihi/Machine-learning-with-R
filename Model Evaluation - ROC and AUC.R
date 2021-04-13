#install.packages('aod')
#install.packages('ggplot2')
library(aod)
library(ggplot2)

binary <- read.csv("binary.csv")
str(binary)

#Logistic Regression Model
install.packages("nnet")
library(nnet)

mymodel <- multinom(admit~.,data = binary)

#mis classification rate
p <- predict(mymodel,binary)
tab <- table(p,binary$admit)
tab
1-sum(253,29)/400

# Model Performance Evaluation
install.packages("ROCR")
library(ROCR)
pred <- predict(mymodel,binary,type = "prob")
hist(pred)
pred <- prediction(pred,binary$admit)
eval <- performance(pred,"acc")
plot(eval)

abline(h=0.71,v=.45)

#Identifying the best cutoff  and Accuracy
eval
max <- which.max(slot(eval,"y.values")[[1]])
acc <- slot(eval,"y.values")[[1]][[max]]
cut <- slot(eval,"x.values")[[1]][[max]]
print(c(Accuracy=acc,Cutoff = cut))

#Receiver Operating Chraracteristic (ROC) curve
pred <- prediction(pred,binary$admit)
roc <- performance(pred,"tpr","fpr")
plot(roc,colorize = T,
     main = "ROC Curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0,b=1)

#AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc,"y.values"))
round(auc,3)

legend(0.6,0.2,auc,title = "AUC",cex = .50)