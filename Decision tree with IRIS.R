entropy=function(ni) # ni a vector containing the number of object from each class
  {
  pi=ni/sum(ni)
  sum(-pi*log(pi,base=2))
}

entropy(c(9,5))


GainH=entropy(c(9,5))-(7/14 * entropy(c(3,4)) + 7/14 * entropy(c(6,1)))

GainW=entropy(c(9,5))-(8/14 * entropy(c(6,2)) + 6/14 * entropy(c(3,3)))

#install.packages("C50")
library(C50)
x=sample(1:150,rep=F)
x=sample(x,rep=F)
IRIS=iris[x,]
IRIS
rownames(IRIS)=1:150
IR_TR=IRIS[1:100,-5]
CLASS_TR=IRIS[1:100,5]
IR_TE=IRIS[101:150,-5]
CLASS_TE=IRIS[101:150,5]
m=C5.0(x=IR_TR,y=CLASS_TR,trials=1)
plot(m)
p=predict(object= m,newdata = IR_TE,type = "class" )
p
library(caret)
confusionMatrix(p,CLASS_TE)
