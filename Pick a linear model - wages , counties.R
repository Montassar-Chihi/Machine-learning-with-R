library(leaps)
library(ggplot2)
wages=read.csv("wages.csv")

# Create all possible models 
subs=regsubsets(earn ~ . , data= wages)

# This is done to select the best model according to each metric
summary(subs) #see best seven models 
summary(subs)$adjr2 #see the adjusted R² of each of the 7 models
summary(subs)$bic #see the BIC of each of the 7 models
summary(subs)$cp #see the CP of each of the 7 models

# It is easier to use this method in order to select the best model 
df=data.frame(
  est=c(summary(subs)$cp,summary(subs)$adjr2,summary(subs)$bic),
  x=rep(1:7 , 3),
  type=rep(c("cp","adjr2","bic"), each=7)
)
qplot(x,est,data=df,geom="line")+theme_bw()+facet_grid(type~., scales="free_y")

# We'll use the same method on a dataset with more variables
cts=read.csv("counties.csv")
sub=regsubsets(crime ~ . , data= cts)
# It does not work because there is too much variables
# So , we will use the stepwise method
start_mod=lm(crime ~ pop, data=cts)
empty_mod=lm(crime ~ 1, data=cts)
full_mod=lm(crime ~ . , data=cts)
step(start_mod,scope=list(upper=full_mod , lower=empty_mod),direction="forward")
