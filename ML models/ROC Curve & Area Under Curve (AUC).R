#https://youtu.be/ypO1DPEKYFo?list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1
#Preparre data
data <- read.csv("G:/R Programming/ML/binary.csv")
data

#Logistic Regresion Model
library(nnet)
mymodel <- multinom(admit~., data)


#Misclassification
p<- predict(mymodel, data)
tab <- table(p, data$admit)

tab #Confusion matrix

#accuracy of the model 
sum(diag(tab))/sum(tab)

# error 
1-sum(diag(tab))/sum(tab)

table(data$admit)
#0   1 
#273 127 
# 273/400= 0.6825
# the overall accuracy is 68.25%

#accuracy of the model 
sum(diag(tab))/sum(tab)
# 70.5%
# 70.8 >68.25 # so it is slightly better model
# if the overall acuracy of the model is less than 68% then we should not use the model.


#install.packages("ROCR")
library(ROCR)
pred <- predict(mymodel,data, type="prob") #evaluating probability
head(pred)
head(data)

hist(pred) # here most of the data is below 0.4 

pred <- prediction(pred,data$admit)
eval <- performance(pred,"acc")
eval
plot(eval)
abline(h=0.71,v=0.45)


#identify the best values
max<-which.max(slot(eval, "y.values")[[1]])
max
acc <- slot(eval, "y.values")[[1]][max]
acc # highest value
cut <- slot(eval, "x.values")[[1]][max]
cut # max x values
print(c(Accuracy=acc,Cutoff=cut))


#Receiver Operating Characteristics(ROC) Curve
tab 
roc <- performance( pred , "tpr", "fpr") #true positive rate 

performance( pred , "fpr") #false positive rate 

# See the video
#calculation for the "tpr"
#> tab

#p     0   1
#0 253  98
#1  20  29

# 29/(29+98)
# 20/(253+20)


#Roc Curve
plot(roc, colorize=TRUE, main="ROC Curve", ylab="Sensitivity", xlab="1-Specificity")
abline(a=0, b=1)


#Area Under Curve (AUC)
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc <- round(auc,4)
legend(.6,0.5, auc, title = "AUC",cex=1.1)
