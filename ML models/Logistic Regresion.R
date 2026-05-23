#https://youtu.be/AVx7Wc1CQ7Y?list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1
mydata <- read.csv(file.choose()) #binary.csv
mydata <- mydaata

str(mydata)

mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)

#Two way table variable for factor variables
xtabs(~admit+rank, data=mydata)

set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8,0.2))

train <- mydata[ind==1,]
test <- mydata[ind==2,]

#logistic regression model
#General Linear model(glm) 
mymodel <- glm(admit ~ gre+gpa+rank, data = train, family = "binomial")
summary(mymodel)


#Prediction
p1 <- predict(mymodel, train, type="response")
head(p1)
head(train)

#see the youtube video to see the calculations



#interpretation of coefficients
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(pred1,train$admit)
tab1 #confusion matrix


misclassification  <- (1-sum(diag(tab1)/sum(tab1)))
misclassification 




#Prediction
p2 <- predict(mymodel, test, type="response")
head(p2)
head(test)

#see the youtube video to see the calculations



#interpretation of coefficients
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted= pred2,Acutual=test$admit)
tab2 #confusion matrix
misclassification  <- (1-sum(diag(tab2)/sum(tab2)))
misclassification 


#Goodneess of Fit (means P value)
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail=F))


