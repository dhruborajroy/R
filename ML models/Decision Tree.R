#read data
data <- read.csv("https://raw.githubusercontent.com/bkrai/Statistical-Modeling-and-Graphs-with-R/main/Cardiotocographic.csv")
data

str(data) #structure of the data 

data$NSPF <- as.factor(data$NSP)



#data partition
set.seed(1234)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))

data
train <- data[pd==1,]
validate <- data[pd==2,]


#decision treee using party package
#install.packages("party")

library(party)
tree <- ctree(NSPF~LB+AC+FM, data=train, controls = ctree_control(mincriterion = 0.99, minsplit = 500))
#NSPF~LB+AC+FM means NSPF is related to these variables ~LB+AC+FM
tree
plot(tree)


predict(tree, validate, type="prob") 
#will show the probability of 1,2,3
predict(tree, validate) 


#decision tree with rpart
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
tree1 <-  rpart(NSP~LB+AC+FM, train)

library(rpart.plot)
rpart.plot(tree1,extra = 2) 
rpart.plot(tree1,extra = 3) 

#will show the probability of 1,2,3
predict(tree, validate) 

#misclassification  error of "training" data
tab <- table(predict(tree), train$NSP)
print(tab)
(1-sum(diag(tab))/sum(tab))

#prediction of validate data
testPred <- predict(tree, newdata= validate)
tab <- table(testPred,validate$NSP)
print(tab)
tab
(1-sum(diag(tab))/sum(tab))
