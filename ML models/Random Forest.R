#read data
data <- read.csv("https://raw.githubusercontent.com/bkrai/Statistical-Modeling-and-Graphs-with-R/main/Cardiotocographic.csv")
data
head(data)
str(data)
data$NSP <- as.factor(data$NSP)
data$NSP



table(data$NSP)

#data partition

set.seed(123)

ind <- sample(2,nrow(data),replace=TRUE, prob = c(0.7,.3))
train <- data[ind==1,] # after comma means all the columns
test <- data[ind==2,] # after comma means all the columns

#Random Forest
#install.packages("randomForest")

library(randomForest)
set.seed(222)
rf <-  randomForest(NSP~.,data=train) # ~. means all the variables 
rf

# randomForest(formula = NSP ~ ., data = train) 
######## Means NSP model, ~. means all the variables, data= train
# Type of random forest: classification
# Number of trees: 500  <- ntree
#Means 500 trees default
# No. of variables tried at each split: 4 <- mtry 
# 4 is sqrt of total variables sqrt(21 variables) 
# OOB estimate of  error rate: 5.81%
# 95% accuracy 
# Confusion matrix:
# 1   2   3 class.error
# 1 1127  15   4  0.01657941
# 2   50 152   2  0.25490196
# 3    8   7 115  0.11538462

attributes(rf)

rf$type
rf$mtry


#prediction & confusion matrix - train data
#install.packages(caret)
library(caret)

p1 <- predict(rf,train)
confusionMatrix(p1, train$NSP)


#Reference
#Prediction    1    2    3
#          1 1146    2    0
#          2    0  202    0
#          3    0    0  130

# here the overall accuracy is (sum of diagonal member)/(total train data) 
# total 2 miss prediction 2 class


#prediction & confusion matrix - test data
#install.packages(caret)
#library(caret)

p2 <- predict(rf,test)
confusionMatrix(p2, test$NSP)


#error rate of random forest
plot(rf)

#the plot we can see the error drops down then after 300 trees the error is more 
# or less constant. 

#ture random forest model
t <- tuneRF(train[,-22], train[,22],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace=TRUE,
       improve = 0.05)


# After tuning the model the random forest should be call again
rf <-  randomForest(NSP~.,data=train,
                    ntree=300, # 300 because after 300 not improving
                    mtry= 8, # here 8 comes from the plot
                    importance=TRUE, 
                    proximity = TRUE
                    )
rf

# no. of nodes for the tree
hist(treesize(rf),
     main = "No. of nodes for the tree",
     col="green" )



#variable importance
varImpPlot(rf, sort = TRUE,
           n.var = 21,
           main = "Importance of Variable Predicting the Results")

importance(rf)
varUsed(rf)


# Partial Dependence Plot
partialPlot(rf, train, ASTV,"1")
#THIS plot represents how a single parameter ASTV is selecting NSP case 1 

partialPlot(rf, train, ASTV,"2")
#THIS plot represents how a single parameter ASTV is selecting NSP case 2 


partialPlot(rf, train, ASTV,"3")
#THIS plot represents how a single parameter ASTV is selecting NSP case 3 

getTree(rf, 1 , labelVar = TRUE)
#this function shows status -1 means this is the terminal variable and the node is predicted value. 


# Multi-dimentional Scaling plot for proximity matrix
MDSplot(rf, train$NSP)
#here red means case 1 and so on........