#https://youtu.be/-Vs9Vae2KI0?list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1

#data

getwd()
setwd("G:/R Programming/ML")
data <- read.csv("binary.csv")
str(data)

#Min-Max Normalization
hist(data$gre) #hist of gre main value
data$gre <- (data$gre- min(data$gre))/(max(data$gre)-min(data$gre))
hist(data$gre) #hist of gre value that lie 0-1

data$gpa <- (data$gpa- min(data$gpa))/(max(data$gpa)-min(data$gpa))
data$rank <- (data$rank- min(data$rank))/(max(data$rank)-min(data$rank))

#data partition
set.seed(222)
ind <- sample(2,nrow(data), replace = T, prob = c(0.7,3))
training <- data[ind==1,]
test <- data[ind==2,]


#Neural Network
install.packages("neuralnet")
library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre+gpa+rank, 
               data=training, 
               hidden=5,
               err.fct="ce", # cross entropy, #sse= Sum of squared error.
              linear.output=FALSE
               )

plot(n)

#prediction
output <- compute(n, training[,-1]) # y variable should be excluded.

head(output$net.result)
head(training) #now compare

# see the video for calculation


#confusion matrix & Misclassification error - Training data
output <- compute(n, training[,-1]) # y variable should be excluded.
p1 <- (output$net.result)
pred1 <- ifelse(p1>=0.5, 1,0)
tab1 <- table(pred1, training$admit)
tab1

sum(diag(tab1))/sum(tab1) # success rate
1-sum(diag(tab1))/sum(tab1) # 1- p means error rate



#confusion matrix & Misclassification error - Training data
output <- compute(n, test[,-1]) # y variable should be excluded.
p2 <- (output$net.result)
pred2 <- ifelse(p2>=0.5, 1,0)
tab2 <- table(pred2, test$admit)
tab2

sum(diag(tab2))/sum(tab2) # success rate
1-sum(diag(tab2))/sum(tab2) # 1- p means error rate
