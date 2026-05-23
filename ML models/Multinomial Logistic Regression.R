# https://youtu.be/fDjKa7yWk1U?list=PL34t5iLfZddu8M0jd7pjSVUjvjBOBdYZ1


#Preparre data
data <- read.csv("https://raw.githubusercontent.com/bkrai/Statistical-Modeling-and-Graphs-with-R/main/Cardiotocographic.csv")
data
data$NSPF <- factor(data$NSP)
data$out <- relevel(data$NSPF, ref = "1")
str(data)

# Develop Multinomial Logistic Regression Model 
install.packages("nnet")
library(nnet)
mymodel <- multinom(out~LB+AC+FM , data = data)

summary(mymodel)


#see the video to see the calculation


#Predict with the model
data$predict <- predict(mymodel, data)

predict(mymodel, data[c(3,100,400), ], type = "prob") #Here 3, 100,400 means the 3rd, 100th 400th patients
str(data)


#misclassification data
tab1 <- table(predict(mymodel), data$NSPF)
tab1
summary(tab1)

prediction <- 1-sum(diag(tab1))/sum(tab1)



#2 tail z test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
z
p <- (1-pnorm(abs(z),0,1))*2
p
# (Intercept)   LB        AC      FM
#2   0.0000000 0.0000000  0 4.396984e-04
#3   0.7284205 0.4629596  0 2.025026e-09

# here p these values are less means the confidance level is high.